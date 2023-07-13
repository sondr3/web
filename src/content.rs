use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use jotdown::{Attributes, Container, Event, Render};
use minijinja::{context, path_loader, value::Value, Environment};
use minijinja_autoreload::AutoReloader;
use once_cell::sync::Lazy;
use serde::Deserialize;
use time::Date;
use url::Url;

use crate::{utils::toml_date_deserializer, Mode};

static RELOADER: Lazy<AutoReloader> = Lazy::new(|| {
    AutoReloader::new(move |notifier| {
        let template_path = PathBuf::from("./site/templates");
        let mut env = Environment::new();
        env.set_loader(path_loader(&template_path));

        notifier.set_fast_reload(true);

        notifier.watch_path(&template_path, true);
        Ok(env)
    })
});

#[derive(Debug, Deserialize)]
pub struct Frontmatter {
    pub title: String,
    #[serde(with = "toml_date_deserializer")]
    pub last_modified: Date,
    pub subtitle: Option<String>,
    pub description: String,
    pub slug: Option<String>,
    pub layout: Option<String>,
}

#[derive(Debug, Copy, Clone)]
pub enum Type {
    Page,
    Post,
}

#[derive(Debug)]
pub struct Content {
    pub source: PathBuf,
    pub out_path: PathBuf,
    pub url: String,
    pub content_type: Type,
    pub frontmatter: Frontmatter,
    pub content: String,
}

impl Content {
    pub fn from_path(path: &Path, kind: Type) -> Result<Content> {
        let file = std::fs::read_to_string(path)?;
        let stem = path.file_stem().unwrap().to_string_lossy();

        match file
            .split("+++")
            .map(str::trim)
            .filter(|e| !e.is_empty())
            .collect::<Vec<_>>()[..]
        {
            [frontmatter, content] => {
                Content::from_file(path, &stem, kind, frontmatter, Some(content))
            }
            [frontmatter] => Content::from_file(path, &stem, kind, frontmatter, None),
            _ => todo!(),
        }
    }

    pub fn render(&self, styles: &str, mode: Mode, url: &Url) -> Result<String> {
        let env = RELOADER.acquire_env()?;
        let template = env.get_template(&self.layout())?;
        let context = self.create(styles, mode, url)?;
        template
            .render(context)
            .context("Failed to render template")
    }

    fn from_file(
        source: &Path,
        stem: &str,
        kind: Type,
        frontmatter: &str,
        content: Option<&str>,
    ) -> Result<Self> {
        let frontmatter: Frontmatter = toml::from_str(frontmatter)?;

        let path: PathBuf = match &frontmatter.slug {
            Some(slug) => [slug, "index.html"].into_iter().collect(),
            None => [stem, "index.html"].into_iter().collect(),
        };

        let url = frontmatter.slug.as_ref().map_or(stem, |s| s);

        Ok(Content {
            source: source.to_path_buf(),
            url: format!("{url}/"),
            out_path: path,
            content_type: kind,
            content: content.unwrap_or_default().into(),
            frontmatter,
        })
    }

    pub fn filename(&self) -> String {
        self.source.file_stem().map_or_else(
            || panic!("No filename found"),
            |name| name.to_string_lossy().to_string(),
        )
    }

    fn layout(&self) -> String {
        match (self.content_type, &self.frontmatter.layout) {
            (_, Some(layout)) => format!("{layout}.jinja"),
            (Type::Page, None) => "page.jinja".to_string(),
            (Type::Post, None) => "post.jinja".to_string(),
        }
    }

    fn content(&self) -> Result<String> {
        let events = jotdown::Parser::new(&self.content).map(jotdown_event_mapper);
        let mut html = String::new();
        jotdown::html::Renderer::default().push(events, &mut html)?;
        Ok(html)
    }

    fn create(&self, styles: &str, mode: Mode, url: &Url) -> Result<Value> {
        let content = self.content()?;

        Ok(context! {
            title => self.frontmatter.title.clone(),
            subtitle => self.frontmatter.subtitle.clone(),
            description => self.frontmatter.description.clone(),
            is_dev => mode.is_dev(),
            canonical_url => url.join(&self.url)?,
            content => content,
            styles => styles,
        })
    }
}

fn jotdown_event_mapper(event: jotdown::Event) -> jotdown::Event {
    match event {
        Event::Start(container, attrs) => jotdown_container_mapper(container, attrs).into(),
        _ => event,
    }
}

struct ContainerWrapper<'a>(Container<'a>, Attributes<'a>);

impl<'a> From<ContainerWrapper<'a>> for jotdown::Event<'a> {
    fn from(val: ContainerWrapper<'a>) -> Self {
        Event::Start(val.0, val.1)
    }
}

fn jotdown_container_mapper<'a>(
    container: Container<'a>,
    attrs: Attributes<'a>,
) -> ContainerWrapper<'a> {
    match container {
        Container::Heading {
            id,
            level,
            has_section,
        } => ContainerWrapper(
            Container::Heading {
                level,
                id: id.to_lowercase().into(),
                has_section,
            },
            attrs,
        ),
        Container::Section { id } => ContainerWrapper(
            Container::Section {
                id: id.to_lowercase().into(),
            },
            attrs,
        ),
        _ => ContainerWrapper(container, attrs),
    }
}
