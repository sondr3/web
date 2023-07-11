use std::path::{Path, PathBuf};

use crate::Mode;
use anyhow::{Context, Result};
use jotdown::Render;
use minijinja::value::Value;
use minijinja::{context, path_loader, Environment};
use minijinja_autoreload::AutoReloader;
use once_cell::sync::Lazy;
use serde::Deserialize;
use time::Date;

use crate::utils::toml_date_deserializer;

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
pub enum ContentType {
    Page,
    Post,
}

#[derive(Debug)]
pub struct Content {
    pub out_path: PathBuf,
    pub url: String,
    pub content_type: ContentType,
    pub frontmatter: Frontmatter,
    pub content: String,
}

impl Content {
    pub fn from_path(path: &Path, kind: ContentType) -> Result<Content> {
        let file = std::fs::read_to_string(path)?;
        let stem = path.file_stem().unwrap().to_string_lossy();

        match file
            .split("+++")
            .map(|e| e.trim())
            .filter(|e| !e.is_empty())
            .collect::<Vec<_>>()[..]
        {
            [frontmatter, content] => Content::from_file(&stem, kind, frontmatter, Some(content)),
            [frontmatter] => Content::from_file(&stem, kind, frontmatter, None),
            _ => todo!(),
        }
    }

    pub fn render(&self, styles: &str, mode: Mode) -> Result<String> {
        let env = RELOADER.acquire_env()?;
        let template = env.get_template(&self.layout())?;
        let context = self.create(styles, mode)?;
        template
            .render(context)
            .context("Failed to render template")
    }

    fn from_file(
        stem: &str,
        kind: ContentType,
        frontmatter: &str,
        content: Option<&str>,
    ) -> Result<Self> {
        let frontmatter: Frontmatter = toml::from_str(frontmatter)?;

        let path: PathBuf = match &frontmatter.slug {
            Some(slug) => [slug, "index.html"].into_iter().collect(),
            None => [stem, "index.html"].into_iter().collect(),
        };

        let url = match &frontmatter.slug {
            Some(slug) => slug,
            None => stem,
        };

        Ok(Content {
            url: format!("{}/", url),
            out_path: path,
            content_type: kind,
            content: content.unwrap_or_default().into(),
            frontmatter,
        })
    }

    fn layout(&self) -> String {
        match (self.content_type, &self.frontmatter.layout) {
            (_, Some(layout)) => format!("{}.jinja", layout),
            (ContentType::Page, None) => "page.jinja".to_string(),
            (ContentType::Post, None) => "post.jinja".to_string(),
        }
    }

    fn content(&self) -> Result<String> {
        let events = jotdown::Parser::new(&self.content);
        let mut html = String::new();
        jotdown::html::Renderer::default().push(events, &mut html)?;
        Ok(html)
    }

    fn create(&self, styles: &str, mode: Mode) -> Result<Value> {
        let content = self.content()?;

        Ok(context! {
            title => self.frontmatter.title.clone(),
            subtitle => self.frontmatter.subtitle.clone(),
            description => self.frontmatter.description.clone(),
            development => mode.is_dev(),
            content => content,
            styles => styles,
        })
    }
}
