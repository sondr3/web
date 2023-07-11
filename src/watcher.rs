use crate::asset::{build_css, Asset};
use crate::builder::{build_pages, build_posts};
use crate::site::{write_css, write_pages};
use crate::{Mode, Options};
use anyhow::{Context, Result};
use notify::event::ModifyKind;
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::{Path, PathBuf};
use std::thread;
use url::Url;

#[derive(Debug)]
pub struct LiveReload {
    pub source: PathBuf,
    pub options: Options,
}

impl LiveReload {
    pub(crate) fn new(source: PathBuf, opts: Options) -> Self {
        LiveReload {
            source,
            options: opts,
        }
    }
}

pub fn start_live_reload(source: &Path) -> Result<()> {
    thread::scope(|scope| {
        let css = scope.spawn(|| {
            let styles = source.join("styles");
            file_watcher(&styles, &["scss"], |event| css_watch_handler(source, event))
        });

        let content = scope.spawn(|| {
            let content = source.join("content");
            file_watcher(&content, &["dj", "toml"], |event| {
                content_watch_handler(source, event)
            })
        });

        let templates = scope.spawn(|| {
            let content = source.join("templates");
            file_watcher(&content, &["jinja"], |event| {
                content_watch_handler(source, event)
            })
        });

        css.join().unwrap().unwrap();
        content.join().unwrap().unwrap();
        templates.join().unwrap().unwrap();
    });

    Ok(())
}

fn css_watch_handler(source: &Path, event: Event) -> Result<()> {
    println!(
        "File(s) {:?} changed, rebuilding CSS",
        strip_prefix_paths(source, &event.paths)?
    );
    let css = build_css(Path::new("./site/"), Mode::Dev)?;
    write_css(Path::new("./dist/"), &css)?;

    Ok(())
}

fn content_watch_handler(source: &Path, event: Event) -> Result<()> {
    println!(
        "File(s) {:?} changed, rebuilding site",
        strip_prefix_paths(source, &event.paths)?
    );
    let url = Url::parse("http://localhost:3000")?;

    let mut pages = build_pages(Path::new("./site/"))?;
    pages.append(&mut build_posts(Path::new("./site/"))?);
    let css = Asset {
        filename: PathBuf::from("styles.css"),
        content: "".to_string(),
    };
    write_pages(Path::new("./dist/"), &css, &pages, Mode::Dev, &url)?;

    Ok(())
}

fn strip_prefix_paths(prefix: impl AsRef<Path>, paths: &[PathBuf]) -> Result<Vec<&Path>> {
    paths
        .iter()
        .map(|p| {
            p.strip_prefix(prefix.as_ref())
                .context("could not strip prefix")
        })
        .collect()
}

fn file_watcher<F, const N: usize>(path: &Path, extensions: &[&str; N], handler: F) -> Result<()>
where
    F: Fn(Event) -> Result<()>,
{
    let (tx, rx) = std::sync::mpsc::channel();
    let mut watcher = RecommendedWatcher::new(tx, Config::default())?;
    watcher.watch(path, RecursiveMode::Recursive)?;

    for res in rx {
        if let Some(res) = filter_event(res, extensions) {
            handler(res)?;
        }
    }

    Ok(())
}

fn filter_event(res: notify::Result<Event>, extensions: &[&str]) -> Option<Event> {
    match res {
        Ok(event) => match event.kind {
            EventKind::Create(_)
            | EventKind::Modify(ModifyKind::Data(_) | ModifyKind::Name(_))
            | EventKind::Remove(_) => event_has_extension(event, extensions),
            _ => None,
        },
        Err(e) => {
            println!("watch error: {:?}", e);
            None
        }
    }
}

fn event_has_extension(event: Event, extensions: &[&str]) -> Option<Event> {
    if event
        .paths
        .iter()
        .any(|p| path_has_extension(p, extensions))
    {
        Some(event)
    } else {
        None
    }
}

fn path_has_extension(path: &Path, extensions: &[&str]) -> bool {
    path.extension()
        .map_or(false, |e| extensions.contains(&e.to_str().unwrap()))
}
