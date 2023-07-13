use std::{
    path::{Path, PathBuf},
    thread,
};

use anyhow::{Context, Result};
use notify::{
    event::ModifyKind, Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher,
};
use tokio::sync::broadcast::Sender;
use url::Url;

use crate::{
    asset::Asset,
    constants::Paths,
    context::Context as AppContext,
    context_builder::{collect_pages, collect_posts},
    render::{write_asset, write_pages_iter},
    Mode,
};

pub fn start_live_reload(paths: &Paths, context: AppContext, tx: &Sender<crate::Event>) {
    thread::scope(|scope| {
        let css = scope.spawn(|| {
            file_watcher(&paths.styles.canonicalize()?, &["scss"], |event| {
                css_watch_handler(paths, &event, tx)
            })
        });

        let content = scope.spawn(|| {
            file_watcher(&paths.content.canonicalize()?, &["dj", "toml"], |event| {
                content_watch_handler(paths, &event, &context, tx)
            })
        });

        let templates = scope.spawn(|| {
            file_watcher(&paths.templates.canonicalize()?, &["jinja"], |event| {
                content_watch_handler(paths, &event, &context, tx)
            })
        });

        css.join().unwrap().unwrap();
        content.join().unwrap().unwrap();
        templates.join().unwrap().unwrap();
    });
}

fn css_watch_handler(paths: &Paths, event: &Event, tx: &Sender<crate::Event>) -> Result<()> {
    tracing::info!(
        "File(s) {:?} changed, rebuilding CSS",
        strip_prefix_paths(&paths.source, &event.paths)?
    );
    let css = Asset::build_css(paths, Mode::Dev)?;
    write_asset(&paths.out, &css)?;
    tx.send(crate::Event::Reload)?;

    Ok(())
}

fn content_watch_handler(
    paths: &Paths,
    event: &Event,
    context: &AppContext,
    tx: &Sender<crate::Event>,
) -> Result<()> {
    tracing::info!(
        "File(s) {:?} changed, rebuilding site",
        strip_prefix_paths(&paths.source, &event.paths)?
    );
    let url = Url::parse("http://localhost:3000")?;
    let mut pages = collect_pages(paths)?;
    pages.append(&mut collect_posts(paths)?);
    write_pages_iter(
        &paths.out,
        "styles.css",
        Mode::Dev,
        &url,
        &context.templates,
        pages.iter(),
    )?;
    tx.send(crate::Event::Reload)?;

    Ok(())
}

fn strip_prefix_paths(prefix: impl AsRef<Path>, paths: &[PathBuf]) -> Result<Vec<&Path>> {
    paths
        .iter()
        .map(|p| {
            p.strip_prefix(prefix.as_ref().canonicalize()?)
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
            tracing::error!("watch error: {e:?}");
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
