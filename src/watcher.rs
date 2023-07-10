use crate::asset::build_css;
use crate::site::write_css;
use crate::{Mode, Options};
use anyhow::{Context, Result};
use notify::event::ModifyKind;
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::{Path, PathBuf};
use std::thread;

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

    pub fn start(self) -> Result<()> {
        let css = thread::spawn(move || {
            file_watcher(&self.source.join("styles"), &["scss"], |event| {
                self.css_watch_handler(event)
            })
        });

        css.join().unwrap()?;

        Ok(())
    }

    fn css_watch_handler(&self, event: Event) -> Result<()> {
        println!(
            "File(s) {:?} changed, rebuilding CSS",
            strip_prefix_paths(&self.source, &event.paths)?
        );
        let css = build_css(Path::new("./site/"), Mode::Dev)?;
        write_css(Path::new("./dist/"), &css)?;

        Ok(())
    }
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
