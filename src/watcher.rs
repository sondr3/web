use crate::asset::build_css;
use crate::site::write_css;
use crate::Mode;
use anyhow::Result;
use notify::event::ModifyKind;
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::Path;

pub fn file_watcher<F, const N: usize>(
    path: &Path,
    extensions: &[&str; N],
    handler: F,
) -> Result<()>
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

pub fn css_watch_handler(event: Event) -> Result<()> {
    println!("File {} changed, rebuilding CSS", event.paths[0].display());
    let css = build_css(Path::new("./site/"), Mode::Dev)?;
    write_css(Path::new("./dist/"), &css)?;

    Ok(())
}
