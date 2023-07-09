use crate::asset::build_css;
use crate::site::write_css;
use crate::Mode;
use anyhow::Result;
use notify::event::ModifyKind;
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::Path;

pub fn file_watcher<F>(path: &Path, _mode: &Mode, handler: F) -> Result<()>
where
    F: Fn(Event) -> Result<()>,
{
    let (tx, rx) = std::sync::mpsc::channel();
    let mut watcher = RecommendedWatcher::new(tx, Config::default())?;
    watcher.watch(path, RecursiveMode::Recursive)?;

    for res in rx {
        if let Some(res) = filter_event(res) {
            handler(res)?;
        }
    }

    Ok(())
}

fn filter_event(res: notify::Result<Event>) -> Option<Event> {
    match res {
        Ok(event) => match event.kind {
            EventKind::Create(_)
            | EventKind::Modify(ModifyKind::Data(_) | ModifyKind::Name(_))
            | EventKind::Remove(_) => {
                if event
                    .paths
                    .iter()
                    .any(|p| p.extension() == Some("scss".as_ref()))
                {
                    Some(event)
                } else {
                    None
                }
            }
            _ => None,
        },
        Err(e) => {
            println!("watch error: {:?}", e);
            None
        }
    }
}

pub fn css_watch_handler(event: Event) -> Result<()> {
    println!("Rebuilding CSS: {:?}", event);
    let css = build_css(Path::new("./site/"), Mode::Dev)?;
    write_css(Path::new("./dist/"), &css)?;

    Ok(())
}
