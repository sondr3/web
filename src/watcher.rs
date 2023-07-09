use crate::asset::build_css;
use crate::site::write_css;
use crate::Mode;
use anyhow::Result;
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::Path;

pub fn file_watcher<F>(path: &Path, _mode: &Mode, handler: F) -> Result<()>
where
    F: Fn(notify::Result<Event>) -> Result<()>,
{
    let (tx, rx) = std::sync::mpsc::channel();
    let mut watcher = RecommendedWatcher::new(tx, Config::default())?;
    watcher.watch(path, RecursiveMode::Recursive)?;

    for res in rx {
        handler(res)?;
    }

    Ok(())
}

pub fn css_watch_handler(res: notify::Result<Event>) -> Result<()> {
    match res {
        Ok(event) => {
            println!("Rebuilding CSS: {:?}", event);
            let css = build_css(Path::new("./site/"), Mode::Dev)?;
            write_css(Path::new("./dist/"), &css)?;
        }
        Err(e) => println!("watch error: {:?}", e),
    }

    Ok(())
}
