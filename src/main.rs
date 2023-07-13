mod asset;
mod compress;
mod constants;
mod content;
mod context;
mod context_builder;
mod minify;
mod render;
mod server;
mod sitemap;
mod utils;
mod watcher;

use crate::{
    constants::Paths, context::Metadata, context_builder::ContextBuilder, render::Renderer,
    watcher::start_live_reload,
};
use anyhow::Result;
use std::{sync::Arc, thread};
use tokio::sync::broadcast;

const HELP_MESSAGE: &str = r#"
web - website generator

Options:
  -s, --server      Disable dev server
  -p, --production  Optimize output
  -v, --verbose     Verbose output
  -h, --help        This message
  
Environment variables:
  CI,PROD           Optimize output
"#;

#[derive(Debug, Copy, Clone)]
pub enum Mode {
    Prod,
    Dev,
}

impl Mode {
    #[must_use]
    pub const fn is_prod(&self) -> bool {
        matches!(self, Self::Prod)
    }

    #[must_use]
    pub const fn is_dev(&self) -> bool {
        matches!(self, Self::Dev)
    }

    #[must_use]
    pub fn from_args(args: &[String]) -> Self {
        if std::env::var("CI").is_ok()
            || std::env::var("PROD").is_ok()
            || args.iter().any(|e| e == "-p" || e == "--production")
        {
            Mode::Prod
        } else {
            Mode::Dev
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Options {
    pub mode: Mode,
    pub verbose: bool,
    pub server: bool,
    pub help: bool,
}

impl Options {
    fn from_args() -> Self {
        let args: Vec<_> = std::env::args().skip(1).collect();

        Self {
            mode: Mode::from_args(&args),
            verbose: args.iter().any(|e| e == "-v" || e == "--verbose"),
            server: !args.iter().any(|e| e == "-s" || e == "--server"),
            help: args.iter().any(|e| e == "-h" || e == "--help"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Event {
    Reload,
    Shutdown,
}

pub struct AppState {
    tx: broadcast::Sender<Event>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let opts = Options::from_args();

    if opts.help {
        println!("{HELP_MESSAGE}");
        return Ok(());
    }

    println!("Running in {:?} mode...", opts.mode);

    let paths = Paths::new();

    let metadata = Metadata::new(opts.mode)?;
    let context = ContextBuilder::new(&paths, opts.mode)?.build(&paths, metadata, opts.mode);
    let renderer = Renderer::new(&paths.out);

    renderer.render_context(&context)?;

    if opts.mode.is_dev() && opts.server {
        let (tx, _rx) = broadcast::channel(100);
        let watcher = thread::spawn(move || start_live_reload(&paths));

        let state = Arc::new(AppState { tx });

        println!("Serving site at http://localhost:3000/...");
        server::create(state).await?;

        watcher.join().unwrap();
    } else if opts.mode.is_prod() {
        compress::folder(&paths.out)?;
    }

    Ok(())
}
