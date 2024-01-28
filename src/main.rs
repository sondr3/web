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

use std::{fmt::Display, thread, time::Instant};

use anyhow::Result;
use time::UtcOffset;
use tokio::sync::broadcast;
use tracing_subscriber::{
    fmt::time::OffsetTime, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter,
};

use crate::{
    constants::Paths, context::Metadata, context_builder::ContextBuilder, render::Renderer,
    watcher::start_live_reload,
};

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

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Prod => write!(f, "prod"),
            Mode::Dev => write!(f, "dev"),
        }
    }
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
    pub server: bool,
    pub mode: Mode,
    pub verbose: bool,
    pub help: bool,
}

impl Options {
    fn from_args() -> Self {
        let args: Vec<_> = std::env::args().skip(1).collect();

        Self {
            server: !args.iter().any(|e| e == "-s" || e == "--server"),
            mode: Mode::from_args(&args),
            verbose: args.iter().any(|e| e == "-v" || e == "--verbose"),
            help: args.iter().any(|e| e == "-h" || e == "--help"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Event {
    Reload,
    Shutdown,
}

#[tokio::main]
async fn main() -> Result<()> {
    let opts = Options::from_args();

    let offset = UtcOffset::current_local_offset().map_or(UtcOffset::UTC, |o| o);
    let format = time::format_description::parse("[hour]:[minute]:[second]")?;
    let timer = OffsetTime::new(offset, format);
    let fmt = tracing_subscriber::fmt::layer()
        .with_target(false)
        .with_timer(timer);
    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| format!("web={}", if opts.verbose { "debug" } else { "info" }).into());

    tracing_subscriber::registry().with(filter).with(fmt).init();

    if opts.help {
        println!("{HELP_MESSAGE}");
        return Ok(());
    }

    tracing::info!("Running in {} mode...", opts.mode);

    let now = Instant::now();

    let paths = Paths::new();
    let metadata = Metadata::new(opts.mode)?;
    let context = ContextBuilder::new(&paths, opts.mode)?.build(&paths, metadata, opts.mode);
    let renderer = Renderer::new(&paths.out);

    renderer.render_context(&context)?;

    let done = now.elapsed();
    tracing::info!(
        "Built {} pages in {:?}ms",
        context.pages.len(),
        done.as_millis()
    );

    if opts.mode.is_dev() && opts.server {
        let (tx, _rx) = broadcast::channel(100);
        let root = paths.out.clone();
        let watcher_tx = tx.clone();
        let watcher = thread::spawn(move || start_live_reload(&paths, &context, &watcher_tx));

        tracing::info!("Serving site at http://localhost:3000/...");
        server::create(&root, tx).await?;

        watcher.join().unwrap();
    } else if opts.mode.is_prod() {
        let now = Instant::now();

        compress::folder(&paths.out)?;

        let done = now.elapsed();
        tracing::info!("Finished compressing output in {:?}ms", done.as_millis());
    }

    Ok(())
}
