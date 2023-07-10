mod asset;
mod builder;
mod compress;
mod content;
mod minify;
mod site;
mod sitemap;
mod utils;
mod watcher;

use crate::builder::Builder;
use crate::site::write_site;
use crate::watcher::{start_live_reload, LiveReload};
use anyhow::Result;
use std::path::Path;

const HELP_MESSAGE: &str = r#"
web - website generator

Options:
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
    pub fn is_prod(&self) -> bool {
        matches!(self, Self::Prod)
    }

    pub fn is_dev(&self) -> bool {
        matches!(self, Self::Dev)
    }

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
    pub help: bool,
}

impl Options {
    fn from_args() -> Self {
        let args: Vec<_> = std::env::args().skip(1).collect();

        Options {
            mode: Mode::from_args(&args),
            verbose: args.iter().any(|e| e == "-v" || e == "--verbose"),
            help: args.iter().any(|e| e == "-h" || e == "--help"),
        }
    }
}

fn main() -> Result<()> {
    let opts = Options::from_args();
    let _verbose = opts.verbose;

    if opts.help {
        println!("{}", HELP_MESSAGE);
        return Ok(());
    }

    println!("Running in {:?} mode...", opts.mode);

    let source = Path::new("./site").to_owned().canonicalize()?;
    let builder = Builder::new(source.clone(), opts);
    let site = builder.build()?;

    write_site(site, opts.mode)?;

    if opts.mode.is_dev() {
        let watcher = LiveReload::new(source, opts);
        start_live_reload(&watcher.source)?;
    }

    Ok(())
}
