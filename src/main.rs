mod asset;
mod builder;
mod compress;
mod content;
mod minify;
mod site;
mod sitemap;
mod utils;

use crate::builder::Builder;
use anyhow::Result;

const HELP_MESSAGE: &str = r#"
web - website generator

Options:
  -p, --production  Optimize output
  -v, --verbose     Verbose output
  -h, --help        This message
  
Environment variables:
  ci,prod           Optimize output
"#;

#[derive(Debug, Copy, Clone)]
pub struct Options {
    pub production: bool,
    pub verbose: bool,
    pub help: bool,
}

impl Options {
    fn from_args() -> Self {
        let args: Vec<_> = std::env::args().skip(1).collect();

        Options {
            production: Options::set_is_prod(&args),
            verbose: args.iter().any(|e| e == "-v" || e == "--verbose"),
            help: args.iter().any(|e| e == "-h" || e == "--help"),
        }
    }

    fn set_is_prod(args: &[String]) -> bool {
        std::env::var("CI").is_ok()
            || std::env::var("PROD").is_ok()
            || args.iter().any(|e| e == "-p" || e == "--production")
    }
}

fn main() -> Result<()> {
    let opts = Options::from_args();
    let _verbose = opts.verbose;

    if opts.help {
        println!("{}", HELP_MESSAGE);
        return Ok(());
    }

    println!(
        "Running in {} mode...",
        if opts.production { "prod" } else { "dev" }
    );

    let builder = Builder::new(opts);
    let site = builder.build()?;

    site.write(opts.production)?;

    Ok(())
}
