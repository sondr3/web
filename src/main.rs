mod asset;
mod builder;
mod content;
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
            production: args.iter().any(|e| e == "-p" || e == "--production"),
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

    let builder = Builder::new(opts);
    let site = builder.build()?;

    site.write()?;

    Ok(())
}
