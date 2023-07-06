use std::{
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
};

use minijinja::{context, path_loader, Environment};
use once_cell::sync::Lazy;

const HELP_MESSAGE: &str = r#"
web - website generator

Options:
  -p, --production  Optimize output
  -v, --verbose     Verbose output
  -h, --help        This message
"#;

#[derive(Debug)]
struct Options {
    production: bool,
    verbose: bool,
    help: bool,
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

static ENV: Lazy<Environment<'static>> = Lazy::new(|| {
    let mut env = Environment::new();
    env.set_loader(path_loader("templates"));
    env
});

trait AppendExtension {
    fn append_extension(&self, ext: impl AsRef<OsStr>) -> PathBuf;
}

impl AppendExtension for PathBuf {
    fn append_extension(&self, ext: impl AsRef<OsStr>) -> PathBuf {
        let mut os_str: OsString = self.into();
        os_str.push(".");
        os_str.push(ext.as_ref());
        os_str.into()
    }
}

#[derive(Debug)]
struct AssetFile {
    filename: String,
    extension: String,
    content: String,
}

impl AssetFile {
    fn final_filename(&self, production: bool) -> String {
        if production {
            self.hash()
        } else {
            format!("{}.{}", self.filename, self.extension)
        }
    }

    fn hash(&self) -> String {
        let digest = format!("{:x}", md5::compute(&self.content));
        let hash = digest.split_at(8).0;

        Path::new(&self.filename)
            .to_path_buf()
            .append_extension(hash)
            .append_extension(&self.extension)
            .display()
            .to_string()
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts = dbg!(Options::from_args());
    let _verbose = opts.verbose;

    if opts.help {
        println!("{}", HELP_MESSAGE);
        return Ok(());
    }

    let styles = AssetFile {
        filename: "styles".to_string(),
        extension: "css".to_string(),
        content: grass::from_path("./src/styles/styles.scss", &grass::Options::default())?,
    };

    let index = ENV.get_template("index.jinja")?;
    let styles_filename = styles.final_filename(opts.production);
    let context = context!(title => "Home", description => "Hello, I'm Sondre! I make things.", styles => styles_filename);

    let dist = Path::new("dist");
    if dist.exists() {
        std::fs::remove_dir_all("./dist")?;
    }

    std::fs::create_dir("./dist")?;
    std::fs::write(format!("./dist/{}", styles_filename), styles.content)?;
    std::fs::write("./dist/index.html", index.render(context)?)?;

    Ok(())
}
