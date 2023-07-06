use std::path::Path;

use minijinja::{context, path_loader, Environment};
use once_cell::sync::Lazy;

static ENV: Lazy<Environment<'static>> = Lazy::new(|| {
    let mut env = Environment::new();
    env.set_loader(path_loader("templates"));
    env
});

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let css = grass::from_path("./src/styles/styles.scss", &grass::Options::default())?;
    let digest = format!("{:x}", md5::compute(css.as_bytes()));
    let _hash = digest.split_at(8).0;

    let index = ENV.get_template("index.jinja")?;
    let context = context!(title => "Home", description => "Hello, I'm Sondre! I make things.");

    let dist = Path::new("dist");
    if !dist.exists() {
        std::fs::create_dir("./dist")?;
    }

    std::fs::write("./dist/styles.css", css)?;
    std::fs::write("./dist/index.html", index.render(context)?)?;

    Ok(())
}
