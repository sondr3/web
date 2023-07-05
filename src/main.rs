use std::{path::Path, process::Command};

use minijinja::{context, path_loader, Environment};
use once_cell::sync::Lazy;

static ENV: Lazy<Environment<'static>> = Lazy::new(|| {
    let mut env = Environment::new();
    env.set_loader(path_loader("templates"));
    env
});

fn main() {
    let output = Command::new("pnpm")
        .arg("styles")
        .output()
        .expect("tailwind failed");
    println!("{}", String::from_utf8(output.stdout).unwrap());

    let index = ENV.get_template("index.jinja").unwrap();
    let context = context!(title => "Home", description => "Hello, I'm Sondre! I make things.");

    let dist = Path::new("dist");
    if !dist.exists() {
        std::fs::create_dir("./dist").unwrap();
    }

    std::fs::write("./dist/index.html", index.render(context).unwrap()).unwrap();
}
