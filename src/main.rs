use std::{path::Path, process::Command};

use askama::Template;

#[derive(Template)]
#[template(path = "index.jinja")]
struct IndexTemplate {
    title: String,
    description: String,
}

fn main() {
    let output = Command::new("pnpm")
        .arg("styles")
        .output()
        .expect("tailwind failed");
    println!("{}", String::from_utf8(output.stdout).unwrap());

    let index = IndexTemplate {
        title: "Home".to_string(),
        description: "Hello, I'm Sondre! I make things.".to_string(),
    };

    let dist = Path::new("dist");
    if !dist.exists() {
        std::fs::create_dir("./dist").unwrap();
    }

    std::fs::write("./dist/index.html", index.render().unwrap()).unwrap();
}
