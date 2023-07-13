use crate::{
    constants::Paths,
    minify,
    utils::{digest_filename, filename},
    Mode,
};
use anyhow::Result;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct PublicFile {
    pub path: PathBuf,
    pub prefix: String,
}

#[derive(Debug)]
pub struct Asset {
    pub filename: String,
    pub content: String,
}

impl Asset {
    pub fn from_path(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let filename = filename(path);

        Ok(Self { filename, content })
    }

    pub fn build_css(paths: &Paths, mode: Mode) -> Result<Self> {
        let path = PathBuf::from("styles.css");
        let source = paths.styles.join("styles.scss");
        let content = grass::from_path(source, &grass::Options::default())?;

        Ok(match mode {
            Mode::Prod => Self {
                filename: digest_filename(&path, &content),
                content: minify::css(&content.clone())?,
            },
            Mode::Dev => Self {
                filename: filename(path),
                content,
            },
        })
    }
}
