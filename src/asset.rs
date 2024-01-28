use std::path::{Path, PathBuf};

use anyhow::Result;

use crate::{
    constants::Paths,
    minify,
    utils::{digest_filename, filename},
    Mode,
};

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

    pub fn build_css(out_file: &str, in_file: &str, paths: &Paths, mode: Mode) -> Result<Self> {
        let path = PathBuf::from(out_file);
        let source = paths.styles.join(in_file);
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
