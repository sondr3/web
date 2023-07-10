use crate::minify::minify_css;
use crate::utils::AppendExtension;
use crate::Mode;
use anyhow::Result;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct PublicFile {
    pub path: PathBuf,
    pub prefix: String,
}

#[derive(Debug)]
pub struct Asset {
    pub filename: PathBuf,
    pub content: String,
}

pub fn build_css(root: &Path, mode: Mode) -> Result<Asset> {
    let filename = PathBuf::from("styles.css");
    let content = grass::from_path(root.join("styles/styles.scss"), &grass::Options::default())?;

    Ok(match mode {
        Mode::Prod => Asset {
            filename: digest_filename(&filename, &content),
            content: minify_css(&content.clone())?,
        },
        Mode::Dev => Asset { filename, content },
    })
}

pub fn digest_filename(filename: &Path, content: &str) -> PathBuf {
    let digest = format!("{:x}", md5::compute(content));
    let hash = digest.split_at(8).0;
    let Some(extension) = filename.extension() else {
        panic!("No extension found for {:?}", filename);
    };

    PathBuf::from(filename)
        .with_extension(hash)
        .append_extension(extension)
}
