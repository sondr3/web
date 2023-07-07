use crate::asset::{build_css, PublicFile};
use crate::site::Site;
use crate::utils::{find_files, is_file};
use crate::{Mode, Options};

use crate::content::{Content, ContentType};
use anyhow::Result;
use std::path::{Path, PathBuf};
use url::Url;

#[derive(Debug)]
pub struct Builder {
    pub source: PathBuf,
    pub options: Options,
}

impl Builder {
    pub fn new(source: PathBuf, opts: Options) -> Self {
        Self {
            source,
            options: opts,
        }
    }

    pub fn build(self) -> Result<Site> {
        let mut pages = build_pages(&self.source)?;
        pages.append(&mut build_posts(&self.source)?);

        Ok(Site {
            url: match self.options.mode {
                Mode::Prod => Url::parse("https://www.eons.io")?,
                Mode::Dev => Url::parse("http://localhost:3000")?,
            },
            out_path: PathBuf::from("./dist"),
            pages,
            public_files: find_public_files(&self.source),
            css: build_css(&self.source, self.options.mode)?,
        })
    }
}

pub fn build_pages(source: &Path) -> Result<Vec<Content>> {
    find_files(&source.join("content/pages"), is_file)
        .map(|f| Content::from_path(&f, ContentType::Page))
        .collect()
}

pub fn build_posts(source: &Path) -> Result<Vec<Content>> {
    find_files(&source.join("content/posts"), is_file)
        .map(|f| Content::from_path(&f, ContentType::Post))
        .collect()
}

pub fn find_public_files(source: &Path) -> Vec<PublicFile> {
    let public_dir = source.join("public");
    find_files(&public_dir, is_file)
        .map(|f| PublicFile {
            path: f,
            prefix: public_dir.display().to_string(),
        })
        .collect()
}
