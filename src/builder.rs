use crate::asset::{AssetFile, BuiltAssetFile, PublicFile};
use crate::site::Site;
use crate::utils::{find_files, is_file};
use crate::Options;

use crate::content::{Content, ContentType};
use anyhow::Result;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Builder {
    root: PathBuf,
    options: Options,
}

impl Builder {
    pub fn new(opts: Options) -> Self {
        Self {
            root: Path::new("./site").to_owned(),
            options: opts,
        }
    }

    pub fn build(self) -> Result<Site> {
        let mut pages = self.build_pages()?;
        pages.append(&mut self.build_posts()?);

        Ok(Site {
            output: PathBuf::from("./dist"),
            pages,
            public_files: self.find_public_files(),
            css: self.compile_css()?,
        })
    }

    fn build_pages(&self) -> Result<Vec<Content>> {
        find_files(&self.root.join("content/pages"), is_file)
            .map(|f| Content::from_path(&f, ContentType::Page))
            .collect()
    }

    fn build_posts(&self) -> Result<Vec<Content>> {
        find_files(&self.root.join("content/posts"), is_file)
            .map(|f| Content::from_path(&f, ContentType::Post))
            .collect()
    }

    fn find_public_files(&self) -> Vec<PublicFile> {
        let public_dir = self.root.join("public");
        find_files(&public_dir, is_file)
            .map(|f| PublicFile {
                path: f,
                prefix: public_dir.to_string_lossy().to_string(),
            })
            .collect()
    }

    fn compile_css(&self) -> Result<BuiltAssetFile> {
        let asset = AssetFile {
            filename: "styles".to_string(),
            extension: "css".to_string(),
            content: grass::from_path(
                self.root.join("styles/styles.scss"),
                &grass::Options::default(),
            )?,
        };

        Ok(BuiltAssetFile::from_asset(&asset, self.options.production))
    }
}
