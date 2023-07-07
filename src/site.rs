use crate::asset::{BuiltAssetFile, PublicFile};
use crate::compress;
use crate::content::Content;
use crate::sitemap::create_sitemap;
use crate::utils::{copy_file, write_file};
use anyhow::Result;
use std::path::{Path, PathBuf};
use url::Url;

#[derive(Debug)]
pub struct Site {
    pub url: Url,
    pub output: PathBuf,
    pub pages: Vec<Content>,
    pub public_files: Vec<PublicFile>,
    pub css: BuiltAssetFile,
}

impl Site {
    pub fn write(self, production: bool) -> Result<()> {
        let dist = Path::new("dist");
        if dist.exists() {
            std::fs::remove_dir_all("./dist")?;
        }

        std::fs::create_dir("./dist")?;

        self.copy_public_files()?;
        self.write_css()?;
        self.write_pages()?;

        self.write_sitemap()?;

        if production {
            self.compress()?;
        }

        Ok(())
    }

    fn compress(&self) -> Result<()> {
        compress::gzip(&self.output)?;
        compress::brotli(&self.output)?;

        Ok(())
    }

    fn write_css(&self) -> Result<()> {
        write_file(&self.output.join(&self.css.filename), &self.css.content)
    }

    fn write_pages(&self) -> Result<()> {
        self.pages.iter().try_for_each(|f| {
            write_file(
                &self.output.join(&f.out_path),
                &f.render(&self.css.filename)?,
            )
        })
    }

    fn write_sitemap(&self) -> Result<()> {
        let sitemap = create_sitemap(&self.pages, &self.url)?;
        write_file(&self.output.join("sitemap.xml"), &sitemap)
    }

    fn copy_public_files(&self) -> Result<()> {
        self.public_files
            .iter()
            .try_for_each(|f| copy_file(&self.output, &f.prefix, &f.path))
    }
}
