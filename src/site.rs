use crate::asset::{BuiltAssetFile, PublicFile};
use crate::content::Content;
use crate::utils::{copy_file, write_file};
use anyhow::Result;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Site {
    pub output: PathBuf,
    pub pages: Vec<Content>,
    pub public_files: Vec<PublicFile>,
    pub css: BuiltAssetFile,
}

impl Site {
    pub fn write(self) -> Result<()> {
        let dist = Path::new("dist");
        if dist.exists() {
            std::fs::remove_dir_all("./dist")?;
        }

        std::fs::create_dir("./dist")?;

        self.copy_public_files()?;
        self.write_css()?;
        self.write_pages()?;

        Ok(())
    }

    fn write_css(&self) -> Result<()> {
        write_file(&self.output.join(&self.css.filename), &self.css.content)
    }

    fn write_pages(&self) -> Result<()> {
        self.pages.iter().try_for_each(|f| {
            write_file(&self.output.join(&f.path), &f.render(&self.css.filename)?)
        })
    }

    fn copy_public_files(&self) -> Result<()> {
        self.public_files
            .iter()
            .try_for_each(|f| copy_file(&self.output, &f.prefix, &f.path))
    }
}
