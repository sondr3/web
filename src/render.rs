use std::path::{Path, PathBuf};

use anyhow::Result;
use minijinja_autoreload::AutoReloader;
use url::Url;

use crate::{
    asset::{Asset, PublicFile},
    content::Content,
    context::Context,
    sitemap,
    utils::{copy_file, write_file},
    Mode,
};

pub struct Renderer {
    pub dest: PathBuf,
}

impl Renderer {
    pub fn new(dest: &Path) -> Self {
        Renderer {
            dest: dest.to_path_buf(),
        }
    }

    pub fn render_context(&self, context: &Context) -> Result<()> {
        self.create_dest()?;

        copy_public_files(&context.public_files, &self.dest)?;
        context
            .assets
            .values()
            .try_for_each(|a| write_asset(&self.dest, a))?;

        write_pages(&self.dest, context)?;
        write_sitemap(&self.dest, context)?;

        Ok(())
    }

    fn create_dest(&self) -> Result<()> {
        if self.dest.exists() {
            std::fs::remove_dir_all(&self.dest)?;
        }

        std::fs::create_dir(&self.dest)?;

        Ok(())
    }
}

pub fn write_asset(dest: &Path, asset: &Asset) -> Result<()> {
    write_file(&dest.join(&asset.filename), &asset.content)
}

pub fn write_pages(dest: &Path, context: &Context) -> Result<()> {
    let css = context.assets.get("styles.css").unwrap();
    let css = &css.filename;

    write_pages_iter(
        dest,
        css,
        context.mode,
        &context.metadata.url,
        &context.templates,
        context.pages.values(),
    )
}

pub fn write_pages_iter<'a, F>(
    dest: &Path,
    css: &str,
    mode: Mode,
    url: &Url,
    templates: &AutoReloader,
    pages: F,
) -> Result<()>
where
    F: Iterator<Item = &'a Content>,
{
    pages.into_iter().try_for_each(|f| {
        write_file(
            &dest.join(&f.out_path),
            f.render(css, mode, url, templates)?,
        )
    })
}

pub fn write_sitemap(dest: &Path, context: &Context) -> Result<()> {
    let sitemap = sitemap::create(context)?;
    write_file(&dest.join("sitemap.xml"), sitemap)
}

pub fn copy_public_files(files: &[PublicFile], dest: &Path) -> Result<()> {
    files
        .iter()
        .try_for_each(|f| copy_file(dest, &f.prefix, &f.path))
}
