use crate::content::Content;
use crate::{
    asset::{Asset, PublicFile},
    context::Context,
    minify::{html_minifier_config, minify_html},
    sitemap::create_sitemap,
    utils::{copy_file, write_file},
    Mode,
};
use anyhow::Result;
use std::path::{Path, PathBuf};
use url::Url;

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
        context.pages.values(),
    )
}

pub fn write_pages_iter<'a, F>(
    dest: &Path,
    css: &str,
    mode: Mode,
    url: &Url,
    pages: F,
) -> Result<()>
where
    F: Iterator<Item = &'a Content>,
{
    let cfg = html_minifier_config();

    pages.into_iter().try_for_each(|f| {
        write_file(
            &dest.join(&f.out_path),
            if mode.is_prod() {
                minify_html(&f.render(css, mode, url)?, &cfg)
            } else {
                f.render(css, mode, url)?.into()
            },
        )
    })
}

pub fn write_sitemap(dest: &Path, context: &Context) -> Result<()> {
    let sitemap = create_sitemap(context)?;
    write_file(&dest.join("sitemap.xml"), sitemap)
}

pub fn copy_public_files(files: &[PublicFile], dest: &Path) -> Result<()> {
    files
        .iter()
        .try_for_each(|f| copy_file(dest, &f.prefix, &f.path))
}
