use crate::asset::{Asset, PublicFile};
use crate::content::Content;
use crate::minify::{html_minifier_config, minify_html};
use crate::sitemap::create_sitemap;
use crate::utils::{copy_file, write_file};
use crate::{compress, Mode};
use anyhow::Result;
use std::path::{Path, PathBuf};
use url::Url;

#[derive(Debug)]
pub struct Site {
    pub url: Url,
    pub out_path: PathBuf,
    pub pages: Vec<Content>,
    pub public_files: Vec<PublicFile>,
    pub css: Asset,
}

pub fn write_site(site: Site, mode: Mode) -> Result<()> {
    let dist = Path::new("dist");
    if dist.exists() {
        std::fs::remove_dir_all("./dist")?;
    }

    std::fs::create_dir("./dist")?;

    copy_public_files(&site.public_files, &site.out_path)?;
    write_css(&site.out_path, &site.css)?;
    write_pages(&site.out_path, &site.css, &site.pages, mode)?;

    write_sitemap(&site.pages, &site.url, &site.out_path)?;

    if mode.is_prod() {
        compress_folder(&site.out_path)?;
    }

    Ok(())
}

fn compress_folder(folder: &Path) -> Result<()> {
    compress::gzip(folder)?;
    compress::brotli(folder)?;

    Ok(())
}

fn write_css(dest: &Path, asset: &Asset) -> Result<()> {
    write_file(&dest.join(&asset.filename), &asset.content)
}

fn write_pages(dest: &Path, css: &Asset, pages: &[Content], mode: Mode) -> Result<()> {
    let cfg = html_minifier_config();
    let css = css.filename.display().to_string();

    pages.iter().try_for_each(|f| {
        write_file(
            &dest.join(&f.out_path),
            if mode.is_prod() {
                minify_html(&f.render(&css)?, &cfg)
            } else {
                f.render(&css)?.into()
            },
        )
    })
}

fn write_sitemap(pages: &[Content], url: &Url, dest: &Path) -> Result<()> {
    let sitemap = create_sitemap(pages, url)?;
    write_file(&dest.join("sitemap.xml"), sitemap)
}

fn copy_public_files(files: &[PublicFile], dest: &Path) -> Result<()> {
    files
        .iter()
        .try_for_each(|f| copy_file(dest, &f.prefix, &f.path))
}
