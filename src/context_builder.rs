use ahash::AHashMap;
use anyhow::Result;

use crate::{
    asset::{Asset, PublicFile},
    constants::Paths,
    content::{Content, Type},
    context::{Context, Metadata},
    utils::{find_files, is_file},
    Mode,
};

pub struct ContextBuilder {
    pub assets: AHashMap<String, Asset>,
    pub pages: AHashMap<String, Content>,
    pub public_files: Vec<PublicFile>,
}

impl ContextBuilder {
    pub fn new(paths: &Paths, mode: Mode) -> Result<Self> {
        let mut pages = collect_pages(paths)?;
        pages.append(&mut collect_posts(paths)?);

        let mut assets = AHashMap::new();
        assets.insert("styles.css".to_string(), Asset::build_css(paths, mode)?);
        collect_js(paths)?.into_iter().for_each(|a| {
            assets.insert(a.filename.clone(), a);
        });

        let public_files = collect_public_files(paths);
        let pages: AHashMap<_, _> = pages.into_iter().map(|p| (p.filename(), p)).collect();

        Ok(ContextBuilder {
            assets,
            pages,
            public_files,
        })
    }

    pub fn build(self, paths: &Paths, metadata: Metadata, mode: Mode) -> Context {
        Context::new(
            paths,
            metadata,
            self.assets,
            self.pages,
            self.public_files,
            mode,
        )
    }
}

fn collect_js(paths: &Paths) -> Result<Vec<Asset>> {
    find_files(&paths.js, is_file)
        .map(|f| Asset::from_path(&f))
        .collect()
}

pub fn collect_pages(paths: &Paths) -> Result<Vec<Content>> {
    find_files(&paths.pages, is_file)
        .map(|f| Content::from_path(&f, Type::Page))
        .collect()
}

pub fn collect_posts(paths: &Paths) -> Result<Vec<Content>> {
    find_files(&paths.posts, is_file)
        .map(|f| Content::from_path(&f, Type::Post))
        .collect()
}

fn collect_public_files(paths: &Paths) -> Vec<PublicFile> {
    find_files(&paths.public, is_file)
        .map(|f| PublicFile {
            path: f,
            prefix: paths.public.display().to_string(),
        })
        .collect()
}
