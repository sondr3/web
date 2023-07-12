use crate::{
    asset::{Asset, PublicFile},
    constants::*,
    content::Content,
    Mode,
};
use ahash::AHashMap;
use anyhow::Result;
use minijinja::{path_loader, Environment};
use minijinja_autoreload::AutoReloader;
use std::path::PathBuf;
use url::Url;

#[derive(Debug)]
pub struct Metadata {
    pub url: Url,
    pub out: PathBuf,
}

impl Metadata {
    pub fn new(mode: Mode) -> Result<Self> {
        Ok(Self {
            url: match mode {
                Mode::Prod => Url::parse("https://www.eons.io")?,
                Mode::Dev => Url::parse("http://localhost:3000")?,
            },
            out: PathBuf::from(OUT_PATH),
        })
    }
}

pub struct Context {
    pub metadata: Metadata,
    pub assets: AHashMap<String, Asset>,
    pub pages: AHashMap<String, Content>,
    pub templates: AutoReloader,
    pub public_files: Vec<PublicFile>,
    pub mode: Mode,
}

impl Context {
    pub fn new(
        paths: &Paths,
        metadata: Metadata,
        assets: AHashMap<String, Asset>,
        pages: AHashMap<String, Content>,
        public_files: Vec<PublicFile>,
        mode: Mode,
    ) -> Self {
        let template_path = paths.templates.clone();
        let env = AutoReloader::new(move |notifier| {
            let mut env = Environment::new();
            env.set_loader(path_loader(&template_path));

            notifier.set_fast_reload(true);

            notifier.watch_path(&template_path, true);
            Ok(env)
        });

        Self {
            metadata,
            assets,
            pages,
            templates: env,
            public_files,
            mode,
        }
    }

    pub fn _update_asset(&mut self, path: impl Into<String>, asset: Asset) {
        self.assets.insert(path.into(), asset);
    }

    pub fn _update_page(&mut self, path: impl Into<String>, page: Content) {
        self.pages.insert(path.into(), page);
    }
}
