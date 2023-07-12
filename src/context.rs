use crate::{
    asset::{Asset, PublicFile},
    content::Content,
};
use ahash::AHashMap;
use minijinja::Environment;
use std::path::PathBuf;

pub struct Context<'a> {
    assets: AHashMap<PathBuf, Asset>,
    pages: AHashMap<PathBuf, Content>,
    templates: Environment<'a>,
    public_files: Vec<PublicFile>,
}

impl<'a> Context<'a> {
    pub fn new(
        assets: AHashMap<PathBuf, Asset>,
        pages: AHashMap<PathBuf, Content>,
        templates: Environment<'a>,
        public_files: Vec<PublicFile>,
    ) -> Self {
        Self {
            assets,
            pages,
            templates,
            public_files,
        }
    }
}
