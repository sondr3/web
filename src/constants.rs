use std::path::PathBuf;

pub const OUT_PATH: &str = "./dist";
pub const SITE_PATH: &str = "./site";
pub const TEMPLATE_PATH: &str = "./site/templates";
pub const PUBLIC_PATH: &str = "./site/public";
pub const CSS_PATH: &str = "./site/styles";
pub const JS_PATH: &str = "./site/js";
pub const CONTENT_PATH: &str = "./site/content";
pub const PAGES_PATH: &str = "./site/content/pages";
pub const POSTS_PATH: &str = "./site/content/posts";

#[derive(Debug, Clone)]
pub struct Paths {
    pub out: PathBuf,
    pub source: PathBuf,
    pub templates: PathBuf,
    pub public: PathBuf,
    pub styles: PathBuf,
    pub js: PathBuf,
    pub content: PathBuf,
    pub pages: PathBuf,
    pub posts: PathBuf,
}

impl Paths {
    pub fn new() -> Self {
        Self {
            out: PathBuf::from(OUT_PATH),
            source: PathBuf::from(SITE_PATH),
            templates: PathBuf::from(TEMPLATE_PATH),
            public: PathBuf::from(PUBLIC_PATH),
            styles: PathBuf::from(CSS_PATH),
            js: PathBuf::from(JS_PATH),
            content: PathBuf::from(CONTENT_PATH),
            pages: PathBuf::from(PAGES_PATH),
            posts: PathBuf::from(POSTS_PATH),
        }
    }
}
