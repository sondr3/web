use crate::content::Content;
use crate::context::Context;
use anyhow::Result;
use quick_xml::se::Serializer;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Display, Formatter};
use time::Date;
use url::Url;

#[derive(Debug, Deserialize, Serialize, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum ChangeFreq {
    Always,
    Hourly,
    Daily,
    Weekly,
    Monthly,
    Yearly,
    Never,
}

impl Display for ChangeFreq {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ChangeFreq::Always => write!(f, "always"),
            ChangeFreq::Hourly => write!(f, "hourly"),
            ChangeFreq::Daily => write!(f, "daily"),
            ChangeFreq::Weekly => write!(f, "weekly"),
            ChangeFreq::Monthly => write!(f, "monthly"),
            ChangeFreq::Yearly => write!(f, "yearly"),
            ChangeFreq::Never => write!(f, "never"),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct UrlEntry {
    pub loc: Url,
    #[serde(rename = "lastmod")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub last_mod: Option<Date>,
    #[serde(rename = "changefreq")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub change_freq: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub priority: Option<f32>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename = "urlset")]
pub struct UrlSet {
    #[serde(rename = "@xmlns")]
    pub xmlns: String,
    #[serde(rename = "@xmlns:image")]
    pub xmlns_image: String,
    #[serde(rename = "@xmlns:video")]
    pub xmlns_video: String,
    #[serde(rename = "url")]
    pub urls: Vec<UrlEntry>,
}

impl UrlEntry {
    pub fn new(
        loc: Url,
        last_mod: Option<Date>,
        change_freq: Option<ChangeFreq>,
        priority: Option<f32>,
    ) -> Self {
        assert!(priority.map_or(true, |e| (0.0..=1.0).contains(&e)));

        Self {
            loc,
            last_mod,
            change_freq: change_freq.map(|e| e.to_string()),
            priority,
        }
    }

    pub fn from_content(value: &Content, base: &Url) -> Result<Self> {
        let url = base.join(&value.url)?;

        Ok(UrlEntry::new(
            url,
            Some(value.frontmatter.last_modified),
            Some(ChangeFreq::Monthly),
            None,
        ))
    }
}

pub fn create(context: &Context) -> Result<String> {
    let urls: Result<Vec<_>, _> = context
        .pages
        .values()
        .map(|e| UrlEntry::from_content(e, &context.metadata.url))
        .collect();

    let mut root = r#"
<?xml version="1.0" encoding="UTF-8"?>
<?xml-stylesheet href="/sitemap-style.xsl" type="text/xsl"?>
    "#
    .trim_start()
    .to_string();

    let url_set = UrlSet {
        xmlns: "http://www.sitemaps.org/schemas/sitemap/0.9".to_string(),
        xmlns_image: "http://www.google.com/schemas/sitemap-image/1.1".to_string(),
        xmlns_video: "http://www.google.com/schemas/sitemap-video/1.1".to_string(),
        urls: urls?,
    };

    let mut buffer = String::new();
    let mut ser = Serializer::new(&mut buffer);
    ser.indent(' ', 2);

    url_set.serialize(ser)?;
    root.push_str(&buffer);

    Ok(root)
}
