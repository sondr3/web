use anyhow::Result;
use lightningcss::printer::PrinterOptions;
use lightningcss::stylesheet::{MinifyOptions, ParserOptions, StyleSheet};
use lightningcss::targets::{Browsers, Targets};
use minify_html::Cfg;

pub struct HtmlMinifier {
    cfg: Cfg,
}

impl HtmlMinifier {
    pub fn new() -> Self {
        let cfg = Cfg {
            minify_js: true,
            minify_css: true,
            keep_comments: false,
            keep_html_and_head_opening_tags: true,
            remove_bangs: false,
            remove_processing_instructions: false,
            ..Cfg::spec_compliant()
        };
        HtmlMinifier { cfg }
    }

    pub fn minify(&self, html: &str) -> Vec<u8> {
        minify_html::minify(html.as_bytes(), &self.cfg)
    }
}

pub struct CssMinifier;

impl CssMinifier {
    pub fn minify(css: &str) -> Result<String> {
        let mut stylesheet = StyleSheet::parse(css, ParserOptions::default())
            .map_err(|e| anyhow::anyhow!("{:?}", e))?;

        let targets = Targets {
            browsers: Browsers::from_browserslist(["> .5% and last 5 versions"])?,
            ..Default::default()
        };

        let minify_opts = MinifyOptions {
            targets,
            ..Default::default()
        };

        stylesheet.minify(minify_opts)?;

        let printer_opts = PrinterOptions {
            minify: true,
            targets,
            ..Default::default()
        };

        let res = stylesheet.to_css(printer_opts)?;
        Ok(res.code)
    }
}
