use anyhow::Result;
use lightningcss::{
    printer::PrinterOptions,
    stylesheet::{MinifyOptions, ParserOptions, StyleSheet},
    targets::{Browsers, Targets},
};
use minify_html::Cfg;

pub fn html_minifier_config() -> Cfg {
    Cfg {
        minify_js: true,
        minify_css: true,
        keep_comments: false,
        keep_html_and_head_opening_tags: true,
        remove_bangs: false,
        remove_processing_instructions: false,
        ..Cfg::spec_compliant()
    }
}

pub fn html(content: &str, cfg: &Cfg) -> Vec<u8> {
    minify_html::minify(content.as_bytes(), cfg)
}

pub fn css(content: &str) -> Result<String> {
    let mut stylesheet = StyleSheet::parse(content, ParserOptions::default())
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
