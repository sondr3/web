use anyhow::Result;
use lightningcss::{
    printer::PrinterOptions,
    stylesheet::{MinifyOptions, ParserOptions, StyleSheet},
    targets::{Browsers, Targets},
};
use minify_html_onepass::Cfg;

pub fn html_minifier_config() -> Cfg {
    Cfg {
        minify_js: true,
        minify_css: true,
    }
}

pub fn html(content: &[u8], cfg: &Cfg) -> Result<Vec<u8>> {
    minify_html_onepass::copy(content, cfg).map_err(|e| anyhow::anyhow!("{:?}", e))
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
