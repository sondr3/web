use std::path::Path;

use anyhow::Result;
use lightningcss::{
    printer::PrinterOptions,
    stylesheet::{MinifyOptions, ParserOptions, StyleSheet},
    targets::{Browsers, Targets},
};
use minify_html_onepass::Cfg;
use walkdir::DirEntry;

use crate::utils::find_files;

pub fn html(root: &Path) -> Result<()> {
    let cfg = Cfg {
        minify_js: true,
        minify_css: true,
    };

    find_files(root, is_html_file).try_for_each(|f| {
        let mut content = std::fs::read(&f)?;
        minify_html_onepass::in_place(&mut content, &cfg)
            .map_err(|e| anyhow::anyhow!("{:?}", e))?;
        std::fs::write(&f, content)?;
        Ok(())
    })
}

fn is_html_file(entry: &DirEntry) -> bool {
    let is_file = entry.file_type().is_file();
    let is_valid_extension = entry.path().extension().map_or(false, |e| e == "html");

    is_file && is_valid_extension
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
