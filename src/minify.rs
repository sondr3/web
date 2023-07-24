use std::path::Path;

use anyhow::Result;
use lightningcss::{
    printer::PrinterOptions,
    stylesheet::{MinifyOptions, ParserOptions, StyleSheet},
    targets::{Browsers, Targets},
};
use minify_html::Cfg;
use walkdir::DirEntry;

use crate::utils::find_files;

pub fn html(root: &Path) -> Result<()> {
    let cfg = html_minifier_config();

    find_files(root, is_html_file).try_for_each(|f| {
        let content = std::fs::read(&f)?;
        let content = minify_html::minify(&content, &cfg);
        std::fs::write(&f, content)?;
        Ok(())
    })
}

pub fn html_minifier_config() -> Cfg {
    Cfg {
        minify_js: true,
        minify_css: true,
        keep_comments: false,
        keep_html_and_head_opening_tags: true,
        remove_bangs: true,
        remove_processing_instructions: true,
        ..Cfg::spec_compliant()
    }
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
