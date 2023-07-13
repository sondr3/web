use crate::utils::{find_files, AppendExtension};
use anyhow::{Context, Result};
use brotli::{enc::BrotliEncoderParams, CompressorWriter};
use flate2::{write::GzEncoder, Compression};
use std::{io::prelude::*, path::Path};
use walkdir::DirEntry;

const VALID_EXTENSIONS: [&str; 15] = [
    "html", "css", "js", "xml", "css", "cjs", "mjs", "json", "txt", "svg", "map", "ttf", "otf",
    "woff2", "eot",
];

pub fn folder(folder: &Path) -> Result<()> {
    gzip(folder)?;
    brotli(folder)?;

    Ok(())
}

fn compressible_files(entry: &DirEntry) -> bool {
    let is_file = entry.file_type().is_file();
    let is_valid_extension = entry.path().extension().map_or(false, |ext| {
        VALID_EXTENSIONS.iter().any(|valid_ext| ext == *valid_ext)
    });

    is_file && is_valid_extension
}

fn gzip(dir: &Path) -> Result<()> {
    find_files(dir, compressible_files).try_for_each(|f| {
        let content = std::fs::read(&f)?;
        let compressed = f.append_extension("gz");

        let mut encoder = GzEncoder::new(Vec::new(), Compression::best());
        encoder.write_all(&content)?;
        let res = encoder.finish()?;

        std::fs::write(compressed, res).context("Failed to write compressed file")
    })
}

fn brotli(dir: &Path) -> Result<()> {
    find_files(dir, compressible_files).try_for_each(|f| {
        let content = std::fs::read(&f)?;
        let compressed = f.append_extension("br");

        let params = BrotliEncoderParams::default();
        let mut compressor = CompressorWriter::with_params(Vec::new(), 4096, &params);
        compressor.write_all(&content)?;

        std::fs::write(compressed, compressor.into_inner())
            .context("Failed to write compressed file")
    })
}
