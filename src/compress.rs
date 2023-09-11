use std::{io::prelude::*, path::Path};

use anyhow::{Context, Result};
use brotli::{enc::BrotliEncoderParams, CompressorWriter};
use flate2::{write::GzEncoder, Compression};
use walkdir::DirEntry;

use crate::utils::{find_files, AppendExtension};

const VALID_EXTENSIONS: [&str; 15] = [
    "html", "css", "js", "xml", "css", "cjs", "mjs", "json", "txt", "svg", "map", "ttf", "otf",
    "woff2", "eot",
];

pub fn folder(folder: &Path) -> Result<()> {
    find_files(folder, compressible_files).try_for_each(|f| {
        let content = std::fs::read(&f)?;
        let gzip = f.append_extension("gz");

        let mut gzip_encoder = GzEncoder::new(Vec::new(), Compression::best());
        gzip_encoder.write_all(&content)?;
        let gzipped = gzip_encoder.finish()?;

        let brotli = f.append_extension("br");
        let brotli_params = BrotliEncoderParams::default();
        let mut brotli_encoder = CompressorWriter::with_params(Vec::new(), 4096, &brotli_params);
        brotli_encoder.write_all(&content)?;

        std::fs::write(gzip, gzipped).context("Failed to write compressed file")?;
        std::fs::write(brotli, brotli_encoder.into_inner())
            .context("Failed to write compressed file")
    })
}

fn compressible_files(entry: &DirEntry) -> bool {
    let is_file = entry.file_type().is_file();
    let is_valid_extension = entry.path().extension().map_or(false, |ext| {
        VALID_EXTENSIONS.iter().any(|valid_ext| ext == *valid_ext)
    });

    is_file && is_valid_extension
}
