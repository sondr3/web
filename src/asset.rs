use crate::utils::AppendExtension;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct AssetFile {
    pub filename: String,
    pub extension: String,
    pub content: String,
}

impl AssetFile {
    pub fn final_filename(&self, production: bool) -> String {
        if production {
            self.hash()
        } else {
            format!("{}.{}", self.filename, self.extension)
        }
    }

    fn hash(&self) -> String {
        let digest = format!("{:x}", md5::compute(&self.content));
        let hash = digest.split_at(8).0;

        Path::new(&self.filename)
            .to_path_buf()
            .append_extension(hash)
            .append_extension(&self.extension)
            .display()
            .to_string()
    }
}

#[derive(Debug)]
pub struct BuiltAssetFile {
    pub filename: String,
    pub content: String,
}

impl BuiltAssetFile {
    pub fn from_asset(asset: &AssetFile, production: bool) -> Self {
        Self {
            filename: asset.final_filename(production),
            content: asset.content.clone(),
        }
    }
}

#[derive(Debug)]
pub struct PublicFile {
    pub path: PathBuf,
    pub prefix: String,
}
