use std::{
    ffi::{OsStr, OsString},
    fs::File,
    path::{Path, PathBuf},
};

use anyhow::Result;
use sha1_smol::Sha1;
use walkdir::{DirEntry, WalkDir};

pub trait AppendExtension {
    fn append_extension(&self, ext: impl AsRef<OsStr>) -> PathBuf;
}

impl AppendExtension for PathBuf {
    fn append_extension(&self, ext: impl AsRef<OsStr>) -> PathBuf {
        let mut os_str: OsString = self.into();
        os_str.push(".");
        os_str.push(ext.as_ref());
        os_str.into()
    }
}

pub mod toml_date_deserializer {
    use serde::{self, Deserialize, Deserializer};
    use time::{Date, Month};
    use toml::value::Datetime;

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Date, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = Datetime::deserialize(deserializer)?;
        let Some(date) = s.date else {
            return Err(serde::de::Error::custom("missing date"));
        };

        let month = Month::try_from(date.month).map_err(serde::de::Error::custom)?;
        Date::from_calendar_date(i32::from(date.year), month, date.day)
            .map_err(serde::de::Error::custom)
    }
}

pub fn is_file(entry: &DirEntry) -> bool {
    entry.file_type().is_file()
}

pub fn is_visible(entry: &DirEntry) -> bool {
    !entry
        .file_name()
        .to_str()
        .map_or(false, |s| s.starts_with('.'))
}

pub fn find_files<F>(directory: &Path, filter_files: F) -> impl Iterator<Item = PathBuf>
where
    F: Fn(&DirEntry) -> bool,
{
    WalkDir::new(directory)
        .into_iter()
        .filter_entry(is_visible)
        .filter_map(Result::ok)
        .filter(filter_files)
        .map(|f| f.path().to_owned())
}

pub fn copy_file(root: impl AsRef<Path>, prefix: &str, entry: impl Into<PathBuf>) -> Result<()> {
    let path = entry.into();
    let filename = path.strip_prefix(prefix)?;

    let file: PathBuf = [root.as_ref(), filename].into_iter().collect();

    std::fs::create_dir_all(file.parent().unwrap())?;
    File::create(&file)?;
    std::fs::copy(path, file)?;

    Ok(())
}

pub fn write_file(path: &Path, content: impl AsRef<[u8]>) -> Result<()> {
    std::fs::create_dir_all(path.parent().unwrap())?;
    std::fs::write::<&Path, &[u8]>(path, content.as_ref())?;
    Ok(())
}

pub fn digest_filename(filename: &Path, content: &str) -> String {
    let digest = Sha1::from(content).hexdigest();
    let hash = digest.split_at(8).0;
    let Some(extension) = filename.extension() else {
        panic!("No extension found for {filename:?}");
    };

    PathBuf::from(filename)
        .with_extension(hash)
        .append_extension(extension)
        .display()
        .to_string()
}

pub fn filename(path: impl Into<PathBuf>) -> String {
    path.into().file_name().map_or_else(
        || panic!("No filename found"),
        |name| name.to_string_lossy().to_string(),
    )
}
