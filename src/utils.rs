use anyhow::Result;
use std::ffi::{OsStr, OsString};
use std::fs::File;
use std::path::{Path, PathBuf};
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

pub fn is_file(entry: &DirEntry) -> bool {
    entry.file_type().is_file()
}

pub fn is_visible(entry: &DirEntry) -> bool {
    !entry
        .file_name()
        .to_str()
        .map(|s| s.starts_with('.'))
        .unwrap_or(false)
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

pub fn write_file(path: &Path, content: &str) -> Result<()> {
    std::fs::create_dir_all(path.parent().unwrap())?;
    std::fs::write(path, content)?;
    Ok(())
}
