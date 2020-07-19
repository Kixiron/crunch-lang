use std::{
    fs::{File, Metadata},
    hash::{BuildHasher, BuildHasherDefault, Hash, Hasher},
    io::Result,
    path::Path,
    time::SystemTime,
};

/// The seed that will initialize the hasher used for hashing file metadata
const HASHER_SEED: u64 = 0xF00DBEEF;

/// Hashes file metadata
#[derive(Debug)]
pub struct FileHasher {
    hasher: BuildHasherDefault<FnvHasher>,
}

impl FileHasher {
    #[inline]
    pub fn new() -> Self {
        Self {
            hasher: BuildHasherDefault::default(),
        }
    }

    #[inline]
    pub fn hash_file<P: AsRef<Path>>(&self, path: P) -> Result<u64> {
        let path = path.as_ref();
        let meta = FileMeta::new(path)?;

        // Hash the file's metadata and the current compiler version
        // TODO: Hash the current linker and its version
        let hash = {
            let mut hasher = self.hasher.build_hasher();
            meta.hash(&mut hasher);
            crate::meta::CRUNCHC_VERSION.hash(&mut hasher);

            hasher.finish()
        };
        crate::debug!("Hashed '{}' as {:X}", path.display(), hash);

        Ok(hash)
    }
}

/// The metadata of a source file
#[derive(Debug, Clone, Hash)]
struct FileMeta<'a> {
    path: &'a Path,
    size: u64,
    created: SystemTime,
    modified: SystemTime,
    read_only: bool,
    os_meta: OsFileMeta,
}

impl<'a> FileMeta<'a> {
    #[inline]
    fn new(path: &'a Path) -> Result<Self> {
        let metadata = File::open(path)?.metadata()?;

        Ok(Self {
            path,
            size: metadata.len(),
            created: metadata.created()?,
            modified: metadata.modified()?,
            read_only: metadata.permissions().readonly(),
            os_meta: OsFileMeta::new(metadata),
        })
    }
}

/// The OS-dependent parts of file metadata
#[derive(Debug, Clone)]
#[repr(transparent)]
struct OsFileMeta(Metadata);

impl OsFileMeta {
    #[inline]
    pub const fn new(metadata: Metadata) -> Self {
        Self(metadata)
    }
}

impl Hash for OsFileMeta {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self(meta) = self;

        cfg_if::cfg_if! {
            // Windows metadata
            if #[cfg(target_family = "windows")] {
                use std::os::windows::fs::MetadataExt;

                meta.file_attributes().hash(state);
                meta.last_write_time().hash(state);

            // Unix metadata
            } else if #[cfg(target_family = "unix")] {
                use std::os::unix::fs::MetadataExt;

                meta.dev().hash(state);
                meta.ino().hash(state);
                meta.mode().hash(state);
                meta.uid().hash(state);
                meta.gid().hash(state);
                meta.mtime().hash(state);
            }
        }
    }
}

/// A wrapper around fnv so that we can use our own init seed for the hasher
#[repr(transparent)]
struct FnvHasher(fnv::FnvHasher);

impl Hasher for FnvHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0.finish()
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        self.0.write(bytes)
    }
}

impl Default for FnvHasher {
    #[inline]
    fn default() -> Self {
        Self(fnv::FnvHasher::with_key(HASHER_SEED))
    }
}

#[test]
fn hashes_are_consistent() {
    let hashes = [
        FileHasher::new().hash_file("Cargo.toml").unwrap(),
        FileHasher::new().hash_file("Cargo.toml").unwrap(),
        FileHasher::new().hash_file("Cargo.toml").unwrap(),
        FileHasher::new().hash_file("Cargo.toml").unwrap(),
        FileHasher::new().hash_file("Cargo.toml").unwrap(),
        FileHasher::new().hash_file("Cargo.toml").unwrap(),
    ];
    assert!(hashes.windows(2).all(|w| w[0] == w[1]));

    let hasher = FileHasher::new();
    let hashes = [
        hasher.hash_file("Cargo.toml").unwrap(),
        hasher.hash_file("Cargo.toml").unwrap(),
        hasher.hash_file("Cargo.toml").unwrap(),
        hasher.hash_file("Cargo.toml").unwrap(),
        hasher.hash_file("Cargo.toml").unwrap(),
    ];
    assert!(hashes.windows(2).all(|w| w[0] == w[1]));
}
