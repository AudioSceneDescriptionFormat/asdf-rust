/*!
Library for loading [Audio Scene Description Format (ASDF)][ASDF] files,
written in Rust.

[ASDF]: https://AudioSceneDescriptionFormat.readthedocs.io/

# Requirements

* Rust compiler, Cargo (<https://rustup.rs/>)

The required Rust packages (a.k.a. "crates") are listed in the file
`Cargo.toml`.

# API Documentation

Run `cargo doc` in the main directory to create the documentation.
The generated HTML documentation can be accessed via
[target/doc/asdf/index.html](index.html).

# Building the C API

Using [cargo-c](https://github.com/lu-zero/cargo-c)
(`cargo install cargo-c`):

```text
cargo cbuild --release
cargo cinstall --release
```

Further options (like `--destdir` and `--prefix`) are available.

# Updating `README.md`

Using [cargo-readme](https://github.com/livioribeiro/cargo-readme)
(`cargo install cargo-readme`):

```text
cargo readme -o README.md
```
*/
use std::collections::HashMap;
use std::path::Path;
use std::time::Duration;

#[macro_use]
extern crate lazy_static;

mod audiofile;
mod error;
mod parser;
mod streamer;
mod transform;

#[cfg(cargo_c)]
mod capi;

use crate::parser::error::LoadError;
use crate::streamer::FileStreamer;
use crate::transform::Transform;

type TransformerStorage = Box<[(Box<dyn Transformer>, Box<[(u64, u64)]>)]>;

pub struct Scene {
    sources: Vec<Source>,
    streamer: FileStreamer,
    /// Transformers with list of activity
    transformers: TransformerStorage,
    /// Map from ID to list of transformers directly applying to this ID
    transformer_map: HashMap<String, Box<[usize]>>,
}

impl Scene {
    /// Loads an ASDF scene from a file.
    pub fn new<P: AsRef<Path>>(
        path: P,
        samplerate: u32,
        blocksize: u32,
        buffer_blocks: u32,
        sleeptime: Duration,
    ) -> Result<Scene, LoadError> {
        parser::load_scene(
            path.as_ref(),
            samplerate,
            blocksize,
            buffer_blocks,
            sleeptime,
        )
    }

    pub fn file_sources(&self) -> u32 {
        self.streamer.channels()
    }

    pub fn get_source_id(&self, index: usize) -> Option<&String> {
        self.sources[index].id.as_ref()
    }

    pub fn seek(&mut self, frame: u64) -> bool {
        self.streamer.seek(frame)
    }

    /// Return value of `false` means un-recoverable error
    #[must_use]
    pub unsafe fn get_audio_data(&mut self, target: &[*mut f32], rolling: bool) -> bool {
        self.streamer.get_data(target, rolling)
    }

    /// `source_idx`: Zero-based source number
    /// Panics if `source_idx` is out of range.
    pub fn get_source_transform(&self, source_idx: usize, frame: u64) -> Option<Transform> {
        // NB: This function is supposed to be realtime-safe!

        let source = &self.sources[source_idx];

        // Transforms applied to <clip> (and its <channel> elements)

        // TODO: what about live sources?

        // NB: If source is not active, we don't need to check other transforms
        let mut result = self.get_clip_transform(source, frame)?;

        // Transforms applied directly to the source

        if let Some(additional_transform) =
            self.get_transform_applying_to(source.id.as_ref(), frame)
        {
            result.accumulate(&additional_transform);
        }

        Some(result)
    }

    // TODO: what about transforms of live sources?
    fn get_clip_transform(&self, source: &Source, frame: u64) -> Option<Transform> {
        for &idx in source.activity.iter() {
            if let Some(transform) = self.get_transform_from(idx, frame) {
                // NB: Only one of them can be active, so we don't need to look further
                return Some(transform);
            }
        }
        None
    }

    fn get_transform_from(&self, idx: usize, frame: u64) -> Option<Transform> {
        // TODO: optimization: store result for last used frame number?

        // TODO: check activity before or after memoization?

        let (transformer, activity) = &self.transformers[idx];

        for &(begin, end) in activity.iter() {
            if begin <= frame && frame < end {
                let mut result = transformer.get_transform(frame - begin);
                let id = transformer.id();
                // TODO: Establish recursion limit! There might be circular dependencies!
                if let Some(additional_transform) = self.get_transform_applying_to(id, frame) {
                    result.apply(&additional_transform);
                }
                return Some(result);
            }
        }
        None
    }

    fn get_transform_applying_to(&self, id: Option<&String>, frame: u64) -> Option<Transform> {
        let transformers = self.transformer_map.get(id?)?;

        // TODO: Establish recursion limit! There might be circular dependencies!

        let mut result: Option<Transform> = None;
        for &idx in transformers.iter() {
            if let Some(transform) = self.get_transform_from(idx, frame) {
                result
                    .get_or_insert(Default::default())
                    .accumulate(&transform);
            }
        }
        result
    }
}

trait Transformer {
    fn id(&self) -> Option<&String>;
    /// begin and end is checked before calling this
    fn get_transform(&self, frame: u64) -> Transform;
}

struct Source {
    id: Option<String>,
    /// List of transforms that define when source is active
    activity: Box<[usize]>,
    // TODO: live or file source?
}
