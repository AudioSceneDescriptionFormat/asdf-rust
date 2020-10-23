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
#![warn(rust_2018_idioms)]

use std::collections::HashMap;
use std::path::Path;
use std::time::Duration;

#[macro_use]
extern crate lazy_static;

mod audiofile;
mod parser;
mod streamer;
mod transform;

#[cfg(cargo_c)]
mod capi;

pub use crate::parser::error::LoadError as SceneLoadError;
use crate::streamer::{FileStreamer, StreamingError};
use crate::transform::Transform;

const REFERENCE_ID: &str = "reference";

type TransformerStorage = Box<[(Box<dyn Transformer>, Box<[(u64, u64)]>)]>;

/// An audio scene loaded from an ASDF file.
pub struct Scene {
    sources: Vec<Source>,
    streamer: FileStreamer,
    /// Transformers with list of activity
    transformers: TransformerStorage,
    /// Map from ID to list of transformers directly applying to this ID
    transformer_map: HashMap<String, Box<[usize]>>,
    reference_transform: Transform,
    /// One port for each live source
    ports: Vec<String>,
    frames: Option<u64>,
}

impl Scene {
    /// Loads an ASDF scene from a file.
    pub fn new<P: AsRef<Path>>(
        path: P,
        samplerate: u32,
        blocksize: u32,
        buffer_blocks: u32,
        sleeptime: Duration,
    ) -> Result<Scene, SceneLoadError> {
        parser::load_scene(
            path.as_ref(),
            samplerate,
            blocksize,
            buffer_blocks,
            sleeptime,
        )
    }

    pub fn frames(&self) -> Option<u64> {
        self.frames
    }

    pub fn file_sources(&self) -> u32 {
        self.streamer.channels()
    }

    pub fn live_sources(&self) -> u32 {
        self.sources.len() as u32 - self.file_sources()
    }

    pub fn get_source_id(&self, index: u32) -> Option<&String> {
        self.sources[index as usize].id.as_ref()
    }

    pub fn get_source_name(&self, index: u32) -> Option<&String> {
        self.sources[index as usize].name.as_ref()
    }

    pub fn get_source_model(&self, index: u32) -> Option<&String> {
        self.sources[index as usize].model.as_ref()
    }

    pub fn get_source_port(&self, index: u32) -> Option<&String> {
        if index >= self.file_sources() {
            Some(&self.ports[(index - self.file_sources()) as usize])
        } else {
            None
        }
    }

    pub fn seek(&mut self, frame: u64) -> bool {
        self.streamer.seek(frame)
    }

    /// Any error should be considered un-recoverable.
    /// `target` will be filled with zeros in case of an error.
    ///
    /// # Safety
    ///
    /// `target` must be pointing to a writable memory area of sufficient size.
    pub unsafe fn get_audio_data(
        &mut self,
        target: &[*mut f32],
        rolling: bool,
    ) -> Result<(), StreamingError> {
        self.streamer.get_data(target, rolling)
    }

    /// `source_idx`: Zero-based source number
    /// Panics if `source_idx` is out of range.
    pub fn get_source_transform(&self, source_idx: u32, frame: u64) -> Option<Transform> {
        // NB: This function is supposed to be realtime-safe!
        let source = &self.sources[source_idx as usize];
        let t = self.get_transform_applying_to(source.id.as_ref(), frame);
        if let Some(mut transform) = source.transform.clone() {
            transform.apply(t);
            Some(transform)
        } else {
            t
        }
    }

    pub fn get_reference_transform(&self, frame: u64) -> Option<Transform> {
        let mut reference_transform = self.reference_transform.clone();
        reference_transform
            .apply(self.get_transform_applying_to(Some(&REFERENCE_ID.into()), frame));
        Some(reference_transform)
    }

    fn get_transform_from(&self, idx: usize, frame: u64) -> Option<Transform> {
        // TODO: optimization: store result for last used frame number?

        // TODO: check activity before or after memoization?

        let (transformer, activity) = &self.transformers[idx];

        for &(begin, end) in activity.iter() {
            if begin <= frame && frame < end {
                let result = transformer.get_transform(frame - begin);
                let id = transformer.id();
                // NB: Recursive call. Cyclic dependencies have been ruled out on scene init
                let t = self.get_transform_applying_to(id, frame);
                if let Some(mut result) = result {
                    result.apply(t);
                    return Some(result);
                } else {
                    return t;
                }
            }
        }
        None
    }

    fn get_transform_applying_to(&self, id: Option<&String>, frame: u64) -> Option<Transform> {
        let transformers = self.transformer_map.get(id?)?;
        transformers.iter().fold(None, |transform, &idx| {
            // NB: Recursive call. Cyclic dependencies have been ruled out on scene init
            Transform::merge(transform, self.get_transform_from(idx, frame))
        })
    }
}

trait Transformer {
    fn id(&self) -> Option<&String>;
    /// begin and end is checked before calling this
    fn get_transform(&self, frame: u64) -> Option<Transform>;
}

#[derive(Default, Debug)]
struct Source {
    // TODO: remove Option?
    id: Option<String>,
    name: Option<String>,
    model: Option<String>,
    /// Transform given in <head> element
    transform: Option<Transform>,
}
