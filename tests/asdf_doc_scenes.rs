use std::error::Error;
use std::path::Path;
use std::time::Duration;

use asdf::Scene;

#[test]
fn load_scenes() -> Result<(), BoxedError> {
    let scene_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("asdf")
        .join("doc")
        .join("scenes");

    let samplerate = 48000;
    let blocksize = 512;
    let buffer_blocks = 30;
    let sleeptime = Duration::from_millis(100);

    for entry in std::fs::read_dir(scene_dir)? {
        let file = entry?.path();
        if let Some(ext) = file.extension() {
            if ext == "asd" {
                println!("Checking {file:?}");
                let _scene = Scene::new(file, samplerate, blocksize, buffer_blocks, sleeptime)?;

                // TODO: get more information from the file?

                // TODO: seek?
            }
        }
    }
    Ok(())
}

struct BoxedError(Box<dyn Error>);

impl<E: Into<Box<dyn Error>>> From<E> for BoxedError {
    fn from(other: E) -> Self {
        Self(other.into())
    }
}

impl std::fmt::Debug for BoxedError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", self.0).unwrap();
        let sources = std::iter::successors(self.0.source(), |&e| e.source());
        for s in sources {
            write!(fmt, "\nerror details: {s}").unwrap();
        }
        Ok(())
    }
}
