use std::error::Error;
use std::path::Path;
use std::time::Duration;

#[test]
fn load_scenes() -> Result<(), Box<dyn Error>> {
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
                println!("Checking {:?}", file);
                let _scene =
                    asdf::Scene::new(file, samplerate, blocksize, buffer_blocks, sleeptime)?;

                // TODO: give useful information on failure

                // TODO: get more information from the file?

                // TODO: seek?
            }
        }
    }
    Ok(())
}
