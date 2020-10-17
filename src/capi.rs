// https://dev.to/luzero/building-crates-so-they-look-like-c-abi-libraries-1ibn
// https://github.com/lu-zero/cargo-c

use std::cell::RefCell;
use std::ffi::{CStr, CString};
use std::fmt::{Display, Write};
use std::time::Duration;

use libc::c_char;

use crate::transform::{Quat, Transform, Vec3};
use crate::{Scene, Source};

#[repr(C)]
#[derive(Default)]
pub struct AsdfTransform {
    active: bool,
    pos: [f32; 3],
    /// Vector part of quaternion
    rot_v: [f32; 3],
    /// Scalar part of quaternion
    rot_s: f32,
}

impl From<Option<Transform>> for AsdfTransform {
    fn from(t: Option<Transform>) -> AsdfTransform {
        if let Some(t) = t {
            let rot = t.rotation.unwrap_or_else(Quat::identity);
            AsdfTransform {
                active: true,
                pos: t.translation.unwrap_or_else(Vec3::zeros).into(),
                rot_v: rot.vector().into(),
                rot_s: rot.scalar(),
            }
        } else {
            AsdfTransform::default()
        }
    }
}

#[repr(C)]
pub struct AsdfSource {
    id: *const c_char,
    name: *const c_char,
    model: *const c_char,
}

impl AsdfSource {
    fn new(source: &Source) -> AsdfSource {
        fn char_ptr(s: &Option<String>) -> *const c_char {
            CString::new(s.as_ref().map(String::as_ref).unwrap_or(""))
                .unwrap()
                .into_raw()
        }
        AsdfSource {
            id: char_ptr(&source.id),
            name: char_ptr(&source.name),
            model: char_ptr(&source.model),
        }
    }
}

impl Drop for AsdfSource {
    fn drop(&mut self) {
        unsafe {
            CString::from_raw(self.id as *mut _);
            CString::from_raw(self.name as *mut _);
            CString::from_raw(self.model as *mut _);
        }
    }
}

impl Scene {
    pub fn get_source(&self, index: usize) -> AsdfSource {
        AsdfSource::new(&self.sources[index])
    }
}

#[no_mangle]
pub unsafe extern "C" fn asdf_scene_new(
    filename: *const c_char,
    samplerate: u32,
    blocksize: u32,
    buffer_blocks: u32,
    usleeptime: u64,
) -> Option<Box<Scene>> {
    let filename = match CStr::from_ptr(filename).to_str() {
        Ok(s) => s,
        Err(e) => {
            set_error_string(format!("Invalid filename: {}", e));
            return None;
        }
    };
    match Scene::new(
        filename,
        samplerate,
        blocksize,
        buffer_blocks,
        Duration::from_micros(usleeptime),
    ) {
        Ok(scene) => Some(Box::new(scene)),
        Err(e) => {
            set_error(&e);
            None
        }
    }
}

#[no_mangle]
pub extern "C" fn asdf_scene_free(_: Option<Box<Scene>>) {}

#[no_mangle]
pub extern "C" fn asdf_scene_file_sources(scene: &Scene) -> u32 {
    scene.file_sources()
}

#[no_mangle]
pub extern "C" fn asdf_scene_get_source(scene: &Scene, index: usize) -> Box<AsdfSource> {
    Box::new(scene.get_source(index))
}

#[no_mangle]
pub extern "C" fn asdf_source_free(_: Option<Box<AsdfSource>>) {}

#[no_mangle]
pub extern "C" fn asdf_scene_get_source_transform(
    scene: &Scene,
    source_idx: usize,
    frame: u64,
) -> AsdfTransform {
    scene.get_source_transform(source_idx, frame).into()
}

/// Reference transform is always "active".
#[no_mangle]
pub extern "C" fn asdf_scene_get_reference_transform(scene: &Scene, frame: u64) -> AsdfTransform {
    scene.get_reference_transform(frame).into()
}

// TODO: possibility to report errors?
#[no_mangle]
pub extern "C" fn asdf_scene_seek(scene: &mut Scene, frame: u64) -> bool {
    scene.seek(frame)
}

/// Return value of `false` means un-recoverable error
/// `data` will be filled with zeros in case of an error.
#[no_mangle]
pub unsafe extern "C" fn asdf_scene_get_audio_data(
    scene: &mut Scene,
    data: *const *mut f32,
    rolling: bool,
) -> bool {
    assert!(!data.is_null());
    let data = std::slice::from_raw_parts(data, scene.file_sources() as usize);
    scene
        .get_audio_data(data, rolling)
        .map_err(|e| set_error(&e))
        .is_ok()
}

/// The error message will be freed if another error occurs. It is the caller's
/// responsibility to make sure they're no longer using the string before
/// calling any other function which may fail.
#[no_mangle]
pub extern "C" fn asdf_scene_last_error() -> *const c_char {
    LAST_ERROR.with(|cell| cell.borrow().as_ptr())
}

thread_local! {
    static LAST_ERROR: RefCell<CString> = RefCell::new(CString::new("no error").unwrap());
}

fn set_error_string(error: impl Display) {
    LAST_ERROR.with(|cell| {
        *cell.borrow_mut() = CString::new(error.to_string()).unwrap();
    });
}

fn set_error(error: &dyn std::error::Error) {
    let mut msg = String::new();
    write!(&mut msg, "{}", error).unwrap();
    let sources = std::iter::successors(error.source(), |e| e.source());
    for s in sources {
        write!(&mut msg, "\nerror details: {}", s).unwrap();
    }
    set_error_string(msg);
}
