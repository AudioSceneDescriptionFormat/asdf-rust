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

/// Static information about a source.
///
/// Use asdf_scene_get_source_transform() to get dynamic information
/// about a source at a given frame.
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

/// Load an ASDF scene from a file.
///
/// Before starting playback (i.e. calling asdf_scene_get_audio_data()
/// with `rolling` set to `true`), asdf_scene_seek() has to be called.
///
/// The returned object must be discarded with asdf_scene_free().
///
/// In case of an error, NULL is returned and
/// asdf_scene_last_error() can be used to get an error description.
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

/// Discard a scene object created with asdf_scene_new().
///
/// Passing NULL is allowed.
#[no_mangle]
pub extern "C" fn asdf_scene_free(_: Option<Box<Scene>>) {}

/// Get number of file sources.
#[no_mangle]
pub extern "C" fn asdf_scene_file_sources(scene: &Scene) -> u32 {
    scene.file_sources()
}

/// Get an AsdfSource object for a given (0-based) source index.
///
/// Calling this function with an invalid source index invokes undefined behavior.
///
/// The returned object must be discarded with asdf_source_free().
#[no_mangle]
pub extern "C" fn asdf_scene_get_source(scene: &Scene, source_index: usize) -> Box<AsdfSource> {
    Box::new(scene.get_source(source_index))
}

/// Discard a source object created with asdf_scene_get_source().
#[no_mangle]
pub extern "C" fn asdf_source_free(_: Option<Box<AsdfSource>>) {}

/// Get AsdfTransform for a given (0-based) source index at a given frame.
///
/// Calling this function with an invalid source index invokes undefined behavior.
///
/// This function is realtime-safe.
#[no_mangle]
pub extern "C" fn asdf_scene_get_source_transform(
    scene: &Scene,
    source_index: usize,
    frame: u64,
) -> AsdfTransform {
    scene.get_source_transform(source_index, frame).into()
}

/// Get AsdfTransform for the reference at a given frame.
///
/// The reference transform is always "active".
///
/// This function is realtime-safe.
#[no_mangle]
pub extern "C" fn asdf_scene_get_reference_transform(scene: &Scene, frame: u64) -> AsdfTransform {
    scene.get_reference_transform(frame).into()
}

/// Seek to the given frame.
///
/// Returns `true` when seeking has completed.  If `false` is returned,
/// the function has to be called again at a later time
/// until `true` is returned.
///
/// While seeking, it is not allowed to call asdf_scene_get_audio_data()
/// with the `rolling` argument set to `true`.
///
/// A return value of `false` doesn't mean an error occured,
/// therefore asdf_scene_last_error() will not contain relevant information.
///
/// This function is realtime-safe.
#[no_mangle]
pub extern "C" fn asdf_scene_seek(scene: &mut Scene, frame: u64) -> bool {
    scene.seek(frame)
}

/// Get a block of audio data.
///
/// If `rolling` is `false`, `data` will be filled with zeros.
/// Before being able to call this function with `rolling` set to `true`,
/// asdf_scene_seek() has to be called.
///
/// A return value of `false` means un-recoverable error,
/// don't try calling again.
/// In case of an error, `data` will be filled with zeros
/// and asdf_scene_last_error() will contain an error description.
///
/// This function is realtime-safe.
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

/// Obtain the error message of the last error.
///
/// The error message will be freed if another error occurs. It is the caller's
/// responsibility to make sure they're no longer using the string before
/// calling any other function which may fail.
///
/// The error message is thread-local, i.e. it can only be obtained
/// from the thread on which the error occured.
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
