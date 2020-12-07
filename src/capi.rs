// https://dev.to/luzero/building-crates-so-they-look-like-c-abi-libraries-1ibn
// https://github.com/lu-zero/cargo-c

use std::cell::RefCell;
use std::ffi::{CStr, CString};
use std::fmt::{Display, Write};
use std::time::Duration;

use libc::c_char;
use rsor::Slice;

use crate::transform::{Quat, Transform, Vec3};
use crate::{Scene, Source};

pub struct AsdfScene {
    inner: Scene,
    blocksize: u32,
    sos: Slice<[f32]>,
}

impl AsdfScene {
    pub fn get_source(&self, index: u32) -> AsdfSourceInfo {
        AsdfSourceInfo::new(
            &self.inner.sources[index as usize],
            self.inner.get_source_port(index),
        )
    }
}

#[repr(C)]
#[derive(Default)]
pub struct AsdfTransform {
    active: bool,
    pos: [f32; 3],
    /// Vector part of quaternion
    rot_v: [f32; 3],
    /// Scalar part of quaternion
    rot_s: f32,
    vol: f32,
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
                vol: t.volume.unwrap_or(1.0),
            }
        } else {
            AsdfTransform::default()
        }
    }
}

/// Static information about a source.
///
/// Use asdf_get_source_transform() to get dynamic information
/// about a source at a given frame.
#[repr(C)]
pub struct AsdfSourceInfo {
    id: *const c_char,
    name: *const c_char,
    model: *const c_char,
    port: *const c_char,
}

impl AsdfSourceInfo {
    fn new(source: &Source, port: Option<&String>) -> AsdfSourceInfo {
        fn char_ptr(s: Option<&String>) -> *const c_char {
            CString::new(s.map(String::as_ref).unwrap_or(""))
                .unwrap()
                .into_raw()
        }
        AsdfSourceInfo {
            id: char_ptr(source.id.as_ref()),
            name: char_ptr(source.name.as_ref()),
            model: char_ptr(source.model.as_ref()),
            port: char_ptr(port),
        }
    }
}

impl Drop for AsdfSourceInfo {
    fn drop(&mut self) {
        unsafe {
            CString::from_raw(self.id as *mut _);
            CString::from_raw(self.name as *mut _);
            CString::from_raw(self.model as *mut _);
        }
    }
}

#[allow(non_camel_case_types)]
#[repr(C)]
pub enum AsdfStreamingResult {
    ASDF_STREAMING_SUCCESS,
    ASDF_STREAMING_EMPTY_BUFFER,
    ASDF_STREAMING_INCOMPLETE_SEEK,
    ASDF_STREAMING_SEEK_WHILE_ROLLING,
}

impl From<Result<(), crate::streamer::StreamingError>> for AsdfStreamingResult {
    fn from(result: Result<(), crate::streamer::StreamingError>) -> Self {
        use crate::streamer::StreamingError::*;
        use AsdfStreamingResult::*;
        match result {
            Ok(()) => ASDF_STREAMING_SUCCESS,
            Err(EmptyBuffer) => ASDF_STREAMING_EMPTY_BUFFER,
            Err(IncompleteSeek) => ASDF_STREAMING_INCOMPLETE_SEEK,
            Err(SeekWhileRolling) => ASDF_STREAMING_SEEK_WHILE_ROLLING,
        }
    }
}

/// Load an ASDF scene from a file.
///
/// Before starting playback (i.e. calling asdf_get_audio_data()
/// with `rolling` set to `true`), asdf_seek() has to be called.
///
/// The returned object must be discarded with asdf_scene_free().
///
/// In case of an error, NULL is returned and
/// asdf_last_error() can be used to get an error description.
#[no_mangle]
pub unsafe extern "C" fn asdf_scene_new(
    filename: *const c_char,
    samplerate: u32,
    blocksize: u32,
    buffer_blocks: u32,
    usleeptime: u64,
) -> Option<Box<AsdfScene>> {
    let filename = CStr::from_ptr(filename)
        .to_str()
        .map_err(|e| {
            set_error_string(format!("Invalid filename: {}", e));
        })
        .ok()?;
    let inner = Scene::new(
        filename,
        samplerate,
        blocksize,
        buffer_blocks,
        Duration::from_micros(usleeptime),
    )
    .map_err(|e| set_error(&e))
    .ok()?;
    let file_sources = inner.file_sources() as usize;
    Some(Box::new(AsdfScene {
        inner,
        blocksize,
        sos: Slice::with_capacity(file_sources),
    }))
}

/// Discard a scene object created with asdf_scene_new().
///
/// Passing NULL is allowed.
#[no_mangle]
pub extern "C" fn asdf_scene_free(_: Option<Box<AsdfScene>>) {}

/// Get number of file sources.
#[no_mangle]
pub extern "C" fn asdf_file_sources(scene: &AsdfScene) -> u32 {
    scene.inner.file_sources()
}

/// Get number of live sources.
#[no_mangle]
pub extern "C" fn asdf_live_sources(scene: &AsdfScene) -> u32 {
    scene.inner.live_sources()
}

/// Get scene duration in frames.
///
/// Returns `0` if the duration is undefined.
#[no_mangle]
pub extern "C" fn asdf_frames(scene: &AsdfScene) -> u64 {
    scene.inner.frames().unwrap_or(0)
}

/// Get an AsdfSourceInfo object for a given (0-based) source index.
///
/// Calling this function with an invalid source index invokes undefined behavior.
///
/// The returned object must be discarded with asdf_sourceinfo_free().
#[no_mangle]
pub extern "C" fn asdf_get_sourceinfo(scene: &AsdfScene, source_index: u32) -> Box<AsdfSourceInfo> {
    Box::new(scene.get_source(source_index))
}

/// Discard a source object created with asdf_get_sourceinfo().
#[no_mangle]
pub extern "C" fn asdf_sourceinfo_free(_: Option<Box<AsdfSourceInfo>>) {}

/// Get AsdfTransform for a given (0-based) source index at a given frame.
///
/// Calling this function with an invalid source index invokes undefined behavior.
///
/// This function is realtime-safe.
#[no_mangle]
pub extern "C" fn asdf_get_source_transform(
    scene: &AsdfScene,
    source_index: u32,
    frame: u64,
) -> AsdfTransform {
    scene.inner.get_source_transform(source_index, frame).into()
}

/// Get AsdfTransform for the reference at a given frame.
///
/// The reference transform is always "active".
///
/// This function is realtime-safe.
#[no_mangle]
pub extern "C" fn asdf_get_reference_transform(scene: &AsdfScene, frame: u64) -> AsdfTransform {
    scene.inner.get_reference_transform(frame).into()
}

/// Seek to the given frame.
///
/// Returns `true` when seeking has completed.  If `false` is returned,
/// the function has to be called again at a later time
/// until `true` is returned.
///
/// While seeking, it is not allowed to call asdf_get_audio_data()
/// with the `rolling` argument set to `true`.
///
/// A return value of `false` doesn't mean an error occured,
/// therefore asdf_last_error() will not contain relevant information.
///
/// This function is realtime-safe.
#[no_mangle]
pub extern "C" fn asdf_seek(scene: &mut AsdfScene, frame: u64) -> bool {
    scene.inner.seek(frame)
}

/// Get a block of audio data.
///
/// If `rolling` is `false`, `data` will be filled with zeros.
/// Before being able to call this function with `rolling` set to `true`,
/// asdf_seek() has to be called (potentially repeatedly, until it returns `true`).
///
/// In case of an error, `data` will be filled with zeros.
///
/// `data` is only allowed to be NULL when there are no file sources.
///
/// This function is realtime-safe but not re-entrant.
#[no_mangle]
pub unsafe extern "C" fn asdf_get_audio_data(
    scene: &mut AsdfScene,
    data: *const *mut f32,
    rolling: bool,
) -> AsdfStreamingResult {
    let sources = scene.inner.file_sources() as usize;
    // NB: C++ doesn't guarantee that std::vector::data() is non-null for an empty vector.
    let data = if sources != 0 {
        data
    } else {
        std::ptr::NonNull::dangling().as_ptr()
    };
    let pointers = std::slice::from_raw_parts(data, sources);
    let blocksize = scene.blocksize as usize;

    // This mutably borrows `scene.sos` and is therefore not re-entrant.
    let data = scene.sos.from_iter_mut(
        pointers
            .iter()
            .map(|&ptr| std::slice::from_raw_parts_mut(ptr, blocksize)),
    );
    // This mutably borrows `scene.inner` and is therefore not re-entrant.
    scene.inner.get_audio_data(data, rolling).into()
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
pub extern "C" fn asdf_last_error() -> *const c_char {
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
