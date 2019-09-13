// https://dev.to/luzero/building-crates-so-they-look-like-c-abi-libraries-1ibn
// https://github.com/lu-zero/cargo-c

use std::cell::RefCell;
use std::ffi::{CStr, CString};
use std::fmt::Display;
use std::panic::{catch_unwind, UnwindSafe};
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
) -> *mut Scene {
    handle_errors(
        || {
            let filename = CStr::from_ptr(filename).to_str().unwrap_display();
            Box::into_raw(Box::new(
                Scene::new(
                    filename,
                    samplerate,
                    blocksize,
                    buffer_blocks,
                    Duration::from_micros(usleeptime),
                )
                .unwrap_display(),
            ))
        },
        std::ptr::null_mut(),
    )
}

#[no_mangle]
pub unsafe extern "C" fn asdf_scene_free(ptr: *mut Scene) {
    if !ptr.is_null() {
        Box::from_raw(ptr);
    }
}

#[no_mangle]
pub unsafe extern "C" fn asdf_scene_file_sources(ptr: *mut Scene) -> u32 {
    assert!(!ptr.is_null());
    let scene = &mut *ptr;
    scene.file_sources()
}

#[no_mangle]
pub unsafe extern "C" fn asdf_scene_get_source(ptr: *mut Scene, index: usize) -> *mut AsdfSource {
    // TODO: use handle_errors() once the ring buffer is UnwindSafe
    assert!(!ptr.is_null());
    let scene = &mut *ptr;
    Box::into_raw(Box::new(scene.get_source(index)))
}

#[no_mangle]
pub unsafe extern "C" fn asdf_source_free(ptr: *mut AsdfSource) {
    if !ptr.is_null() {
        Box::from_raw(ptr);
    }
}

#[no_mangle]
pub unsafe extern "C" fn asdf_scene_get_source_transform(
    ptr: *mut Scene,
    source_idx: usize,
    frame: u64,
) -> AsdfTransform {
    // TODO: use handle_errors() once the ring buffer is UnwindSafe
    assert!(!ptr.is_null());
    let scene = &mut *ptr;
    scene.get_source_transform(source_idx, frame).into()
}

// TODO: possibility to report errors?
#[no_mangle]
pub unsafe extern "C" fn asdf_scene_seek(ptr: *mut Scene, frame: u64) -> bool {
    assert!(!ptr.is_null());
    let scene = &mut *ptr;
    scene.seek(frame)
}

/// Return value of `false` means un-recoverable error
#[no_mangle]
pub unsafe extern "C" fn asdf_scene_get_audio_data(
    ptr: *mut Scene,
    data: *const *mut f32,
    rolling: bool,
) -> bool {
    // TODO: use handle_errors() once the ring buffer is UnwindSafe
    assert!(!ptr.is_null());
    let scene = &mut *ptr;
    assert!(!data.is_null());
    let data = std::slice::from_raw_parts(data, scene.file_sources() as usize);
    // TODO: get error message if something is wrong!
    scene.get_audio_data(data, rolling)
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

fn set_error<D: Display>(error: D) {
    LAST_ERROR.with(|cell| {
        *cell.borrow_mut() = CString::new(error.to_string()).unwrap();
    });
}

fn handle_errors<F, T>(f: F, optb: T) -> T
where
    F: FnOnce() -> T + UnwindSafe,
{
    match catch_unwind(f) {
        Ok(value) => value,
        Err(e) => {
            if let Some(e) = e.downcast_ref::<&str>() {
                set_error(*e);
            } else if let Some(e) = e.downcast_ref::<String>() {
                set_error(e);
            } else {
                set_error("unknown error");
            }
            optb
        }
    }
}

trait ResultExt<T, E: Display> {
    fn unwrap_display(self) -> T;
}

impl<T, E: Display> ResultExt<T, E> for Result<T, E> {
    fn unwrap_display(self) -> T {
        match self {
            Ok(value) => value,
            Err(e) => panic!(e.to_string()),
        }
    }
}
