use std::io::{self, Read, Seek};
use std::mem::MaybeUninit;

use libc::{c_int, c_void};

use minimp3_ex_sys as ffi;

use super::{AudioFileBasics, BoxedError};

const EIO: errno::Errno = errno::Errno(5);

pub struct File<R> {
    #[allow(dead_code)]
    reader: Box<R>,
    decoder: ffi::mp3dec_ex_t,
    current_block: Block,
}

unsafe impl<R: Read + Seek + Send> Send for File<R> {}

unsafe impl<R: Read + Seek + Sync> Sync for File<R> {}

impl<R> Drop for File<R> {
    fn drop(&mut self) {
        unsafe {
            ffi::mp3dec_ex_close(&mut self.decoder);
        }
    }
}

unsafe extern "C" fn read_cb<R>(buf: *mut c_void, size: usize, user_data: *mut c_void) -> usize
where
    R: Read,
{
    let buf = buf as *mut u8;
    assert!(!user_data.is_null());
    let reader = &mut *(user_data as *mut R);
    let buffer = std::slice::from_raw_parts_mut(buf, size);
    loop {
        match reader.read(buffer) {
            Ok(bytes) => return bytes,
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
            Err(_) => {
                errno::set_errno(EIO);
                return 0;
            }
        }
    }
}

unsafe extern "C" fn seek_cb<R>(position: u64, user_data: *mut c_void) -> c_int
where
    R: Seek,
{
    assert!(!user_data.is_null());
    let reader = &mut *(user_data as *mut R);
    let result = reader.seek(io::SeekFrom::Start(position));
    // NB: we have to return 0 on success
    result.map(|_| 0).unwrap_or(-1)
}

impl<R> File<R>
where
    R: Read + Seek,
{
    pub fn new(reader: R) -> Result<File<R>, OpenError> {
        let reader = Box::new(reader);
        let mut decoder = MaybeUninit::<ffi::mp3dec_ex_t>::uninit();
        let ptr_to_reader = &*reader as *const R as *mut c_void;
        let mut io = ffi::mp3dec_io_t {
            read: Some(read_cb::<R>),
            seek: Some(seek_cb::<R>),
            read_data: ptr_to_reader,
            seek_data: ptr_to_reader,
        };
        let res = unsafe {
            ffi::mp3dec_ex_open_cb(decoder.as_mut_ptr(), &mut io, ffi::MP3D_SEEK_TO_SAMPLE)
        };
        if res != 0 {
            return Err(MiniMp3Error(res).into());
        }
        let decoder = unsafe { decoder.assume_init() };
        let channels = decoder.info.channels as u32;
        Ok(File {
            reader,
            decoder,
            current_block: Block {
                ptr: std::ptr::null_mut(),
                frames: 0,
                channels: (0..channels)
                    .map(|_| Channel {
                        ptr: std::ptr::null_mut(),
                        len: 0,
                        step: channels,
                    })
                    .collect(),
            },
        })
    }
}

impl<R> super::AudioFileBasics for File<R>
where
    R: Read + Seek,
{
    fn seek(&mut self, frame: u64) -> Result<(), BoxedError> {
        let res = unsafe { ffi::mp3dec_ex_seek(&mut self.decoder, frame * self.channels() as u64) };
        if res == 0 {
            Ok(())
        } else {
            Err(SeekError(MiniMp3Error(res)).into())
        }
    }

    fn samplerate(&self) -> u32 {
        self.decoder.info.hz as u32
    }

    fn channels(&self) -> u32 {
        self.decoder.info.channels as u32
    }

    fn frames(&self) -> u64 {
        self.decoder.samples / self.decoder.info.channels as u64
    }
}

impl<R> super::AudioFileBlocks for File<R>
where
    R: Read + Seek,
{
    type Block = Block;

    fn next_block(&mut self, max_frames: u32) -> Result<&mut Block, BoxedError> {
        let samples = unsafe {
            ffi::mp3dec_ex_read_frame(
                &mut self.decoder,
                &mut self.current_block.ptr,
                (max_frames * self.channels()) as usize,
            ) as u32
        };
        if samples == 0 && self.decoder.last_error != 0 {
            return Err(ReadError(MiniMp3Error(self.decoder.last_error)).into());
        }
        if samples % self.channels() != 0 {
            return Err(ModuloError.into());
        }
        self.current_block.frames = samples / self.channels();
        Ok(&mut self.current_block)
    }
}

pub struct Block {
    ptr: *mut f32,
    frames: u32,
    channels: Box<[Channel]>,
}

impl super::Block for Block {
    type Channel = Channel;

    fn channel_iterators(&mut self) -> &mut [Channel] {
        for i in 0..self.channels.len() {
            let channel = &mut self.channels[i];
            channel.ptr = unsafe { self.ptr.add(i) };
            channel.len = self.frames;
        }
        &mut self.channels
    }

    fn frames(&self) -> u32 {
        self.frames
    }
}

pub struct Channel {
    ptr: *mut f32,
    step: u32,
    len: u32,
}

impl Iterator for Channel {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        if self.len == 0 {
            None
        } else {
            let value = unsafe { *self.ptr };
            self.len -= 1;
            self.ptr = unsafe { self.ptr.add(self.step as usize) };
            Some(value)
        }
    }

    // TODO: size_hint()?
}

#[derive(thiserror::Error, Debug)]
#[error("{}", match *.0 {
    ffi::MP3D_E_PARAM => "MP3D_E_PARAM",
    ffi::MP3D_E_MEMORY => "MP3D_E_MEMORY",
    ffi::MP3D_E_IOERROR => "MP3D_E_IOERROR",
    ffi::MP3D_E_USER => "MP3D_E_USER", // this should never reach users?
    ffi::MP3D_E_DECODE => "MP3D_E_DECODE",
    _ => "unknown error",
})]
pub struct MiniMp3Error(pub c_int);

#[derive(thiserror::Error, Debug)]
#[error("error loading MP3 file: {}", .0)]
pub struct OpenError(#[from] MiniMp3Error);

#[derive(thiserror::Error, Debug)]
#[error("error seeking in MP3 file: {}", .0)]
pub struct SeekError(MiniMp3Error);

#[derive(thiserror::Error, Debug)]
#[error("error reading MP3 data: {}", .0)]
pub struct ReadError(MiniMp3Error);

#[derive(thiserror::Error, Debug)]
#[error("error reading MP3 data: samples not divisible by channels")]
pub struct ModuloError;
