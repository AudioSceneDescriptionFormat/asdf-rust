/// https://xiph.org/flac/api/group__flac__stream__decoder.html
use std::io::{self, Read, Seek};

use libc::c_void;

use libflac_sys as ffi;

use super::BoxedError;

struct ClientData<R> {
    reader: R,
    buffer: *const *const ffi::FLAC__int32,
    blocksize: u32,
    samplerate: u32,
    channels: u32,
    channel_assignment: ffi::FLAC__ChannelAssignment,
    bits_per_sample: u32,
    last_error_status: Option<ffi::FLAC__StreamDecoderErrorStatus>,
}

pub struct File<R> {
    samplerate: u32,
    channels: u32,
    frames: u64,
    client_data: Box<ClientData<R>>,
    current_block: Block,
    decoder: *mut ffi::FLAC__StreamDecoder,
}

unsafe impl<R: Read + Seek + Send> Send for File<R> {}

unsafe impl<R: Read + Seek + Sync> Sync for File<R> {}

impl<R> Drop for File<R> {
    fn drop(&mut self) {
        unsafe {
            ffi::FLAC__stream_decoder_finish(self.decoder);
            ffi::FLAC__stream_decoder_delete(self.decoder);
        }
    }
}

unsafe extern "C" fn read_callback<R>(
    _decoder: *const ffi::FLAC__StreamDecoder,
    buffer: *mut ffi::FLAC__byte,
    bytes: *mut libc::size_t,
    client_data: *mut c_void,
) -> ffi::FLAC__StreamDecoderReadStatus
where
    R: Read,
{
    assert!(!client_data.is_null());
    let client_data = &mut *(client_data as *mut ClientData<R>);
    assert!(!bytes.is_null());
    let size = *bytes;
    let buffer = std::slice::from_raw_parts_mut(buffer, size);
    loop {
        match client_data.reader.read(buffer) {
            Ok(read_bytes) => {
                *bytes = read_bytes;
                if read_bytes == 0 {
                    assert!(size != 0);
                    return ffi::FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
                } else {
                    return ffi::FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {}
            Err(_) => {
                // TODO: store error message somewhere?
                return ffi::FLAC__STREAM_DECODER_READ_STATUS_ABORT;
            }
        }
    }
}

unsafe extern "C" fn seek_callback<R>(
    _decoder: *const ffi::FLAC__StreamDecoder,
    absolute_byte_offset: ffi::FLAC__uint64,
    client_data: *mut c_void,
) -> ffi::FLAC__StreamDecoderSeekStatus
where
    R: Seek,
{
    assert!(!client_data.is_null());
    let client_data = &mut *(client_data as *mut ClientData<R>);
    match client_data
        .reader
        .seek(io::SeekFrom::Start(absolute_byte_offset))
    {
        Ok(position) => {
            assert!(position == absolute_byte_offset);
            ffi::FLAC__STREAM_DECODER_SEEK_STATUS_OK
        }
        Err(_) => {
            // TODO: store error somewhere?
            ffi::FLAC__STREAM_DECODER_SEEK_STATUS_ERROR
        }
    }
}

unsafe extern "C" fn tell_callback<R>(
    _decoder: *const ffi::FLAC__StreamDecoder,
    absolute_byte_offset: *mut ffi::FLAC__uint64,
    client_data: *mut c_void,
) -> ffi::FLAC__StreamDecoderTellStatus
where
    R: Seek,
{
    assert!(!client_data.is_null());
    let client_data = &mut *(client_data as *mut ClientData<R>);
    // NB: unstable has stream_position() with feature(seek_convenience)
    match client_data.reader.stream_position() {
        Ok(position) => {
            *absolute_byte_offset = position;
            ffi::FLAC__STREAM_DECODER_TELL_STATUS_OK
        }
        Err(_) => {
            // TODO: store error somewhere?
            ffi::FLAC__STREAM_DECODER_TELL_STATUS_ERROR
        }
    }
}

unsafe extern "C" fn length_callback<R>(
    _decoder: *const ffi::FLAC__StreamDecoder,
    stream_length: *mut ffi::FLAC__uint64,
    client_data: *mut c_void,
) -> ffi::FLAC__StreamDecoderLengthStatus
where
    R: Seek,
{
    assert!(!client_data.is_null());
    let client_data = &mut *(client_data as *mut ClientData<R>);
    // NB: unstable has stream_len() with feature(seek_convenience)
    match client_data.reader.stream_position().and_then(|current| {
        client_data
            .reader
            .seek(io::SeekFrom::End(0))
            .and_then(|length| {
                *stream_length = length;
                client_data.reader.seek(io::SeekFrom::Start(current))
            })
    }) {
        Ok(_) => ffi::FLAC__STREAM_DECODER_LENGTH_STATUS_OK,
        Err(_) => ffi::FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR,
    }
}

unsafe extern "C" fn eof_callback<R>(
    _decoder: *const ffi::FLAC__StreamDecoder,
    client_data: *mut c_void,
) -> ffi::FLAC__bool
where
    R: Seek,
{
    assert!(!client_data.is_null());
    let client_data = &mut *(client_data as *mut ClientData<R>);
    let is_eof = client_data
        .reader
        .stream_position()
        .and_then(|current| {
            client_data
                .reader
                .seek(io::SeekFrom::End(0))
                .and_then(|length| {
                    client_data
                        .reader
                        .seek(io::SeekFrom::Start(current))
                        .map(|_| current >= length)
                })
        })
        .unwrap_or(false);
    if is_eof {
        true.into()
    } else {
        false.into()
    }
}

unsafe extern "C" fn error_callback<R>(
    _decoder: *const ffi::FLAC__StreamDecoder,
    status: ffi::FLAC__StreamDecoderErrorStatus,
    client_data: *mut c_void,
) {
    assert!(!client_data.is_null());
    let client_data = &mut *(client_data as *mut ClientData<R>);
    client_data.last_error_status = Some(status);
}

unsafe extern "C" fn write_callback<R>(
    _decoder: *const ffi::FLAC__StreamDecoder,
    frame: *const ffi::FLAC__Frame,
    buffer: *const *const ffi::FLAC__int32,
    client_data: *mut c_void,
) -> ffi::FLAC__StreamDecoderWriteStatus
where
    R: Seek,
{
    assert!(!frame.is_null());
    let frame = &*frame;
    assert!(!client_data.is_null());
    let client_data = &mut *(client_data as *mut ClientData<R>);
    client_data.buffer = buffer;
    client_data.blocksize = frame.header.blocksize;
    client_data.samplerate = frame.header.sample_rate;
    client_data.channels = frame.header.channels;
    client_data.channel_assignment = frame.header.channel_assignment;
    client_data.bits_per_sample = frame.header.bits_per_sample;
    ffi::FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE
}

unsafe extern "C" fn metadata_callback<R>(
    _decoder: *const ffi::FLAC__StreamDecoder,
    metadata: *const ffi::FLAC__StreamMetadata,
    client_data: *mut c_void,
) {
    assert!(!metadata.is_null());
    let metadata = &*metadata;
    assert!(!client_data.is_null());
    let client_data = &mut *(client_data as *mut ClientData<R>);

    if metadata.type_ == ffi::FLAC__METADATA_TYPE_STREAMINFO {
        client_data.samplerate = metadata.data.stream_info.sample_rate;
        client_data.channels = metadata.data.stream_info.channels;
    }
}

impl<R> File<R>
where
    R: Read + Seek,
{
    pub fn new(reader: R) -> Result<File<R>, OpenError> {
        let client_data = Box::new(ClientData {
            reader,
            buffer: std::ptr::null(),
            blocksize: 0,
            channels: 0,
            samplerate: 0,
            channel_assignment: 0,
            bits_per_sample: 0,
            last_error_status: None,
        });

        let decoder = unsafe { ffi::FLAC__stream_decoder_new() };
        let status = unsafe {
            ffi::FLAC__stream_decoder_init_stream(
                decoder,
                Some(read_callback::<R>),
                Some(seek_callback::<R>),
                Some(tell_callback::<R>),
                Some(length_callback::<R>),
                Some(eof_callback::<R>),
                Some(write_callback::<R>),
                Some(metadata_callback::<R>),
                Some(error_callback::<R>),
                &*client_data as *const _ as *mut _,
            )
        };

        if status != ffi::FLAC__STREAM_DECODER_INIT_STATUS_OK {
            return Err(FlacError::new(match status {
                ffi::FLAC__STREAM_DECODER_INIT_STATUS_UNSUPPORTED_CONTAINER => {
                    "the FLAC library was not compiled with support for the given container format"
                }

                ffi::FLAC__STREAM_DECODER_INIT_STATUS_MEMORY_ALLOCATION_ERROR => {
                    "FLAC: an error occurred allocating memory"
                }
                _ => "unknown FLAC initialization error",
            })
            .into());
        }
        // TODO: FLAC__stream_decoder_init_ogg_stream()? FLAC_API_SUPPORTS_OGG_FLAC?

        let success = unsafe { ffi::FLAC__stream_decoder_process_until_end_of_metadata(decoder) };
        if success != true.into() {
            // TODO: more specific error message?
            // TODO: check state? ffi::FLAC__stream_decoder_get_state(decoder);
            return Err(FlacError::new("error reading FLAC metadata").into());
        }

        let frames = unsafe { ffi::FLAC__stream_decoder_get_total_samples(decoder) };
        let channels = client_data.channels;
        let samplerate = client_data.samplerate;

        if samplerate == 0 && channels == 0 {
            return Err(FlacError::new("no FLAC metadata found").into());
        }

        Ok(File {
            channels,
            frames,
            samplerate,
            current_block: Block {
                frames: 0,
                remaining_frames: 0,
                channels: (0..channels)
                    .map(|_| Channel {
                        ptr: std::ptr::null_mut(),
                        len: 0,
                        normalization_factor: 1.0,
                    })
                    .collect(),
                normalization_factor: 1.0,
            },
            decoder,
            client_data,
        })
    }
}

impl<R> super::AudioFileBasics for File<R>
where
    R: Read + Seek,
{
    fn seek(&mut self, frame: u64) -> Result<(), BoxedError> {
        let success = unsafe { ffi::FLAC__stream_decoder_seek_absolute(self.decoder, frame) };

        // TODO: if the decoder state is FLAC__STREAM_DECODER_SEEK_ERROR,
        // then the decoder must be flushed with FLAC__stream_decoder_flush()
        // or reset with FLAC__stream_decoder_reset() before decoding can continue.

        self.current_block.remaining_frames = 0;

        if success == true.into() {
            Ok(())
        } else {
            Err(FlacError::new("error seeking in FLAC file").into())
        }
    }

    fn samplerate(&self) -> u32 {
        self.samplerate
    }

    fn channels(&self) -> u32 {
        self.channels
    }

    fn frames(&self) -> u64 {
        self.frames
    }
}

impl<R> super::AudioFileBlocks for File<R>
where
    R: Read + Seek,
{
    type Block = Block;

    fn next_block(&mut self, max_frames: u32) -> Result<&mut Self::Block, BoxedError> {
        let block = &mut self.current_block;
        let data = &self.client_data;
        if block.remaining_frames == 0 {
            let success = unsafe { ffi::FLAC__stream_decoder_process_single(self.decoder) };
            let state = unsafe { ffi::FLAC__stream_decoder_get_state(self.decoder) };
            if success != true.into() {
                // TODO: check state? get error message from read/seek callback?
                // FLAC__STREAM_DECODER_OGG_ERROR
                // FLAC__STREAM_DECODER_SEEK_ERROR
                // FLAC__STREAM_DECODER_ABORTED
                // FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR
                return Err(FlacError::new("FLAC decoder error").into());
            }

            if state == ffi::FLAC__STREAM_DECODER_END_OF_STREAM {
                block.frames = 0;
                return Ok(block);
            }
            if data.channels != self.channels {
                return Err(FlacError::new("number of FLAC channels cannot change").into());
            }
            if data.samplerate != self.samplerate {
                return Err(FlacError::new("FLAC samplerate cannot change").into());
            }
            let bits_per_sample = data.bits_per_sample;
            assert!(bits_per_sample != 0);
            block.normalization_factor =
                -((ffi::FLAC__int32::MIN >> (32 - bits_per_sample)) as f32);
            block.remaining_frames = data.blocksize;
        }
        let start_frame = data.blocksize - block.remaining_frames;
        block.frames = block.remaining_frames.min(max_frames);
        for i in 0..block.channels.len() {
            let channel = &mut block.channels[i];
            channel.ptr = unsafe { (*data.buffer.add(i)).add(start_frame as usize) };
            channel.len = block.frames;
            channel.normalization_factor = block.normalization_factor;
        }
        block.remaining_frames -= block.frames;
        Ok(block)
    }
}

pub struct Block {
    frames: u32,
    remaining_frames: u32,
    channels: Box<[Channel]>,
    normalization_factor: f32,
}

impl super::Block for Block {
    type Channel = Channel;

    fn channel_iterators(&mut self) -> &mut [Channel] {
        &mut self.channels
    }

    fn frames(&self) -> u32 {
        self.frames
    }
}

pub struct Channel {
    ptr: *const ffi::FLAC__int32,
    len: u32,
    normalization_factor: f32,
}

impl Iterator for Channel {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        if self.len == 0 {
            None
        } else {
            let value = unsafe { *self.ptr };
            let value = value as f32 / self.normalization_factor;
            self.len -= 1;
            self.ptr = unsafe { self.ptr.add(1) };
            Some(value)
        }
    }

    // TODO: size_hint()?
}

#[derive(thiserror::Error, Debug)]
#[error("{}", .message)]
pub struct FlacError {
    message: String,
}

impl FlacError {
    pub fn new(message: impl AsRef<str>) -> Self {
        Self {
            message: message.as_ref().into(),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum OpenError {
    #[error(transparent)]
    Flac(#[from] FlacError),
    // TODO: more variants? error messages?
}
