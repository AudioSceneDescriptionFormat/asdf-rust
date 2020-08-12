use std::fs;
use std::io;
use std::num::NonZeroU64;
use std::path::Path;

use super::converter;
use super::vorbis;
use super::wav;
use super::{AudioFileBasics, AudioFileBlocks, BoxedError, RepeatedAudioFile};

/// Can be used with dynamic dispatch
pub trait AudioFile: AudioFileBasics {
    fn fill_channels(
        &mut self,
        channel_map: &[Option<usize>],
        blocksize: u32,
        offset: u32,
        channels: &mut [Box<[f32]>],
    ) -> Result<(), BoxedError>;
}

impl<B, F> AudioFile for F
where
    B: super::Block,
    F: AudioFileBlocks<Block = B> + AudioFileBasics,
{
    // This is a non-generic version of AudioFileBlocks::fill_channels():
    fn fill_channels(
        &mut self,
        channel_map: &[Option<usize>],
        blocksize: u32,
        offset: u32,
        channels: &mut [Box<[f32]>],
    ) -> Result<(), BoxedError> {
        self.fill_channels(channel_map, blocksize, offset, channels)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LoadError {
    #[error("I/O error")]
    Open(#[from] io::Error),
    #[error("error decoding file:\ntrying Vorbis: {vorbis_error}\ntrying WAV: {wav_error}")]
    Decode {
        vorbis_error: vorbis::OpenError,
        wav_error: hound::Error,
    },
    #[error("libsamplerate error")]
    Resample(#[from] converter::LibSamplerateError),
}

pub fn load_audio_file<P>(
    path: P,
    samplerate: u32,
    iterations: NonZeroU64,
) -> Result<Box<dyn AudioFile + Send + Sync>, LoadError>
where
    P: AsRef<Path>,
{
    let path = path.as_ref();

    // TODO: canonicalize for nicer error messages?
    //path = path.canonicalize();

    // TODO: check if file exists (for nicer error message)? path.is_file()

    if path.is_dir() {
        return Err(LoadError::Open(io::Error::new(
            io::ErrorKind::Other,
            "path is a directory, not a file",
        )));
    }

    let file = fs::File::open(path)?;
    let vorbis_error = match vorbis::File::new(file) {
        Ok(file) => {
            return Ok(repeat_and_convert(file, iterations, samplerate)?);
        }
        Err(e) => e,
    };

    let file = fs::File::open(path)?;
    let reader = io::BufReader::new(file);
    let wav_error = match wav::File::new(reader) {
        Ok(file) => {
            return Ok(repeat_and_convert(file, iterations, samplerate)?);
        }
        Err(e) => e,
    };

    // TODO: try more file types (FLAC, mp3, ...)

    Err(LoadError::Decode {
        vorbis_error,
        wav_error,
    })
}

fn repeat_and_convert<F>(
    file: F,
    iterations: NonZeroU64,
    samplerate: u32,
) -> Result<Box<dyn AudioFile + Send + Sync>, converter::LibSamplerateError>
where
    F: AudioFileBasics + AudioFileBlocks + Send + Sync + 'static,
{
    if iterations.get() == 1 {
        if file.samplerate() == samplerate {
            Ok(Box::new(file))
        } else {
            Ok(Box::new(converter::Converter::new(file, samplerate)?))
        }
    } else {
        let file = RepeatedAudioFile::new(file, iterations);
        if file.samplerate() == samplerate {
            Ok(Box::new(file))
        } else {
            Ok(Box::new(converter::Converter::new(file, samplerate)?))
        }
    }
}
