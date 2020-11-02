use std::fs;
use std::io;
use std::path::Path;

use super::{converter, flac, mp3, vorbis, wav};
use super::{AudioFileBasics, AudioFileBlocks, BoxedError, RepeatedAudioFile};
use crate::parser::Iterations;

/// Can be used with dynamic dispatch
pub trait AudioFile: AudioFileBasics {
    fn fill_channels(
        &mut self,
        channel_map: &[Option<usize>],
        blocksize: u32,
        offset: u32,
        channels: &mut [&mut [f32]],
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
        channels: &mut [&mut [f32]],
    ) -> Result<(), BoxedError> {
        self.fill_channels(channel_map, blocksize, offset, channels)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LoadError {
    #[error("I/O error")]
    Open(#[from] io::Error),
    #[error(
        "error decoding file:\n\
        trying Vorbis: {vorbis_error}\n\
        trying WAV: {wav_error}\n\
        trying FLAC: {flac_error}\n\"
        trying MP3: {mp3_error}"
    )]
    Decode {
        vorbis_error: vorbis::OpenError,
        wav_error: hound::Error,
        flac_error: flac::OpenError,
        mp3_error: mp3::OpenError,
    },
    #[error("libsamplerate error")]
    Resample(#[from] converter::LibSamplerateError),
}

pub fn load_audio_file<P>(
    path: P,
    samplerate: u32,
    iterations: Iterations,
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

    let file = fs::File::open(path)?;
    let flac_error = match flac::File::new(file) {
        Ok(file) => {
            return Ok(repeat_and_convert(file, iterations, samplerate)?);
        }
        Err(e) => e,
    };

    let file = fs::File::open(path)?;
    let mp3_error = match mp3::File::new(file) {
        Ok(file) => {
            return Ok(repeat_and_convert(file, iterations, samplerate)?);
        }
        Err(e) => e,
    };

    Err(LoadError::Decode {
        vorbis_error,
        wav_error,
        flac_error,
        mp3_error,
    })
}

fn repeat_and_convert<F>(
    file: F,
    iterations: Iterations,
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
