use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::num::NonZeroU64;
use std::path::{Path, PathBuf};

use crate::error::{FromSourceAndContext, ResultExt};

use super::converter;
use super::vorbis;
use super::wav;
use super::{AudioFileBasics, AudioFileBlocks, RepeatedAudioFile};

/// Can be used with dynamic dispatch
pub trait AudioFile: AudioFileBasics {
    fn fill_channels(
        &mut self,
        channel_map: &[Option<usize>],
        blocksize: u32,
        offset: u32,
        channels: &mut [Box<[f32]>],
    ) -> Result<(), Box<dyn Error + Send + Sync>>;
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
    ) -> Result<(), Box<dyn Error + Send + Sync>> {
        self.fill_channels(channel_map, blocksize, offset, channels)
    }
}

#[derive(Debug)]
pub struct LoadError {
    path: PathBuf,
    source: LoadErrorKind,
}

#[derive(Debug)]
enum LoadErrorKind {
    Open(io::Error),
    Decode {
        vorbis_error: vorbis::OpenError,
        wav_error: hound::Error,
    },
    Resample(converter::LibSamplerateError),
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LoadErrorKind::*;
        match &self.source {
            Open(e) => write!(f, "Error opening {:?}: {}", self.path, e),
            Decode {
                vorbis_error,
                wav_error,
            } => write!(
                f,
                "Error decoding {:?}:\nTrying Vorbis: {}\nTrying WAV: {}",
                self.path, vorbis_error, wav_error
            ),
            Resample(e) => write!(f, "Error parsing {:?}: {}", self.path, e),
        }
    }
}

impl Error for LoadError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        use LoadErrorKind::*;
        match self.source {
            Open(ref e) => Some(e),
            Decode { .. } => None, // We have multiple underlying errors, TODO: ?
            Resample(ref e) => Some(e),
        }
    }
}

impl FromSourceAndContext<io::Error, &Path> for LoadError {
    fn from_source_and_context(source: io::Error, context: &Path) -> LoadError {
        LoadError {
            path: context.into(),
            source: LoadErrorKind::Open(source),
        }
    }
}

impl FromSourceAndContext<converter::LibSamplerateError, &Path> for LoadError {
    fn from_source_and_context(source: converter::LibSamplerateError, context: &Path) -> LoadError {
        LoadError {
            path: context.into(),
            source: LoadErrorKind::Resample(source),
        }
    }
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
        return Err(LoadError {
            path: path.into(),
            source: LoadErrorKind::Open(io::Error::new(
                io::ErrorKind::Other,
                "Path is a directory, not a file",
            )),
        });
    }

    let file = fs::File::open(path).context(path)?;
    let vorbis_error = match vorbis::File::new(file) {
        Ok(file) => {
            return Ok(repeat_and_convert(file, iterations, samplerate).context(path)?);
        }
        Err(e) => e,
    };

    let file = fs::File::open(path).context(path)?;
    let reader = io::BufReader::new(file);
    let wav_error = match wav::File::new(reader) {
        Ok(file) => {
            return Ok(repeat_and_convert(file, iterations, samplerate).context(path)?);
        }
        Err(e) => e,
    };

    // TODO: try more file types (FLAC, mp3, ...)

    Err(LoadError {
        path: path.into(),
        source: LoadErrorKind::Decode {
            vorbis_error,
            wav_error,
        },
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
