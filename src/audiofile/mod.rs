use std::num::NonZeroU64;
use std::ops::IndexMut;

pub mod converter;
pub mod dynamic;
pub mod flac;
pub mod vorbis;
pub mod wav;

pub type BoxedError = Box<dyn std::error::Error + Send + Sync>;

pub trait AudioFileBasics {
    fn channels(&self) -> u32;
    fn frames(&self) -> u64;
    fn samplerate(&self) -> u32;
    fn seek(&mut self, frame: u64) -> Result<(), BoxedError>;
}

pub trait AudioFileBlocks {
    type Block: Block;

    fn next_block(&mut self, max_frames: u32) -> Result<&mut Self::Block, BoxedError>;

    /// Panics if `buffer` is not long enough.
    fn copy_block_to_interleaved(
        &mut self,
        max_frames: u32,
        buffer: &mut [f32],
    ) -> Result<u32, BoxedError> {
        let block = self.next_block(max_frames)?;
        let frames = block.frames();
        let iterators = block.channel_iterators();
        let channels = iterators.len();
        for frame in 0..frames {
            for channel in 0..channels {
                buffer[frame as usize * channels + channel] = iterators[channel].next().unwrap();
            }
        }
        // TODO: benchmark alternative implementation
        /*
        for (i, source) in iterators.iter_mut().enumerate() {
            let target = buffer[i..].iter_mut().step_by(channels);
            for (a, b) in source.zip(target) {
                *b = a
            }
        }
        */
        Ok(frames)
    }

    fn fill_channels<D>(
        &mut self,
        channel_map: &[Option<usize>],
        blocksize: u32,
        offset: u32,
        channels: &mut [D],
    ) -> Result<(), BoxedError>
    where
        D: std::ops::DerefMut<Target = [f32]>,
    {
        let mut offset = offset;
        while offset < blocksize {
            let file_block = self.next_block(blocksize - offset)?;
            if file_block.is_empty() {
                break;
            }
            let iterators = file_block.channel_iterators();
            // TODO: check channel_map for validity?
            for (i, &channel) in channel_map.iter().enumerate() {
                if let Some(channel) = channel {
                    // TODO: use iterators[i]?
                    for (a, b) in IndexMut::index_mut(iterators, i)
                        .zip(&mut channels[channel][offset as usize..])
                    {
                        *b = a;
                    }
                }
            }
            offset += file_block.frames();
        }
        // TODO: return number of frames?
        Ok(())
    }
}

pub trait Block {
    type Channel: Iterator<Item = f32>;
    fn channel_iterators(&mut self) -> &mut [Self::Channel];
    fn frames(&self) -> u32;
    fn is_empty(&self) -> bool {
        self.frames() == 0
    }
}

struct RepeatedAudioFile<F> {
    file: F,
    current_iteration: u64,
    iterations: NonZeroU64,
}

impl<F> RepeatedAudioFile<F> {
    fn new(file: F, iterations: NonZeroU64) -> RepeatedAudioFile<F> {
        RepeatedAudioFile {
            file,
            current_iteration: 0,
            iterations,
        }
    }
}

impl<F> AudioFileBasics for RepeatedAudioFile<F>
where
    F: AudioFileBasics,
{
    fn channels(&self) -> u32 {
        self.file.channels()
    }

    fn frames(&self) -> u64 {
        self.file.frames() * self.iterations.get()
    }

    fn samplerate(&self) -> u32 {
        self.file.samplerate()
    }

    fn seek(&mut self, frame: u64) -> Result<(), BoxedError> {
        self.current_iteration = frame / self.file.frames();
        self.file.seek(frame % self.file.frames())
    }
}

impl<F> AudioFileBlocks for RepeatedAudioFile<F>
where
    F: AudioFileBasics + AudioFileBlocks,
{
    type Block = F::Block;

    fn next_block(&mut self, max_frames: u32) -> Result<&mut Self::Block, BoxedError> {
        // This is an ugly work-around that can be removed once Polonius is stabilized.
        // http://smallcultfollowing.com/babysteps/blog/2018/06/15/mir-based-borrow-check-nll-status-update/#polonius
        // TODO: remove this pointer ugliness:
        let ptr = &mut self.file as *mut _;
        let file: &mut F = unsafe { &mut *ptr };
        let block = file.next_block(max_frames)?;
        if block.frames() > 0 || self.current_iteration >= (self.iterations.get() - 1) {
            return Ok(block);
        }
        let file: &mut F = unsafe { &mut *ptr };
        file.seek(0)?;
        self.current_iteration += 1;
        file.next_block(max_frames)
    }
}
