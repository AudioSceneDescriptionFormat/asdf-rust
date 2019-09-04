use std::error::Error;
use std::io::{Read, Seek};

pub struct File<R>
where
    R: Read + Seek,
{
    reader: hound::WavReader<R>,
    // NB: No dynamic memory is allocated when using zero-sized types (which we do)
    block_reader: Box<dyn BlockReader<R>>,
    current_block: Block,
}

unsafe impl<R: Read + Seek + Send> Send for File<R> {}

unsafe impl<R: Read + Seek + Sync> Sync for File<R> {}

impl<R> File<R>
where
    R: Read + Seek,
{
    pub fn new(reader: R) -> Result<File<R>, hound::Error> {
        // TODO: same buffer size as Converter?
        let buffer_size = 2048;

        // TODO: channel selection?

        let reader = hound::WavReader::new(reader)?;
        let spec = reader.spec();
        Ok(File {
            reader,
            block_reader: {
                use hound::SampleFormat::{Float, Int};
                match (spec.sample_format, spec.bits_per_sample) {
                    (Float, 32) => Box::new(FloatFormat),
                    (Int, 16) => Box::new(Pcm16Format),
                    // TODO:
                    (Int, bits) => unimplemented!("{} bit PCM", bits),
                    (Float, bits) => unimplemented!("{} bit FLOAT", bits),
                }
            },
            current_block: Block {
                channels: (0..spec.channels)
                    .map(|_| Channel {
                        data: (0..buffer_size).map(|_| 0.0f32).collect(),
                        index: 0,
                        stop: 0,
                    })
                    .collect(),
                len_frames: 0,
                capacity_frames: buffer_size,
            },
        })
    }
}

impl<R> super::AudioFileBasics for File<R>
where
    R: Read + Seek,
{
    fn channels(&self) -> u32 {
        self.current_block.channels.len() as u32
    }

    fn frames(&self) -> u64 {
        self.reader.duration().into()
    }

    fn samplerate(&self) -> u32 {
        self.reader.spec().sample_rate
    }

    fn seek(&mut self, frame: u64) -> Result<(), Box<dyn Error + Send + Sync>> {
        Ok(self.reader.seek(frame as u32)?)
    }
}

trait BlockReader<R>
where
    R: Read + Seek,
{
    fn next_sample(&self, reader: &mut hound::WavReader<R>) -> Option<hound::Result<f32>>;

    fn fill_block(
        &self,
        mut reader: &mut hound::WavReader<R>,
        block: &mut Block,
        max_frames: u32,
    ) -> hound::Result<()> {
        // TODO: channel selection
        let max_frames = std::cmp::min(max_frames, block.capacity_frames);
        let mut frame = 0;
        'outer: while frame < max_frames {
            for channel in block.channels.iter_mut() {
                if let Some(sample) = self.next_sample(&mut reader) {
                    channel.data[frame as usize] = sample?;
                } else {
                    // This should only ever happen in the first channel, but we don't check this!
                    break 'outer;
                }
            }
            frame += 1;
        }
        for channel in block.channels.iter_mut() {
            channel.index = 0;
            channel.stop = frame;
        }
        block.len_frames = frame;
        Ok(())
    }
}

struct FloatFormat;

impl<R> BlockReader<R> for FloatFormat
where
    R: Read + Seek,
{
    fn next_sample(&self, reader: &mut hound::WavReader<R>) -> Option<hound::Result<f32>>
    where
        R: Read,
    {
        reader.samples::<f32>().next()
    }
}

// TODO: Pcm8Format
// TODO: Pcm24Format
// TODO: Pcm32Format

struct Pcm16Format;

impl<R> BlockReader<R> for Pcm16Format
where
    R: Read + Seek,
{
    fn next_sample(&self, reader: &mut hound::WavReader<R>) -> Option<hound::Result<f32>>
    where
        R: Read,
    {
        // TODO: off-by-one? use max_value - 1?
        reader
            .samples::<i16>()
            .next()
            .map(|result| result.map(|sample| f32::from(sample) / f32::from(i16::max_value())))
    }
}

impl<R> super::AudioFileBlocks for File<R>
where
    R: Read + Seek,
{
    type Block = Block;

    fn next_block(&mut self, max_frames: u32) -> Result<&mut Block, Box<dyn Error + Send + Sync>> {
        // Dynamic dispatch based on sample format (FloatFormat, Pcm16Format, etc.):
        self.block_reader
            .fill_block(&mut self.reader, &mut self.current_block, max_frames)?;

        // TODO: channel selection?

        Ok(&mut self.current_block)
    }
}

pub struct Block {
    channels: Box<[Channel]>,
    len_frames: u32,
    capacity_frames: u32,
}

impl super::Block for Block {
    type Channel = Channel;

    fn channel_iterators(&mut self) -> &mut [Channel] {
        &mut self.channels
    }

    fn frames(&self) -> u32 {
        self.len_frames
    }
}

pub struct Channel {
    data: Box<[f32]>,
    index: u32,
    stop: u32,
}

impl Iterator for Channel {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        if self.index == self.stop {
            None
        } else {
            let value = self.data[self.index as usize];
            self.index += 1;
            Some(value)
        }
    }

    // TODO: size_hint()?
}
