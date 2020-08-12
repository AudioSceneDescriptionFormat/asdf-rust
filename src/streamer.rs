use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::thread;
use std::time::Duration;

use crossbeam::queue;

use crate::audiofile::BoxedError;
use crate::parser::{FileStorage, PlaylistEntry};

enum Fade {
    In,
    Out,
    None,
}

struct Block {
    channels: Box<[Box<[f32]>]>,
}

impl Block {
    fn new(frames: u32, channels: u32) -> Block {
        Block {
            channels: (0..channels)
                .map(|_| (0..frames).map(|_| 0.0f32).collect())
                .collect(),
        }
    }
}

struct DataProducer {
    data_producer: queue::spsc::Producer<Block>,
    recycling_consumer: queue::spsc::Consumer<Block>,
}

struct DataConsumer {
    blocksize: u32,
    data_consumer: queue::spsc::Consumer<Block>,
    recycling_producer: queue::spsc::Producer<Block>,
}

fn make_data_queue(capacity: usize, blocksize: u32, channels: u32) -> (DataProducer, DataConsumer) {
    let (data_producer, data_consumer) = queue::spsc::new(capacity);
    let (recycling_producer, recycling_consumer) = queue::spsc::new(capacity);
    for _ in 0..capacity {
        recycling_producer
            .push(Block::new(blocksize, channels))
            .unwrap();
    }
    (
        DataProducer {
            data_producer,
            recycling_consumer,
        },
        DataConsumer {
            blocksize,
            data_consumer,
            recycling_producer,
        },
    )
}

struct WriteBlock<'b> {
    // NB: Option in order to be able to move Block in drop()
    block: Option<Block>,
    queue: &'b mut queue::spsc::Producer<Block>,
}

impl<'b> Drop for WriteBlock<'b> {
    fn drop(&mut self) {
        if let Some(block) = self.block.take() {
            self.queue.push(block).unwrap();
        }
    }
}

impl<'b> WriteBlock<'b> {
    fn channels(&mut self) -> &mut [Box<[f32]>] {
        &mut self.block.as_mut().unwrap().channels
    }
}

impl DataProducer {
    fn write_block(&mut self) -> Option<WriteBlock> {
        let mut block = match self.recycling_consumer.pop() {
            Ok(block) => block,
            _ => return None,
        };

        // TODO: avoid filling everything with zeros?
        for channel in block.channels.iter_mut() {
            for value in channel.iter_mut() {
                *value = 0.0f32;
            }
        }
        Some(WriteBlock {
            block: Some(block),
            queue: &mut self.data_producer,
        })
    }
}

impl DataConsumer {
    fn clear(&mut self) {
        while let Ok(data) = self.data_consumer.pop() {
            self.recycling_producer.push(data).unwrap()
        }
    }

    /// Return value of `false` means un-recoverable error (but output buffer is still filled)
    #[must_use]
    unsafe fn write_channel_ptrs(&mut self, target: &[*mut f32], fade: Fade) -> bool {
        if let Ok(block) = self.data_consumer.pop() {
            for (source, &target) in block.channels.iter().zip(target) {
                match fade {
                    Fade::In => {
                        for i in 0..self.blocksize {
                            *target.add(i as usize) =
                                source[i as usize] * (i + 1) as f32 / self.blocksize as f32;
                        }
                    }
                    Fade::Out => {
                        for i in 0..self.blocksize {
                            *target.add(i as usize) = source[i as usize]
                                * (self.blocksize - i) as f32
                                / self.blocksize as f32;
                        }
                    }
                    Fade::None => {
                        let target =
                            std::slice::from_raw_parts_mut(target, self.blocksize as usize);
                        target.copy_from_slice(source);
                    }
                }
            }
            self.recycling_producer.push(block).unwrap();
            true
        } else {
            fill_with_zeros(target, self.blocksize);
            false
        }
    }
}

pub struct FileStreamer {
    ready_consumer: queue::spsc::Consumer<(u64, DataConsumer)>,
    seek_producer: queue::spsc::Producer<(u64, DataConsumer)>,
    data_consumer: Option<DataConsumer>,
    reader_thread: Option<thread::JoinHandle<Result<(), BoxedError>>>,
    reader_thread_keep_reading: Arc<AtomicBool>,
    channels: u32,
    blocksize: u32,
    previously_rolling: bool,
    seek_frame: Option<u64>,
}

struct ActiveIter<'a> {
    block_start: u64,
    block_end: u64,
    inner: std::slice::IterMut<'a, PlaylistEntry>,
}

impl<'a> Iterator for ActiveIter<'a> {
    type Item = &'a mut PlaylistEntry;

    fn next(&mut self) -> Option<&'a mut PlaylistEntry> {
        while let Some(entry) = self.inner.next() {
            if entry.begin < self.block_end && self.block_start < (entry.begin + entry.duration) {
                return Some(entry);
            }
        }
        None
    }
}

impl FileStreamer {
    pub fn new(
        mut playlist: Vec<PlaylistEntry>,
        mut file_storage: FileStorage,
        blocksize: u32,
        channels: u32,
        buffer_blocks: u32,
        sleeptime: Duration,
    ) -> FileStreamer {
        let (ready_producer, ready_consumer) = queue::spsc::new(1);
        let (seek_producer, seek_consumer) = queue::spsc::new::<(u64, DataConsumer)>(1);
        let (mut data_producer, data_consumer) =
            make_data_queue(buffer_blocks as usize, blocksize, channels);
        let reader_thread_keep_reading = Arc::new(AtomicBool::new(true));
        let keep_reading = Arc::clone(&reader_thread_keep_reading);
        let reader_thread = thread::spawn(move || {
            let mut data_consumer = Some(data_consumer);
            let mut current_frame = 0;
            let mut seek_frame = 0;

            while keep_reading.load(Ordering::Acquire) {
                if let Ok((frame, mut queue)) = seek_consumer.pop() {
                    queue.clear();
                    data_consumer = Some(queue);
                    current_frame = frame;
                    seek_frame = frame;
                }
                let mut block = match data_producer.write_block() {
                    Some(block) => block,
                    None => {
                        thread::sleep(sleeptime);
                        continue;
                    }
                };
                let mut active_files = ActiveIter {
                    block_start: current_frame,
                    block_end: current_frame + u64::from(blocksize),
                    inner: playlist.iter_mut(),
                };
                // TODO: Is linear search too slow? How long can playlists be?
                for entry in &mut active_files {
                    let (file, channel_map) = &mut file_storage[entry.idx];
                    let offset = if entry.begin < current_frame {
                        if current_frame == seek_frame {
                            file.seek(current_frame - entry.begin)?;
                        }
                        0
                    } else {
                        file.seek(0)?;
                        (entry.begin - current_frame) as u32
                    };
                    file.fill_channels(&channel_map, blocksize, offset, block.channels())?;
                }
                current_frame += u64::from(blocksize);

                // Make sure the block is queued before data_consumer is sent
                drop(block);

                if current_frame - seek_frame >= u64::from(buffer_blocks) * u64::from(blocksize) {
                    if let Some(data_consumer) = data_consumer.take() {
                        // There is only one data queue, push() will always succeed
                        ready_producer.push((seek_frame, data_consumer)).unwrap();
                    }
                }
            }
            Ok(())
        });
        FileStreamer {
            ready_consumer,
            seek_producer,
            data_consumer: None,
            reader_thread: Some(reader_thread),
            reader_thread_keep_reading,
            channels,
            blocksize,
            previously_rolling: false,
            seek_frame: None,
        }
    }

    pub fn channels(&self) -> u32 {
        self.channels
    }

    // TODO: more information on error, use Result?
    /// Return value of `false` means un-recoverable error
    #[must_use]
    pub unsafe fn get_data(&mut self, target: &[*mut f32], rolling: bool) -> bool {
        // TODO: Check if disk thread is still running? return false if not?

        let previously = self.previously_rolling;
        let result = if !rolling && !previously {
            fill_with_zeros(target, self.blocksize);
            true
        } else if let Some(ref mut queue) = self.data_consumer {
            let fade = if rolling && !previously {
                Fade::In
            } else if !rolling && previously {
                Fade::Out
            } else {
                Fade::None
            };
            queue.write_channel_ptrs(target, fade)
        } else {
            fill_with_zeros(target, self.blocksize);
            false
        };
        // NB: This has to be updated before seeking:
        self.previously_rolling = rolling;
        if let Some(frame) = self.seek_frame.take() {
            if rolling {
                // NB: Seeking while rolling is not supported
                return false;
            }
            let _ = self.seek(frame);
        }
        result
    }

    #[must_use]
    pub fn seek(&mut self, frame: u64) -> bool {
        // TODO: Check if disk thread is still running? What if not?

        if self.previously_rolling {
            self.seek_frame = Some(frame);
            // Don't seek yet; get_data() fades out and calls seek afterwards
            return false;
        }
        if self.data_consumer.is_none() {
            // NB: There can never be more than one message
            if let Ok((ready_frame, queue)) = self.ready_consumer.pop() {
                self.data_consumer = Some(queue);
                if ready_frame == frame {
                    return true;
                }
            }
        }
        if let Some(queue) = self.data_consumer.take() {
            self.seek_producer.push((frame, queue)).unwrap();
        }
        false
    }
}

impl Drop for FileStreamer {
    fn drop(&mut self) {
        self.reader_thread_keep_reading
            .store(false, Ordering::Release);
        // TODO: handle error from closure? log errors?
        self.reader_thread.take().unwrap().join().unwrap().unwrap();
    }
}

unsafe fn fill_with_zeros(target: &[*mut f32], blocksize: u32) {
    for ptr in target {
        for f in 0..blocksize {
            *ptr.add(f as usize) = 0.0f32;
        }
    }
}
