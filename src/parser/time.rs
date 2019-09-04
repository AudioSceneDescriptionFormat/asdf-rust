use std::ops::{Add, AddAssign};
use std::str::FromStr;

use super::error::ParseSecondsError;

#[derive(Clone, Copy, Default, PartialEq)]
pub struct Seconds(pub f32);

impl FromStr for Seconds {
    type Err = ParseSecondsError;

    fn from_str(s: &str) -> Result<Self, ParseSecondsError> {
        // TODO: support more syntax
        match f32::from_str(s) {
            Ok(value) => Ok(Seconds(value)),
            // TODO: add error info
            Err(_) => Err(ParseSecondsError {}),
        }
    }
}

impl Add for Seconds {
    type Output = Seconds;

    fn add(mut self, rhs: Seconds) -> Seconds {
        self += rhs;
        self
    }
}

impl AddAssign for Seconds {
    fn add_assign(&mut self, rhs: Seconds) {
        self.0 += rhs.0;
    }
}

pub fn seconds2frames(time: Seconds, samplerate: u32) -> u64 {
    assert!(samplerate > 0);
    (time.0 * samplerate as f32) as u64
}

pub fn frames2seconds(frames: u64, samplerate: u32) -> Seconds {
    assert!(samplerate > 0);
    Seconds(frames as f32 / samplerate as f32)
}
