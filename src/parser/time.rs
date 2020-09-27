use std::ops::{Add, AddAssign};
use std::str::FromStr;

#[derive(Clone, Copy, Default, PartialEq, PartialOrd)]
pub struct Seconds(pub f32);

#[derive(thiserror::Error, Debug)]
#[error("error parsing time string")]
pub struct ParseSecondsError(#[from] std::num::ParseFloatError);

impl FromStr for Seconds {
    type Err = ParseSecondsError;

    fn from_str(s: &str) -> Result<Self, ParseSecondsError> {
        // TODO: support more syntax
        Ok(Seconds(f32::from_str(s)?))
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

/// Time value used in ASDF files.
///
/// Either given in seconds or in a fraction of its parent duration.
#[derive(Clone, Copy)]
pub enum XmlTime {
    Seconds(Seconds),
    Fraction(f32),
}

impl XmlTime {
    pub fn resolve(self, total: Seconds) -> Seconds {
        match self {
            XmlTime::Seconds(s) => s,
            XmlTime::Fraction(f) => Seconds(f * total.0),
        }
    }
}

impl FromStr for XmlTime {
    type Err = ParseXmlTimeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim_end();

        if let Some(s) = s.strip_suffix("%") {
            let f = f32::from_str(s)? / 100.0;
            // TODO: check valid range (>= 0; <= 100?)
            Ok(XmlTime::Fraction(f))
        } else {
            let time = Seconds::from_str(s)?;
            if time < Seconds(0.0) {
                return Err(ParseXmlTimeError::NegativeSeconds);
            }
            Ok(XmlTime::Seconds(time))
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParseXmlTimeError {
    #[error("error parsing time percentage")]
    Percentage(#[from] std::num::ParseFloatError),
    #[error("error parsing time string")]
    Seconds(#[from] ParseSecondsError),
    #[error("negative time values are not allowed")]
    NegativeSeconds,
}
