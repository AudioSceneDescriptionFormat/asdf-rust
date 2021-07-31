use std::ops::{Add, AddAssign};
use std::str::FromStr;

use regex::Regex;

use super::Iterations;

#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
pub struct Seconds(pub f32);

impl std::ops::Div<Iterations> for Seconds {
    type Output = Seconds;

    fn div(self, rhs: Iterations) -> Seconds {
        Seconds(self.0 / rhs.get() as f32)
    }
}

#[derive(thiserror::Error, Debug)]
#[error("error parsing time string")]
pub struct ParseSecondsError;

impl FromStr for Seconds {
    type Err = ParseSecondsError;

    fn from_str(s: &str) -> Result<Self, ParseSecondsError> {
        // Inspired by https://www.w3.org/TR/SMIL/smil-timing.html#Timing-ClockValueSyntax

        lazy_static! {
            static ref RE_HMS: Regex =
                Regex::new(r"^(?P<h>\d+):(?P<m>\d{2}):(?P<s>\d{2}(?:\.\d+)?)$").unwrap();
            static ref RE_MS: Regex = Regex::new(r"^(?P<m>\d+):(?P<s>\d{2}(?:\.\d+)?)$").unwrap();
            static ref RE_SUFFIX: Regex =
                Regex::new(r"^(?P<number>\d+(?:\.\d+)?)\s*(?P<suffix>h|min|s)?$").unwrap();
        }

        let s = s.trim();
        if let Some(caps) = RE_HMS.captures(s) {
            let h: f32 = caps.name("h").unwrap().as_str().parse().unwrap();
            let m: f32 = caps.name("m").unwrap().as_str().parse().unwrap();
            let s: f32 = caps.name("s").unwrap().as_str().parse().unwrap();
            if m >= 60.0 || s >= 60.0 {
                return Err(ParseSecondsError);
            }
            Ok(Seconds(h * 60.0 * 60.0 + m * 60.0 + s))
        } else if let Some(caps) = RE_MS.captures(s) {
            let m: f32 = caps.name("m").unwrap().as_str().parse().unwrap();
            let s: f32 = caps.name("s").unwrap().as_str().parse().unwrap();
            if s >= 60.0 {
                return Err(ParseSecondsError);
            }
            Ok(Seconds(m * 60.0 + s))
        } else if let Some(caps) = RE_SUFFIX.captures(s) {
            let number: f32 = caps.name("number").unwrap().as_str().parse().unwrap();
            Ok(Seconds(if let Some(suffix) = caps.name("suffix") {
                match suffix.as_str() {
                    "h" => number * 60.0 * 60.0,
                    "min" => number * 60.0,
                    "s" => number,
                    _ => unreachable!(),
                }
            } else {
                number
            }))
        } else {
            Err(ParseSecondsError)
        }
    }
}

#[cfg(test)]
mod tests_parse_seconds {
    use super::*;

    #[test]
    fn hms_leading_zeros() {
        assert_eq!(" 000:00:00 ".parse::<Seconds>().unwrap(), Seconds(0.0));
    }

    #[test]
    fn hms_with_fraction() {
        assert_eq!(
            "1:01:01.01".parse::<Seconds>().unwrap(),
            Seconds(60.0 * 60.0 + 61.01)
        );
    }

    #[test]
    fn ms_leading_zeros() {
        assert_eq!("000:00".parse::<Seconds>().unwrap(), Seconds(0.0));
    }

    #[test]
    fn ms_with_fraction() {
        assert_eq!(" 1:01.01 ".parse::<Seconds>().unwrap(), Seconds(61.01));
    }

    #[test]
    fn no_suffix() {
        assert_eq!(" 61.01 ".parse::<Seconds>().unwrap(), Seconds(61.01));
    }

    #[test]
    fn suffix_h() {
        assert_eq!("1.5 h".parse::<Seconds>().unwrap(), Seconds(90.0 * 60.0));
    }

    #[test]
    fn suffix_min() {
        assert_eq!("10min".parse::<Seconds>().unwrap(), Seconds(600.0));
    }

    #[test]
    fn suffix_s() {
        assert_eq!(" 9.99s ".parse::<Seconds>().unwrap(), Seconds(9.99));
    }

    #[test]
    fn leading_digit_is_required() {
        assert!(".99".parse::<Seconds>().is_err());
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

        if let Some(s) = s.strip_suffix('%') {
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
