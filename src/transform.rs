use std::str::FromStr;

use xmlparser as xml;

use crate::parser::error::ParseError;
use crate::parser::Attributes;

pub type Vec3 = nalgebra::Vector3<f32>;
pub type Quat = nalgebra::UnitQuaternion<f32>;

#[derive(Clone, Default, Debug)]
pub struct Transform {
    // NB: There must be a well-defined order of operations, i.e.
    //     first scale, then rotation, then translation
    // TODO: scale?
    pub rotation: Option<Quat>,
    pub translation: Option<Vec3>,
    // NB: Some operations are order-independent: vol, ...
    pub volume: Option<f32>,
    // TODO: more fields?
}

impl Transform {
    /// Panics if there is more than one rotation.
    pub fn merge(one: Option<Transform>, two: Option<Transform>) -> Option<Transform> {
        // TODO: Repeated scaling is disallowed
        // NB: Repeated rotation is disallowed
        if one.is_none() {
            return two;
        }
        if let Some(one) = one {
            Some(if let Some(two) = two {
                if one.rotation.is_some() && two.rotation.is_some() {
                    // This is checked for when loading the scene
                    unreachable!("Multiple rotations at once");
                }
                Transform {
                    rotation: one.rotation.xor(two.rotation),
                    translation: one
                        .translation
                        .map(|v| v + two.translation.unwrap_or_else(Vec3::zeros))
                        .or(two.translation),
                    volume: one
                        .volume
                        .map(|v| v * two.volume.unwrap_or(1.0))
                        .or(two.volume),
                }
            } else {
                one
            })
        } else {
            two
        }
    }

    pub fn apply(&mut self, other: Option<Transform>) {
        if let Some(other) = other {
            // NB: We apply rotation first, then translation.

            // Rotation (around origin): rotate translation, combine rotations
            if let Some(rotation) = other.rotation {
                // NB: The order of the operations doesn't matter
                self.translation = self.translation.map(|v| rotation * v);
                // NB: other rotation is left-multiplied
                self.rotation = self.rotation.map(|r| rotation * r).or(Some(rotation));
                // TODO: renormalize_fast()?
            }

            if let Some(translation) = other.translation {
                // NB: rotation stays unchanged
                *self.translation.get_or_insert_with(Vec3::zeros) += translation;
            }

            if let Some(volume) = other.volume {
                *self.volume.get_or_insert(1.0) *= volume;
            }
        }
    }
}

pub fn parse_transform<'a>(
    attributes: &mut Attributes<'a>,
) -> Result<Option<Transform>, ParseError> {
    let mut result: Option<Transform> = None;

    let mut out_attributes = Attributes::new();

    for &(name, value) in attributes.iter() {
        match name.as_str() {
            "pos" => {
                result.get_or_insert_with(Default::default).translation = Some(parse_pos(value)?);
            }
            "rot" => {
                result.get_or_insert_with(Default::default).rotation = Some(parse_rot(value)?);
            }
            "vol" => {
                result.get_or_insert_with(Default::default).volume = Some(parse_vol(value)?);
            }
            // TODO: more attributes?
            _ => out_attributes.push((name, value)),
        };
    }
    *attributes = out_attributes;
    Ok(result)
}

pub fn parse_pos(value: xml::StrSpan<'_>) -> Result<Vec3, ParseError> {
    let mut values = value.as_str().split_whitespace().map(|s| {
        f32::from_str(s)
            .map_err(|e| ParseError::new(format!("error parsing \"pos\" value(s): {}", e), value))
    });
    let x = values.next().unwrap_or_else(|| {
        Err(ParseError::new(
            "At least 2 numbers are needed for \"pos\"",
            value,
        ))
    })?;
    let y = values.next().unwrap_or_else(|| {
        Err(ParseError::new(
            "At least 2 numbers are needed for \"pos\"",
            value,
        ))
    })?;
    let z = values.next().unwrap_or(Ok(0.0))?;
    if values.next().is_some() {
        return Err(ParseError::new(
            "No more than 3 numbers are allowed for \"pos\"",
            value,
        ));
    }
    Ok(Vec3::new(x, y, z))
}

pub fn parse_rot(value: xml::StrSpan<'_>) -> Result<Quat, ParseError> {
    let mut values = value.as_str().split_whitespace().map(|s| {
        f32::from_str(s)
            .map_err(|e| ParseError::new(format!("error parsing \"rot\" value(s): {}", e), value))
    });
    let azimuth = values.next().unwrap_or_else(|| {
        Err(ParseError::new(
            "At least 1 number is needed for \"rot\"",
            value,
        ))
    })?;
    let elevation = values.next().unwrap_or(Ok(0.0))?;
    let roll = values.next().unwrap_or(Ok(0.0))?;
    if values.next().is_some() {
        return Err(ParseError::new(
            "No more than 3 numbers are allowed for \"rot\"",
            value,
        ));
    }

    fn radians(deg: f32) -> f32 {
        deg * std::f32::consts::PI / 180.0
    }

    Ok(Quat::from_axis_angle(&Vec3::z_axis(), radians(azimuth))
        * Quat::from_axis_angle(&Vec3::x_axis(), radians(elevation))
        * Quat::from_axis_angle(&Vec3::y_axis(), radians(roll)))
}

pub fn parse_vol(value: xml::StrSpan<'_>) -> Result<f32, ParseError> {
    let vol = value
        .as_str()
        .trim()
        .parse()
        .map_err(|e| ParseError::from_source(e, "invalid volume", value))?;
    if vol < 0.0 {
        return Err(ParseError::new("volume cannot be negative", value));
    }
    Ok(vol)
}
