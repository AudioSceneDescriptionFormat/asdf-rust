use std::str::FromStr;

use xmlparser as xml;

use crate::error::ResultExt;
use crate::parser::error::ParseError;
use crate::parser::Attributes;

pub type Vec3 = nalgebra::Vector3<f32>;
pub type Quat = nalgebra::UnitQuaternion<f32>;

pub fn get_length(v: Vec3) -> f32 {
    v.norm()
}

#[derive(Clone, Default)]
pub struct Transform {
    // NB: There must be a well-defined order of operations, i.e.
    //     first scale, then rotation, then translation
    // TODO: scale?
    pub rotation: Option<Quat>,
    pub translation: Option<Vec3>,

    // NB: Some operations are order-independent: gain, ...
    // TODO: gain, ...
}

impl Transform {
    /// NB: rotation stays unchanged
    pub fn apply_translation(&mut self, other: Option<Vec3>) {
        if let Some(translation) = other {
            *self.translation.get_or_insert(Vec3::zeros()) += translation;
        }
    }

    /// Rotation (around origin): rotate translation, rotate orientation
    pub fn apply_rotation(&mut self, other: Option<Quat>) {
        // NB: The order of the operations doesn't matter
        if let Some(rotation) = other {
            self.translation = self.translation.map(|v| rotation * v);
            // NB: other rotation is left-multiplied
            self.rotation = self.rotation.map(|r| rotation * r).or(Some(rotation));
            // TODO: renormalize_fast()?
        }
    }

    pub fn accumulate(&mut self, other: &Transform) {
        // TODO: Repeated scaling is disallowed
        // NB: Repeated rotation is disallowed

        if let Some(rotation) = other.rotation {
            if self.rotation.is_some() {
                // TODO: don't panic, proper Result
                panic!("Multiple rotations at once");
            } else {
                // NB: The rotation does not affect self.translation!
                self.rotation = Some(rotation);
            }
        }

        self.apply_translation(other.translation);

        // TODO: handle other members
    }

    pub fn apply(&mut self, other: &Transform) {
        // NB: We apply rotation first, then translation.
        self.apply_rotation(other.rotation);
        self.apply_translation(other.translation);

        // TODO: handle other members
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
            // TODO: other attributes
            _ => out_attributes.push((name, value)),
        };
    }
    *attributes = out_attributes;
    Ok(result)
}

pub fn parse_pos(value: xml::StrSpan) -> Result<Vec3, ParseError> {
    let mut values = value
        .as_str()
        .split_whitespace()
        .map(|s| f32::from_str(s).context(value));
    let x = values.next().unwrap_or(Err(ParseError::new(
        "At least 2 numbers are needed for \"pos\"",
        value,
    )))?;
    let y = values.next().unwrap_or(Err(ParseError::new(
        "At least 2 numbers are needed for \"pos\"",
        value,
    )))?;
    let z = values.next().unwrap_or(Ok(0.0))?;
    if values.next().is_some() {
        return Err(ParseError::new(
            "No more than 3 numbers are allowed for \"pos\"",
            value,
        ));
    }
    Ok(Vec3::new(x, y, z))
}

pub fn parse_rot(value: xml::StrSpan) -> Result<Quat, ParseError> {
    let mut values = value
        .as_str()
        .split_whitespace()
        .map(|s| f32::from_str(s).context(value));
    let azimuth = values.next().unwrap_or(Err(ParseError::new(
        "At least 1 number is needed for \"rot\"",
        value,
    )))?;
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
