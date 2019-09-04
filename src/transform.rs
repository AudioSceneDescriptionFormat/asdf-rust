use std::str::FromStr;

use nalgebra_glm as glm;
use xmlparser as xml;

use crate::error::ResultExt;
use crate::parser::error::ParseError;
use crate::parser::Attributes;

pub type Vec3 = glm::TVec3<f32>;
pub type Quat = glm::Qua<f32>;

pub fn get_length(v: Vec3) -> f32 {
    v.norm()
}

#[derive(Clone, Default)]
pub struct Transform {
    // NB: There must be a well-defined order of operations, i.e.
    //     first scale, then rotation, then translation
    _rotation: Option<Quat>,
    pub translation: Option<Vec3>,
    // TODO: scale?

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

    pub fn accumulate(&mut self, other: &Transform) {
        // NB: Repeated rotation is disallowed
        // TODO: Repeated scaling is disallowed

        /*
        if (other.rotation)
        {
            if (this->rotation)
            {
                // TODO: different exception type?
                // TODO: this exception should not terminate the program!
                throw std::runtime_error("Multiple rotations at once");
            }
            else
            {
                // NB: The rotation does not affect this->translation!
                this->rotation = *other.rotation;
            }
        }
        */

        self.apply_translation(other.translation);

        // TODO: handle other members
    }

    pub fn apply(&mut self, other: &Transform) {
        // NB: We apply rotation first, then translation.

        /*
        if (other.rotation)
        {
            this->apply_rotation(*other.rotation);
        }
        */
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
            // TODO: other attributes
            _ => out_attributes.push((name, value)),
        };
    }
    *attributes = out_attributes;
    Ok(result)
}

pub fn parse_pos(value: xml::StrSpan) -> Result<Vec3, ParseError> {
    let mut numbers = Vec::new();
    for s in value.as_str().split_whitespace() {
        numbers.push(f32::from_str(s).context(value)?);
    }
    if numbers.len() < 2 {
        return Err(ParseError::new(
            "At least 2 numbers are needed for \"pos\"",
            value,
        ));
    } else if numbers.len() > 3 {
        return Err(ParseError::new(
            "No more than 3 numbers are allowed for \"pos\"",
            value,
        ));
    }
    Ok(Vec3::new(
        numbers[0],
        numbers[1],
        if numbers.len() == 3 { numbers[2] } else { 0.0 },
    ))
}
