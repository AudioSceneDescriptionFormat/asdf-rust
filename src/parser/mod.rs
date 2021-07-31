use std::collections::{HashMap, HashSet};
use std::convert::TryInto as _;
use std::fs;
use std::num::NonZeroU64;
use std::path::{Path, PathBuf};
use std::time::Duration;

use asdfspline::{AsdfPosSpline, AsdfRotSpline, NormWrapper, PiecewiseCubicCurve, Spline};
use regex::Regex;
use xmlparser as xml;

use crate::audiofile::dynamic::AudioFile;
use crate::streamer::FileStreamer;
use crate::transform::{Transform, Vec3};
use crate::{Scene, Source, Transformer, REFERENCE_ID};

mod elements;
pub mod error;
mod time;

use elements::{AsdfElement, Element};
use error::{IntegrityError, LoadError, ParseError};
use time::frames2seconds;

pub type FileStorage = Vec<(Box<dyn AudioFile + Send + Sync>, Box<[Option<usize>]>)>;

#[derive(Default)]
pub struct SceneInitializer {
    dir: PathBuf,
    samplerate: u32,
    blocksize: u32,
    buffer_blocks: u32,
    sleeptime: Duration,
    all_ids: HashSet<String>,
    file_sources: Vec<Source>,
    live_sources: Vec<Source>,
    /// Same length as `live_sources`
    ports: Vec<String>,
    current_id_suffix: u32,
    file_storage: FileStorage,
    transformer_storage: Vec<Box<dyn Transformer>>,
    transformer_instances: Vec<TransformerInstance>,
    transformer_map: HashMap<String, Vec<usize>>,
    streamer: Option<FileStreamer>,
    reference_transform: Transform,
    frames: Option<u64>,
}

impl SceneInitializer {
    fn add_transformer(
        &mut self,
        transformer: Box<dyn Transformer>,
        begin: u64,
        duration: u64,
        targets: &[String],
        instances: &mut Vec<TransformerInstance>,
    ) -> usize {
        let idx = self.transformer_storage.len();
        self.transformer_storage.push(transformer);
        for target in targets {
            self.transformer_map
                .entry(target.clone())
                .or_default()
                .push(idx);
        }
        instances.push(TransformerInstance {
            begin,
            duration,
            idx,
        });
        idx
    }
}

#[derive(Clone)]
pub struct PlaylistEntry {
    pub begin: u64,
    pub duration: u64,
    pub idx: usize,
}

#[derive(Clone)]
pub struct TransformerInstance {
    pub begin: u64,
    pub duration: u64,
    pub idx: usize,
}

struct ConstantTransformer {
    id: Option<String>,
    transform: Transform,
}

impl Transformer for ConstantTransformer {
    fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }

    fn get_transform(&self, _frame: u64) -> Option<Transform> {
        Some(self.transform.clone())
    }
}

struct EmptyTransformer {
    id: Option<String>,
}

impl Transformer for EmptyTransformer {
    fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }

    fn get_transform(&self, _frame: u64) -> Option<Transform> {
        None
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Iterations(NonZeroU64);

impl Default for Iterations {
    fn default() -> Iterations {
        Iterations(NonZeroU64::new(1).unwrap())
    }
}

impl Iterations {
    pub fn get(&self) -> u64 {
        self.0.get()
    }
}

struct Norm3;

impl NormWrapper<Norm3> for Vec3 {
    fn norm(&self) -> f32 {
        self.norm()
    }
}

struct SplineTransformer {
    id: Option<String>,
    pos_spline: Option<AsdfPosSpline<Vec3, Norm3>>,
    rot_spline: Option<AsdfRotSpline>,
    vol_spline: Option<PiecewiseCubicCurve<f32>>,
    samplerate: u32,
}

impl Transformer for SplineTransformer {
    fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }

    fn get_transform(&self, frame: u64) -> Option<Transform> {
        let time = frames2seconds(frame, self.samplerate).0;
        Some(Transform {
            translation: self.pos_spline.as_ref().map(|s| s.evaluate(time)),
            rotation: self.rot_spline.as_ref().map(|s| s.evaluate(time)),
            volume: self.vol_spline.as_ref().map(|s| s.evaluate(time)),
        })
    }
}

#[macro_export]
macro_rules! parse_error {
    ($span:expr, $msg:expr) => {
        return Err(ParseError::new($msg, $span).into());
    };
    ($span:expr, $fmt:expr, $($arg:expr),+) => {
        return Err(ParseError::new(format!($fmt, $($arg),+), $span).into());
    };
}

pub fn load_scene(
    path: &Path,
    samplerate: u32,
    blocksize: u32,
    buffer_blocks: u32,
    sleeptime: Duration,
) -> Result<Scene, LoadError> {
    let file_data = fs::read_to_string(path)?;
    let mut element_stack = Vec::<(Box<dyn Element<'_>>, xml::StrSpan<'_>)>::new();
    let mut scene = SceneInitializer {
        dir: path.parent().unwrap().into(),
        samplerate,
        blocksize,
        buffer_blocks,
        sleeptime,
        ..Default::default()
    };
    let mut attributes = Attributes::new();

    for token in xml::Tokenizer::from(&file_data as &str) {
        use xml::Token::*;
        match token? {
            Declaration {
                version,
                encoding,
                standalone,
                span,
            } => {
                if version.as_str() != "1.0" {
                    parse_error!(version, "Only XML version 1.0 is supported")
                }
                if let Some(encoding) = encoding {
                    if encoding.as_str().to_lowercase() != "utf-8" {
                        parse_error!(encoding, "Only UTF-8 encoding is supported")
                    }
                }
                if let Some(standalone) = standalone {
                    if !standalone {
                        parse_error!(span, "\"standalone\" must be \"yes\"")
                    }
                }
            }
            DtdStart { span, .. } => {
                parse_error!(span, "Document type definitions are not supported")
            }
            DtdEnd { .. } => unreachable!(),
            EmptyDtd { span, .. } => {
                parse_error!(span, "Document type definitions are not supported")
            }
            EntityDeclaration { .. } => unreachable!(),
            Cdata { span, .. } => parse_error!(span, "CDATA sections are not supported"),
            ProcessingInstruction { span, .. } => {
                parse_error!(span, "Processing instructions are not supported")
            }
            Comment { .. } => { /* ignored */ }
            ElementStart {
                prefix,
                local: name,
                ..
            } => {
                no_namespaces(prefix)?;
                let new_element = match element_stack.last_mut() {
                    Some((parent, parent_span)) => parent.open_child_element(name, *parent_span)?,
                    None => Box::new(AsdfElement::new(name)?),
                };
                element_stack.push((new_element, name));
            }
            Attribute {
                prefix,
                local: name,
                value,
                ..
            } => {
                no_namespaces(prefix)?;
                if attributes.iter().any(|&(k, _)| k.as_str() == name.as_str()) {
                    parse_error!(name, "Duplicate attribute {:?}", name.as_str())
                }
                attributes.push((name, value));
            }
            ElementEnd { end, .. } => {
                use xml::ElementEnd::*;
                match end {
                    Open => {
                        let (element, span) = element_stack.last_mut().unwrap();
                        element.parse_attributes(&mut attributes, *span, &mut scene)?;
                    }
                    Close(prefix, name) => {
                        no_namespaces(prefix)?;
                        let (element, span) = element_stack.pop().unwrap();
                        if name.as_str() != span.as_str() {
                            parse_error!(
                                name,
                                "Non-matching closing tag </{}> (expected </{}>)",
                                name.as_str(),
                                span.as_str()
                            )
                        }
                        if let Some((parent, _)) = element_stack.last_mut() {
                            element.close(name, Some(parent), &mut scene)?;
                        } else {
                            element.close(name, None, &mut scene)?;
                        }
                        assert!(attributes.is_empty());
                    }
                    Empty => {
                        let (mut element, span) = element_stack.pop().unwrap();
                        element.parse_attributes(&mut attributes, span, &mut scene)?;
                        if let Some((parent, _)) = element_stack.last_mut() {
                            element.close(span, Some(parent), &mut scene)?;
                        } else {
                            element.close(span, None, &mut scene)?;
                        }
                    }
                }
                if let Some(&(name, _)) = attributes.first() {
                    parse_error!(name, "Attribute {:?} is not allowed", name.as_str())
                }
            }
            Text { text } => {
                if !text.as_str().trim().is_empty() {
                    parse_error!(text, "No text content allowed")
                }
            }
        }
    }
    if let Some((_, span)) = element_stack.last() {
        parse_error!(*span, "Missing </{}> tag", span.as_str())
    }
    if scene.streamer.is_none() {
        // See https://github.com/RazrFalcon/xmlparser/issues/8
        parse_error!(file_data.as_str().into(), "Missing XML root element")
    }
    scene.try_into()
}

impl std::convert::TryFrom<SceneInitializer> for Scene {
    type Error = LoadError;

    fn try_from(scene: SceneInitializer) -> Result<Self, Self::Error> {
        for id in scene.transformer_map.keys() {
            if id != REFERENCE_ID && !scene.all_ids.contains(id) {
                return Err(IntegrityError::NonExistingId(id.clone()).into());
            }
        }

        let mut transformer_activity = Vec::new();
        transformer_activity.resize(scene.transformer_storage.len(), Vec::new());

        for instance in &scene.transformer_instances {
            transformer_activity[instance.idx]
                .push((instance.begin, instance.begin + instance.duration));
        }

        // TODO: assert that transformer activities are sorted (they should be!?!)

        let mut sources = scene.file_sources;
        let mut more_sources = scene.live_sources;
        sources.append(&mut more_sources);
        let scene = Scene {
            sources,
            streamer: scene.streamer.unwrap(),
            transformers: scene
                .transformer_storage
                .into_iter()
                .zip(transformer_activity)
                .map(|(t, a)| (t, a.into()))
                .collect(),
            transformer_map: scene
                .transformer_map
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            reference_transform: scene.reference_transform,
            ports: scene.ports,
            frames: scene.frames,
        };
        for source in &scene.sources {
            if let Some(id) = &source.id {
                scene.check_integrity(&id, HashSet::new())?;
            }
        }
        scene.check_integrity("reference", HashSet::new())?;
        Ok(scene)
    }
}

pub type Attributes<'a> = Vec<(xml::StrSpan<'a>, xml::StrSpan<'a>)>;

trait GetAttributeValue {
    fn get_value(&mut self, name: &str) -> Option<xml::StrSpan<'_>>;
    fn get_item(&mut self, name: &str) -> Option<(xml::StrSpan<'_>, xml::StrSpan<'_>)>;
}

impl<'a> GetAttributeValue for Attributes<'a> {
    fn get_value(&mut self, name: &str) -> Option<xml::StrSpan<'_>> {
        self.get_item(name).map(|(_, v)| v)
    }

    fn get_item(&mut self, name: &str) -> Option<(xml::StrSpan<'_>, xml::StrSpan<'_>)> {
        self.iter()
            .position(|&(k, _)| k.as_str() == name)
            .map(|idx| self.remove(idx))
    }
}

fn no_namespaces(prefix: xml::StrSpan<'_>) -> Result<(), ParseError> {
    if prefix.is_empty() {
        Ok(())
    } else {
        Err(ParseError::new(
            "The ASDF doesn't use XML namespaces",
            prefix,
        ))
    }
}

impl SceneInitializer {
    /// https://www.w3.org/TR/xml-id/
    ///
    /// * the ID value matches the allowed lexical form,
    /// * the value is unique within the XML document, and that
    /// * each element has at most one single unique identifier
    ///
    /// The last point has already been checked before (duplicate attributes are not allowed).
    ///
    /// Returning an empty string means the attribute didn't exist.
    /// NB: Empty strings are not valid XML IDs!
    fn parse_id(&mut self, value: xml::StrSpan<'_>) -> Result<String, ParseError> {
        lazy_static! {
            // TODO: Allow flanking whitespace?
            static ref RE: Regex = Regex::new(r"(?x)
                ^
                # NCNameStartChar
                [
                A-Z_a-z
                \u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D
                \u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF
                \u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF
                ]
                # NCNameChar*
                [
                -.0-9\u00B7\u0300-\u036F\u203F-\u2040
                A-Z_a-z
                \u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D
                \u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF
                \u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF
                ]*
                $
                ").unwrap();
        }
        let id = value.to_string();
        if id == REFERENCE_ID {
            parse_error!(value, "Reserved ID: \"{}\"", REFERENCE_ID)
        }
        if !RE.is_match(&id) {
            parse_error!(value, "Invalid XML ID: \"{}\"", id)
        }
        if !self.all_ids.insert(id.clone()) {
            parse_error!(value, "Non-unique ID: \"{}\"", id)
        }
        Ok(id)
    }

    /// This creates intentionally invalid XML IDs for internal use
    fn create_new_id(&mut self) -> String {
        self.current_id_suffix += 1;
        let id = format!(".asdf:{}", self.current_id_suffix);
        if !self.all_ids.insert(id.clone()) {
            unreachable!("Non-unique auto-generated ID: {:?}", id);
        }
        id
    }

    fn get_id(&mut self, attributes: &mut Attributes<'_>) -> Result<Option<String>, ParseError> {
        Ok(if let Some(value) = attributes.get_value("id") {
            Some(self.parse_id(value)?)
        } else {
            None
        })
    }

    fn get_file_source_id(
        &mut self,
        attributes: &mut Attributes<'_>,
    ) -> Result<Option<String>, ParseError> {
        if let Some(value) = attributes.get_value("source") {
            let id = value.as_str().trim().to_string();
            if self
                .file_sources
                .iter()
                .filter_map(|src| src.id.as_ref())
                .any(|s| *s == id)
            {
                return Ok(Some(id));
            }
            if self
                .live_sources
                .iter()
                .filter_map(|src| src.id.as_ref())
                .any(|x| *x == id)
            {
                parse_error!(
                    value,
                    "cannot use source {:?} because it has a \"port\" attribute",
                    id
                );
            }
            let id = self.parse_id(value)?;
            self.file_sources.push(Source {
                id: Some(id.clone()),
                ..Default::default()
            });
            Ok(Some(id))
        } else {
            Ok(None)
        }
    }
}

impl Scene {
    /// Check reference cycles and overlapping rotations.
    fn check_integrity(
        &self,
        id: &str,
        already_checked: HashSet<String>,
    ) -> Result<(), IntegrityError> {
        if let Some(transformer_indices) = self.transformer_map.get(id) {
            let transformers: Vec<_> = transformer_indices
                .iter()
                .map(|&idx| &self.transformers[idx])
                .collect();

            if already_checked.contains(id) {
                return Err(IntegrityError::CyclicDependency(id.to_owned()));
            }
            let mut already_checked = already_checked;
            already_checked.insert(id.to_owned());

            for (transformer, _activity) in &transformers {
                if let Some(id) = transformer.id() {
                    // TODO: less expensive cycle check?
                    self.check_integrity(id, already_checked.clone())?;
                }
            }

            // Check for overlapping rotations
            for i in 0..transformers.len() {
                let (transformer_i, activity_i) = transformers[i];
                for (transformer_j, activity_j) in transformers.iter().skip(i + 1) {
                    // TODO: possible early return(s) if activity is sorted?
                    for &(begin_i, end_i) in activity_i.iter() {
                        for &(begin_j, end_j) in activity_j.iter() {
                            if begin_i < end_j && begin_j < end_i {
                                let begin = begin_i.max(begin_j);
                                let end = end_i.min(end_i);
                                if self.has_rotation(&**transformer_i, begin, end)
                                    && self.has_rotation(&**transformer_j, begin, end)
                                {
                                    return Err(IntegrityError::MultipleRotations(id.to_owned()));
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    // No need to check reference cycles, because they have been checked in check_integrity()
    fn has_rotation(&self, transformer: &dyn Transformer, begin: u64, end: u64) -> bool {
        // Check given transformer
        if let Some(Transform {
            rotation: Some(_), ..
        }) = transformer.get_transform(0)
        {
            return true;
        }
        // Recursively check transformers that apply to the given transformer
        if let Some(id) = transformer.id() {
            if let Some(transformer_indices) = self.transformer_map.get(id) {
                for &idx in transformer_indices.iter() {
                    let (transformer, activity) = &self.transformers[idx];
                    for &(begin_i, end_i) in activity.iter() {
                        if begin < end_i
                            && begin_i < end
                            && self.has_rotation(&**transformer, begin.max(begin_i), end.min(end_i))
                        {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }
}
