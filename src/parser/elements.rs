use std::any::Any;
use std::num::NonZeroU64;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use asdfspline::AsdfPosSpline;
use xmlparser as xml;

use crate::audiofile::dynamic::{load_audio_file, AudioFile};
use crate::streamer::FileStreamer;
use crate::transform::{parse_pos, parse_transform, Transform, Vec3};
use crate::{Source, Transformer, REFERENCE_ID};

use super::error::ParseError;
use super::time::{frames2seconds, seconds2frames, Seconds};
use super::{
    Attributes, ConstantTransformer, GetAttributeValue, PlaylistEntry, SceneInitializer,
    SplineTransformer, TransformerInstance,
};

pub trait AsAny {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Any> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub trait Element<'a>: AsAny {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        end_span: xml::StrSpan,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError>;

    fn open_child_element(
        &mut self,
        name: xml::StrSpan,
        parent_span: xml::StrSpan,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        Err(ParseError::new(
            format!("No child elements allowed in <{}>", parent_span.as_str()),
            name,
        ))
    }

    fn add_files_and_transformers(
        &mut self,
        _files: Vec<PlaylistEntry>,
        _transformers: Vec<TransformerInstance>,
        _duration: u64,
        _span: xml::StrSpan,
    ) -> Result<(), ParseError> {
        unreachable!("This has to be implemented for all container elements");
    }

    // NB: "span" is stored in "scene", therefore they must have matching lifetimes.
    // TODO: can the borrowed Box be avoided?
    // TODO: see https://github.com/rust-lang/rust-clippy/issues/3971
    // TODO: see https://github.com/rust-lang/rust-clippy/issues/1845
    #[allow(clippy::borrowed_box)]
    fn close(
        self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element>>,
        scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError>;
}

pub struct AsdfElement {
    version: String,
    seq: SeqElement,
    previous_child: String,
}

impl AsdfElement {
    pub fn new(name: xml::StrSpan) -> Result<AsdfElement, ParseError> {
        if name.as_str() == "asdf" {
            Ok(AsdfElement {
                version: String::new(),
                seq: SeqElement::new(),
                previous_child: String::new(),
            })
        } else {
            Err(ParseError::new(
                format!("Expected <asdf> as root element, not <{}>", name.as_str()),
                name,
            ))
        }
    }
}

impl<'a> Element<'a> for AsdfElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        span: xml::StrSpan,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(value) = attributes.get_value("version") {
            if value.as_str().trim() == "0.4" {
                self.version = "0.4".into();
            } else {
                return Err(ParseError::new(
                    "Only ASDF version 0.4 is currently supported",
                    value,
                ));
            }
        } else {
            return Err(ParseError::new(
                "\"version\" attribute is required in <asdf> element",
                span,
            ));
        }
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan,
        parent_span: xml::StrSpan,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        let result = match name.as_str() {
            "head" => {
                if self.previous_child.is_empty() {
                    Ok(Box::new(HeadElement::new()) as Box<dyn Element>)
                } else {
                    Err(ParseError::new("No element is allowed before <head>", name))
                }
            }
            "body" => {
                if self.previous_child.is_empty() || self.previous_child == "head" {
                    Ok(Box::new(BodyElement::new()) as Box<dyn Element>)
                } else {
                    Err(ParseError::new(
                        "Only a <head> element is allowed before <body>",
                        name,
                    ))
                }
            }
            _ => {
                // <asdf> is an implicit <seq> (but only if there is no <body>):
                if self.previous_child == "body" {
                    Err(ParseError::new(
                        "No elements are allowed after <body>",
                        name,
                    ))
                } else {
                    self.seq.open_child_element(name, parent_span)
                }
            }
        };
        self.previous_child = name.to_string();
        result
    }

    fn add_files_and_transformers(
        &mut self,
        files: Vec<PlaylistEntry>,
        transformers: Vec<TransformerInstance>,
        duration: u64,
        span: xml::StrSpan,
    ) -> Result<(), ParseError> {
        self.seq
            .add_files_and_transformers(files, transformers, duration, span)
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element>>,
        scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        assert!(parent.is_none());
        scene.transformer_instances = self.seq.transformers;
        scene.streamer = Some(FileStreamer::new(
            self.seq.files,
            scene.file_storage.split_off(0),
            scene.blocksize,
            scene.sources.len() as u32,
            scene.buffer_blocks,
            scene.sleeptime,
        ));
        Ok(())
    }
}

struct HeadElement {
    reference: bool,
}

impl HeadElement {
    pub fn new() -> HeadElement {
        HeadElement { reference: false }
    }
}

impl<'a> Element<'a> for HeadElement {
    fn parse_attributes(
        &mut self,
        _attributes: &mut Attributes,
        _span: xml::StrSpan,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        // No attributes are allowed
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan,
        _parent_span: xml::StrSpan,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        match name.as_str() {
            "meta" => Err(ParseError::new("TODO: implement <meta> tags", name)),
            "source" => Ok(Box::new(SourceElement::new())),
            "reference" => {
                if self.reference {
                    Err(ParseError::new("Only one <reference> is allowed", name))
                } else {
                    self.reference = true;
                    Ok(Box::new(ReferenceElement::new()))
                }
            }
            _ => Err(ParseError::new(
                format!("No <{}> elements allowed in <head>", name.as_str()),
                name,
            )),
        }
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        _parent: Option<&mut Box<dyn Element>>,
        _scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        Ok(())
    }
}

struct SourceElement {}

impl SourceElement {
    pub fn new() -> SourceElement {
        SourceElement {}
    }
}

impl<'a> Element<'a> for SourceElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        _span: xml::StrSpan,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        let id = scene.get_id(attributes)?;

        let name = attributes.get_value("name").map(|v| v.to_string());
        let model = attributes.get_value("model").map(|v| v.to_string());

        // TODO: source without ID is only allowed for live sources!

        let transform = parse_transform(attributes)?.unwrap_or_default();
        scene.sources.push(Source {
            id,
            name,
            model,
            activity: Default::default(),
            transform,
        });
        Ok(())
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        _parent: Option<&mut Box<dyn Element>>,
        _scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        // TODO: disallow <source/> with no attributes?
        Ok(())
    }
}

struct ReferenceElement {}

impl ReferenceElement {
    pub fn new() -> ReferenceElement {
        ReferenceElement {}
    }
}

impl<'a> Element<'a> for ReferenceElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        _span: xml::StrSpan,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(id) = attributes.get_value("id") {
            if id.as_str() != REFERENCE_ID {
                return Err(ParseError::new(
                    format!("Reference ID must be {:?}", REFERENCE_ID),
                    id,
                ));
            }
        }
        scene.reference_transform = parse_transform(attributes)?.unwrap_or_default();
        Ok(())
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        _parent: Option<&mut Box<dyn Element>>,
        _scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        // TODO: disallow <reference/> with no attributes?
        Ok(())
    }
}

struct BodyElement {
    seq: SeqElement,
}

impl BodyElement {
    pub fn new() -> BodyElement {
        BodyElement {
            seq: SeqElement::new(),
        }
    }
}

impl<'a> Element<'a> for BodyElement {
    fn parse_attributes(
        &mut self,
        _attributes: &mut Attributes,
        _span: xml::StrSpan,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        // No attributes are allowed
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan,
        parent_span: xml::StrSpan,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        self.seq.open_child_element(name, parent_span)
    }

    fn add_files_and_transformers(
        &mut self,
        files: Vec<PlaylistEntry>,
        transformers: Vec<TransformerInstance>,
        duration: u64,
        span: xml::StrSpan,
    ) -> Result<(), ParseError> {
        self.seq
            .add_files_and_transformers(files, transformers, duration, span)
    }

    fn close(
        self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element>>,
        scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        Box::new(self.seq).close(span, parent, scene)
    }
}

struct SeqElement {
    files: Vec<PlaylistEntry>,
    transformers: Vec<TransformerInstance>,
    end: u64,
    iterations: NonZeroU64,
}

impl SeqElement {
    fn new() -> SeqElement {
        SeqElement {
            files: Vec::new(),
            transformers: Vec::new(),
            end: 0,
            iterations: NonZeroU64::new(1).unwrap(),
        }
    }
}

impl<'a> Element<'a> for SeqElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        _span: xml::StrSpan,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(repeat_value) = attributes.get_value("repeat") {
            self.iterations = parse_attribute(repeat_value)?;
        }
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan,
        parent_span: xml::StrSpan,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        child_in_container(name, parent_span)
    }

    fn add_files_and_transformers(
        &mut self,
        files: Vec<PlaylistEntry>,
        transformers: Vec<TransformerInstance>,
        duration: u64,
        _span: xml::StrSpan,
    ) -> Result<(), ParseError> {
        let end = self.end;
        self.files
            .extend(files.into_iter().map(|entry| PlaylistEntry {
                begin: end + entry.begin,
                ..entry
            }));
        self.transformers.extend(
            transformers
                .into_iter()
                .map(|instance| TransformerInstance {
                    begin: end + instance.begin,
                    ..instance
                }),
        );
        self.end += duration;
        Ok(())
    }

    fn close(
        self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element>>,
        _scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        let mut files = Vec::new();
        let mut transformers = Vec::new();
        for i in 0..self.iterations.get() {
            files.extend(self.files.iter().cloned().map(|entry| PlaylistEntry {
                begin: i * self.end + entry.begin,
                ..entry
            }));
            transformers.extend(self.transformers.iter().cloned().map(|instance| {
                TransformerInstance {
                    begin: i * self.end + instance.begin,
                    ..instance
                }
            }));
        }
        parent
            .unwrap()
            .add_files_and_transformers(files, transformers, self.end, span)
    }
}

struct ParElement {
    files: Vec<PlaylistEntry>,
    transformers: Vec<TransformerInstance>,
    duration_frames: Option<u64>,
    iterations: NonZeroU64,
}

impl ParElement {
    fn new() -> ParElement {
        ParElement {
            files: Vec::new(),
            transformers: Vec::new(),
            duration_frames: None,
            iterations: NonZeroU64::new(1).unwrap(),
        }
    }
}

impl<'a> Element<'a> for ParElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        _span: xml::StrSpan,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(repeat_value) = attributes.get_value("repeat") {
            self.iterations = parse_attribute(repeat_value)?;
        }
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan,
        parent_span: xml::StrSpan,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        child_in_container(name, parent_span)
    }

    fn add_files_and_transformers(
        &mut self,
        files: Vec<PlaylistEntry>,
        transformers: Vec<TransformerInstance>,
        duration: u64,
        span: xml::StrSpan,
    ) -> Result<(), ParseError> {
        if let Some(self_duration) = self.duration_frames {
            if duration > self_duration {
                // NB: This is a limitation to simplify parsing, it may be lifted at some point.
                return Err(ParseError::new(
                    "The first element in <par> must be the longest",
                    span,
                ));
            }
        } else {
            self.duration_frames = Some(duration);
        }
        self.files.extend(files);
        self.transformers.extend(transformers);
        Ok(())
    }

    // TODO: better code re-use w.r.t. SeqElement?
    fn close(
        self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element>>,
        _scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        let duration = self.duration_frames.unwrap_or_default();
        let mut files = Vec::new();
        let mut transformers = Vec::new();
        for i in 0..self.iterations.get() {
            files.extend(self.files.iter().cloned().map(|entry| PlaylistEntry {
                begin: i * duration + entry.begin,
                ..entry
            }));
            transformers.extend(self.transformers.iter().cloned().map(|instance| {
                TransformerInstance {
                    begin: i * duration + instance.begin,
                    ..instance
                }
            }));
        }
        parent
            .unwrap()
            .add_files_and_transformers(files, transformers, duration, span)?;
        Ok(())
    }
}

#[derive(Default)]
struct ClipElement {
    clip_id: Option<String>,
    source_id: Option<String>,
    file: Option<Box<dyn AudioFile + Send + Sync>>,
    channels: Vec<ChannelElement>,
    /// An optional 0-based source index for each channel in the file.
    /// The channel_map can be shorter than the number of channels in the file
    /// (but not longer!).
    channel_map: Vec<Option<usize>>,
    channel_ids: Vec<String>,
    transform: Option<Transform>,
}

impl ClipElement {
    fn new() -> ClipElement {
        Default::default()
    }
}

impl<'a> Element<'a> for ClipElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        span: xml::StrSpan,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        self.clip_id = scene.get_id(attributes)?;
        self.source_id = scene.get_source_id(attributes)?;

        // TODO: check source_id (if non-empty) for source properties
        // TODO: depending on this, the rest may be treated differently
        // NB: only model="point" (default) and model="plane" is allowed for now
        // TODO: allow other source models (e.g. binaural, ambisonics, ...)

        let iterations = if let Some(repeat_value) = attributes.get_value("repeat") {
            parse_attribute(repeat_value)?
        } else {
            NonZeroU64::new(1).unwrap()
        };

        if let Some(file_value) = attributes.get_value("file") {
            let mut path = PathBuf::from(file_value.as_str());

            if path == Path::new("") {
                return Err(ParseError::new("Empty file name", file_value));
            }
            if path.is_relative() {
                path = scene.dir.join(path);
            }

            self.file = Some(
                load_audio_file(path, scene.samplerate, iterations).map_err(|e| {
                    ParseError::new(format!("error loading audio file: {}", e), file_value)
                })?,
            );
        } else {
            return Err(ParseError::new(
                "\"file\" attribute is required in <clip> element",
                span,
            ));
        }
        self.transform = parse_transform(attributes)?;
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan,
        _parent_span: xml::StrSpan,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        if name.as_str() == "channel" {
            if self.source_id.is_none() {
                let file_channels = self.file.as_ref().unwrap().channels();
                if self.channel_map.len() >= file_channels as usize {
                    return Err(ParseError::new(
                        format!(
                        "Only as many <channel> elements as channels in the file are allowed ({})",
                        file_channels),
                        name,
                    ));
                }
                Ok(Box::new(ChannelElement::default()))
            } else {
                Err(ParseError::new(
                    "No <channel> elements are allowed if <clip> has a \"source\" attribute",
                    name,
                ))
            }
        } else {
            Err(ParseError::new(
                format!(
                    "Only <channel> elements are allowed in <clip>, not <{}>",
                    name.as_str()
                ),
                name,
            ))
        }
    }

    fn close(
        mut self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element>>,
        scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        let file = self.file.take().unwrap();
        let duration = file.frames();
        let file_channels = file.channels();
        if self.channels.is_empty() {
            if file_channels != 1 {
                return Err(ParseError::new(
                    "Multi-channel <clip> must have at least one <channel> sub-element",
                    span,
                ));
            }
            // Implicit <channel> element for mono file
            let mut channel = ChannelElement::default();
            channel.source_id = self.source_id.clone();
            assert!(channel.transform.is_none());
            self.channels.push(channel);
        }
        let mut transformers = Vec::new();
        for channel in self.channels {
            if let Some(skip) = channel.skip {
                for _ in 0..skip {
                    self.channel_map.push(None);
                }
            } else {
                if self.channel_map.len() >= file_channels as usize {
                    return Err(ParseError::new(
                        format!(
                            "Too many <channel> elements (file has only {} channels)",
                            file_channels
                        ),
                        span,
                    ));
                }
                let source_number = if let Some(source_id) = channel.source_id {
                    // Source must already exist
                    scene
                        .sources
                        .iter()
                        .filter_map(|s| s.id.as_ref())
                        .position(|id| *id == source_id)
                        .unwrap()
                } else {
                    scene.sources.push(Default::default());
                    scene.sources.len() - 1
                };
                self.channel_map.push(Some(source_number));

                // IDs are required for the parent transformer to work
                let channel_id = channel.channel_id.unwrap_or_else(|| scene.create_new_id());
                self.channel_ids.push(channel_id.clone());

                // <channel> transformer

                let targets = vec![];
                let idx = scene.add_transformer(
                    Box::new(ConstantTransformer {
                        id: Some(channel_id),
                        transform: channel.transform.unwrap_or_default(),
                    }),
                    0,
                    duration,
                    &targets,
                    &mut transformers,
                );
                scene.channel_transformers.push((idx, source_number, span));
            }
        }

        // <clip> transformer that applies to all <channel> elements

        scene.add_transformer(
            Box::new(ConstantTransformer {
                id: self.clip_id,
                transform: self.transform.unwrap_or_default(),
            }),
            0,
            duration,
            &self.channel_ids,
            &mut transformers,
        );

        scene.file_storage.push((file, self.channel_map.into()));
        let files = vec![PlaylistEntry {
            begin: 0,
            duration,
            idx: scene.file_storage.len() - 1,
        }];
        parent
            .unwrap()
            .add_files_and_transformers(files, transformers, duration, span)
    }
}

#[derive(Default)]
struct ChannelElement {
    channel_id: Option<String>,
    source_id: Option<String>,
    skip: Option<u32>,
    transform: Option<Transform>,
}

impl<'a> Element<'a> for ChannelElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        _span: xml::StrSpan,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(skip) = attributes.get_value("skip") {
            self.skip = Some(parse_attribute(skip)?);
            if let Some(&(name, _)) = attributes.first() {
                return Err(ParseError::new(
                    "No further attributes are allowed in <channel> if \"skip\" is given",
                    name,
                ));
            }
        } else {
            self.channel_id = scene.get_id(attributes)?;
            self.source_id = scene.get_source_id(attributes)?;
            assert!(self.transform.is_none());
            self.transform = parse_transform(attributes)?;
        }
        Ok(())
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element>>,
        _scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        let parent = parent.unwrap();
        let clip = (**parent)
            .as_any_mut()
            .downcast_mut::<ClipElement>()
            .unwrap();
        clip.channels.push(*self);
        Ok(())
    }
}

#[derive(Default)]
struct TransformElement {
    id: Option<String>,
    duration: Option<Seconds>,
    targets: Vec<String>,
    transform: Option<Transform>,
    nodes: Vec<TransformNodeElement>,
}

impl TransformElement {
    fn new() -> TransformElement {
        Default::default()
    }
}

impl<'a> Element<'a> for TransformElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        span: xml::StrSpan,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        self.id = scene.get_id(attributes)?;
        if let Some(apply_to) = attributes.get_value("apply-to") {
            assert!(self.targets.is_empty());
            for s in apply_to.as_str().split_whitespace() {
                // TODO: make sure IDs are valid?
                self.targets.push(s.into());
            }
            if self.targets.is_empty() {
                return Err(ParseError::new(
                    "There must be at least one target ID",
                    apply_to,
                ));
            }
        } else {
            return Err(ParseError::new(
                "Attribute \"apply-to\" is required in <transform>",
                span,
            ));
        }
        self.transform = parse_transform(attributes)?;

        // TODO: allow specifying duration?
        // TODO: if duration is longer than enclosing <par> duration:
        //       do nothing special, this is caught later

        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan,
        _parent_span: xml::StrSpan,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        if name.as_str() == "o" {
            Ok(Box::new(TransformNodeElement::default()))
        } else {
            Err(ParseError::new(
                format!(
                    "Only <o> elements are allowed in <transform>, not <{}>",
                    name.as_str()
                ),
                name,
            ))
        }
    }

    fn close(
        mut self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element>>,
        scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        assert!(!self.targets.is_empty());

        let parent = parent.unwrap();

        let parent_duration_frames =
            if let Some(par) = (**parent).as_any().downcast_ref::<ParElement>() {
                par.duration_frames
            } else {
                None
            };

        if self.nodes.is_empty() {
            self.nodes.push(TransformNodeElement {
                transform: self.transform.unwrap_or_default(),
                ..Default::default()
            });
        } else if self.transform.is_some() {
            return Err(ParseError::new(
                "<o> elements are not allowed if <transform> has transform attributes",
                span,
            ));
        }
        assert!(!self.nodes.is_empty());

        if self.nodes[0].time.unwrap_or_default() != Seconds(0.0) {
            // TODO: This is a temporary restriction until "begin" semantics are sorted out:
            return Err(ParseError::new(
                "The first <transform> node is not allowed to have a time != 0 (for now)",
                span,
            ));
        }

        let transformer = if self.nodes.len() == 1 {
            let node = self.nodes.pop().unwrap();
            if node.closed {
                return Err(ParseError::new(
                    "pos=\"closed\" is not allowed with a single node",
                    span,
                ));
            }
            Box::new(ConstantTransformer {
                id: self.id,
                transform: node.transform,
            }) as Box<dyn Transformer>
        } else {
            let mut positions = Vec::<Vec3>::new();
            let mut times = Vec::<Option<f32>>::new();
            let mut speeds = Vec::<Option<f32>>::new();
            let mut tensions = Vec::<Option<f32>>::new();
            let mut continuities = Vec::<Option<f32>>::new();
            let mut biases = Vec::<Option<f32>>::new();
            let mut closed = false;

            for node in self.nodes {
                times.push(node.time.map(|t| t.0));
                if node.closed {
                    // This was checked during parsing:
                    assert!(node.speed.is_none());
                    assert!(node.tension.is_none());
                    assert!(node.continuity.is_none());
                    assert!(node.bias.is_none());
                    assert!(!closed);
                    closed = true;
                } else {
                    let position = node.transform.translation;
                    // TODO: remove this requirement
                    assert!(position.is_some());
                    positions.push(position.unwrap());
                    speeds.push(node.speed);
                    tensions.push(node.tension);
                    continuities.push(node.continuity);
                    biases.push(node.bias);
                }

                // TODO: handle rotation, volume, ...
            }
            if let Some(last_time) = times.last().unwrap() {
                if self.duration.is_some() {
                    return Err(ParseError::new(
                        "Last node cannot have \"time\" \
                         if <transform> has explicit duration (for now)",
                        span,
                    ));
                }
                // TODO: handle "begin" time?
                self.duration = Some(Seconds(*last_time));
            } else {
                *times.last_mut().unwrap() = if self.duration.is_some() {
                    self.duration.map(|t| t.0)
                } else if let Some(duration_frames) = parent_duration_frames {
                    Some(frames2seconds(duration_frames, scene.samplerate).0)
                } else {
                    return Err(ParseError::new("Unable to infer time of last node", span));
                };
            }
            if !closed {
                if tensions.remove(0).is_some()
                    || continuities.remove(0).is_some()
                    || biases.remove(0).is_some()
                {
                    return Err(ParseError::new(
                        "tension/continuity/bias are not allowed in the first node \
                         (except if pos=\"closed\")",
                        span,
                    ));
                }
                if tensions.pop().unwrap().is_some()
                    || continuities.pop().unwrap().is_some()
                    || biases.pop().unwrap().is_some()
                {
                    return Err(ParseError::new(
                        "tension/continuity/bias are not allowed in the last node",
                        span,
                    ));
                }
            }
            assert!(tensions.len() == continuities.len());
            assert!(tensions.len() == biases.len());
            let tcb: Vec<_> = (0..tensions.len())
                .map(|i| {
                    [
                        tensions[i].unwrap_or_default(),
                        continuities[i].unwrap_or_default(),
                        biases[i].unwrap_or_default(),
                    ]
                })
                .collect();
            Box::new(SplineTransformer {
                id: self.id,
                spline: AsdfPosSpline::new(&positions, &times, &speeds, &tcb, closed).map_err(
                    |e| ParseError::new(format!("Error creating ASDF spline: {}", e), span),
                )?,
                samplerate: scene.samplerate,
            }) as Box<dyn Transformer>
        };

        if let Some(duration) = self
            .duration
            .map(|t| seconds2frames(t, scene.samplerate))
            .or(parent_duration_frames)
        {
            let mut transformers = Vec::new();
            scene.add_transformer(transformer, 0, duration, &self.targets, &mut transformers);
            parent.add_files_and_transformers(vec![], transformers, duration, span)
        } else {
            Err(ParseError::new(
                "Unable to infer <transform> duration",
                span,
            ))
        }
    }
}

#[derive(Default)]
struct TransformNodeElement {
    time: Option<Seconds>,
    closed: bool,
    transform: Transform,
    speed: Option<f32>,
    tension: Option<f32>,
    continuity: Option<f32>,
    bias: Option<f32>,
}

impl<'a> Element<'a> for TransformNodeElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
        span: xml::StrSpan,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(time_value) = attributes.get_value("time") {
            self.time = Some(parse_attribute(time_value)?);
        }
        let mut position = None;
        if let Some(pos_value) = attributes.get_value("pos") {
            // TODO: allow flanking whitespace?
            if pos_value.as_str() == "closed" {
                self.closed = true;
            } else {
                position = Some(parse_pos(pos_value)?);
            }
        }
        self.transform = parse_transform(attributes)?.unwrap_or_default();
        if !self.closed && position.is_some() {
            assert!(self.transform.translation.is_none());
            self.transform.translation = position;
        }

        if let Some((speed_key, speed_value)) = attributes.get_item("speed") {
            if self.closed {
                return Err(ParseError::new(
                    "\"speed\" is not allowed when pos=\"closed\"",
                    speed_key,
                ));
            }
            // TODO: disallow negative values?
            self.speed = Some(parse_attribute(speed_value)?);
        }
        // TODO: code re-use?
        if let Some((tension_key, tension_value)) = attributes.get_item("tension") {
            if self.closed {
                return Err(ParseError::new(
                    "\"tension\" is not allowed when pos=\"closed\"",
                    tension_key,
                ));
            }
            self.tension = Some(parse_attribute(tension_value)?);
        }
        if let Some((continuity_key, continuity_value)) = attributes.get_item("continuity") {
            if self.closed {
                return Err(ParseError::new(
                    "\"continuity\" is not allowed when pos=\"closed\"",
                    continuity_key,
                ));
            }
            self.continuity = Some(parse_attribute(continuity_value)?);
        }
        if let Some((bias_key, bias_value)) = attributes.get_item("bias") {
            if self.closed {
                return Err(ParseError::new(
                    "\"bias\" is not allowed when pos=\"closed\"",
                    bias_key,
                ));
            }
            self.bias = Some(parse_attribute(bias_value)?);
        }

        if !self.closed && self.transform.translation.is_none() {
            return Err(ParseError::new("\"pos\" must be given (for now)", span));
        }
        Ok(())
    }

    fn close(
        self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element>>,
        _scene: &mut SceneInitializer<'a>,
    ) -> Result<(), ParseError> {
        let parent = parent.unwrap();
        let parent = (**parent)
            .as_any_mut()
            .downcast_mut::<TransformElement>()
            .unwrap();
        if let Some(previous) = parent.nodes.last() {
            if previous.closed {
                return Err(ParseError::new(
                    "No further <o> element allowed after pos=\"closed\"",
                    span,
                ));
            }
        }
        parent.nodes.push(*self);
        Ok(())
    }
}

fn child_in_container<'a>(
    name: xml::StrSpan,
    parent_span: xml::StrSpan,
) -> Result<Box<dyn Element<'a>>, ParseError> {
    match name.as_str() {
        "seq" => Ok(Box::new(SeqElement::new())),
        "par" => Ok(Box::new(ParElement::new())),
        "clip" => Ok(Box::new(ClipElement::new())),
        "transform" => Ok(Box::new(TransformElement::new())),
        _ => Err(ParseError::new(
            format!(
                "No <{}> element allowed in <{}>",
                name.as_str(),
                parent_span.as_str()
            ),
            name,
        )),
    }
}

/// Convenience function for automatic deduction of return type.
fn parse_attribute<T>(span: xml::StrSpan) -> Result<T, ParseError>
where
    T: ParseAttribute,
{
    T::parse_attribute(span)
}

trait ParseAttribute {
    fn parse_attribute(span: xml::StrSpan) -> Result<Self, ParseError>
    where
        Self: Sized;
}

impl ParseAttribute for NonZeroU64 {
    fn parse_attribute(span: xml::StrSpan) -> Result<Self, ParseError> {
        NonZeroU64::from_str(span.as_str()).map_err(|e| {
            ParseError::new(
                format!("error parsing attribute as positive integer: {}", e),
                span,
            )
        })
    }
}

impl ParseAttribute for u32 {
    fn parse_attribute(span: xml::StrSpan) -> Result<Self, ParseError> {
        u32::from_str(span.as_str()).map_err(|e| {
            ParseError::new(
                format!("error parsing attribute as non-negative integer: {}", e),
                span,
            )
        })
    }
}

impl ParseAttribute for f32 {
    fn parse_attribute(span: xml::StrSpan) -> Result<Self, ParseError> {
        f32::from_str(span.as_str()).map_err(|e| {
            ParseError::new(
                format!("error parsing attribute as decimal value(s): {}", e),
                span,
            )
        })
    }
}

impl ParseAttribute for Seconds {
    fn parse_attribute(span: xml::StrSpan) -> Result<Self, ParseError> {
        Seconds::from_str(span.as_str())
            .map_err(|e| ParseError::new(format!("invalid time value: {}", e), span))
    }
}
