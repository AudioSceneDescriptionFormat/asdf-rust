use std::any::Any;
use std::num::NonZeroU64;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use asdfspline::Spline as _;
use asdfspline::{AsdfPosSpline, AsdfRotSpline, PiecewiseCubicCurve};
use xmlparser as xml;

use crate::audiofile::dynamic::{load_audio_file, AudioFile};
use crate::streamer::FileStreamer;
use crate::transform::{parse_pos, parse_rot, parse_transform, parse_vol, Quat, Transform, Vec3};
use crate::{Source, Transformer, REFERENCE_ID};

use super::error::ParseError;
use super::time::{frames2seconds, seconds2frames, Seconds, XmlTime};
use super::{
    Attributes, ConstantTransformer, EmptyTransformer, GetAttributeValue, Iterations,
    PlaylistEntry, SceneInitializer, SplineTransformer, TransformerInstance,
};
use crate::parse_error;

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
        attributes: &mut Attributes<'_>,
        end_span: xml::StrSpan<'_>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError>;

    fn open_child_element(
        &mut self,
        name: xml::StrSpan<'_>,
        parent_span: xml::StrSpan<'_>,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        parse_error!(
            name,
            "No child elements allowed in <{}>",
            parent_span.as_str()
        )
    }

    fn add_files_and_transformers(
        &mut self,
        _files: Vec<PlaylistEntry>,
        _transformers: Vec<TransformerInstance>,
        _duration: u64,
        _span: xml::StrSpan<'_>,
    ) -> Result<(), ParseError> {
        unreachable!("This has to be implemented for all container elements");
    }

    // NB: "span" is stored in "scene", therefore they must have matching lifetimes.
    fn close(
        self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element<'_>>>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError>;
}

pub struct AsdfElement {
    version: String,
    seq: SeqElement,
    previous_child: String,
}

impl AsdfElement {
    pub fn new(name: xml::StrSpan<'_>) -> Result<AsdfElement, ParseError> {
        if name.as_str() == "asdf" {
            Ok(AsdfElement {
                version: String::new(),
                seq: SeqElement::new(None),
                previous_child: String::new(),
            })
        } else {
            parse_error!(
                name,
                "Expected <asdf> as root element, not <{}>",
                name.as_str()
            )
        }
    }
}

impl<'a> Element<'a> for AsdfElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes<'_>,
        span: xml::StrSpan<'_>,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(value) = attributes.get_value("version") {
            if value.as_str().trim() == "0.4" {
                self.version = "0.4".into();
            } else {
                parse_error!(value, "Only ASDF version 0.4 is currently supported");
            }
        } else {
            parse_error!(span, "\"version\" attribute is required in <asdf> element");
        }
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan<'_>,
        parent_span: xml::StrSpan<'_>,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        let result = match name.as_str() {
            "head" => {
                if self.previous_child.is_empty() {
                    Ok(Box::new(HeadElement::new()) as Box<dyn Element<'_>>)
                } else {
                    parse_error!(name, "No element is allowed before <head>")
                }
            }
            "body" => {
                if self.previous_child.is_empty() || self.previous_child == "head" {
                    Ok(Box::new(BodyElement::new()) as Box<dyn Element<'_>>)
                } else {
                    parse_error!(name, "Only a <head> element is allowed before <body>")
                }
            }
            _ => {
                // <asdf> is an implicit <seq> (but only if there is no <body>):
                if self.previous_child == "body" {
                    parse_error!(name, "No elements are allowed after <body>")
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
        span: xml::StrSpan<'_>,
    ) -> Result<(), ParseError> {
        self.seq
            .add_files_and_transformers(files, transformers, duration, span)
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element<'_>>>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        assert!(parent.is_none());
        scene.transformer_instances = self.seq.transformers;
        scene.streamer = Some(FileStreamer::new(
            self.seq.files,
            scene.file_storage.split_off(0),
            scene.blocksize,
            scene.file_sources.len() as u32,
            scene.buffer_blocks,
            scene.sleeptime,
        ));
        scene.frames = {
            let infinite = scene.live_sources.iter().any(|s| s.transform.is_some());
            if infinite {
                None
            } else {
                Some(self.seq.end)
            }
        };
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
        _attributes: &mut Attributes<'_>,
        _span: xml::StrSpan<'_>,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        // No attributes are allowed
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan<'_>,
        _parent_span: xml::StrSpan<'_>,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        match name.as_str() {
            "meta" => parse_error!(name, "TODO: implement <meta> tags"),
            "source" => Ok(Box::new(SourceElement::new())),
            "reference" => {
                if self.reference {
                    parse_error!(name, "Only one <reference> is allowed")
                } else {
                    self.reference = true;
                    Ok(Box::new(ReferenceElement::new()))
                }
            }
            _ => parse_error!(name, "No <{}> elements allowed in <head>", name.as_str()),
        }
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        _parent: Option<&mut Box<dyn Element<'_>>>,
        _scene: &mut SceneInitializer,
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
        attributes: &mut Attributes<'_>,
        _span: xml::StrSpan<'_>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        let id = scene.get_id(attributes)?;
        // TODO: source without ID is only allowed for live sources!
        let name = attributes.get_value("name").map(|v| v.to_string());
        let model = attributes.get_value("model").map(|v| v.to_string());
        let port = attributes.get_value("port").map(|v| v.to_string());
        let transform = parse_transform(attributes)?;
        let source = Source {
            id,
            name,
            model,
            transform,
        };
        if let Some(port) = port {
            scene.ports.push(port);
            scene.live_sources.push(source);
        } else {
            scene.file_sources.push(source);
        }
        Ok(())
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        _parent: Option<&mut Box<dyn Element<'_>>>,
        _scene: &mut SceneInitializer,
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
        attributes: &mut Attributes<'_>,
        _span: xml::StrSpan<'_>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(id) = attributes.get_value("id") {
            if id.as_str() != REFERENCE_ID {
                parse_error!(id, "Reference ID must be {:?}", REFERENCE_ID);
            }
        }
        scene.reference_transform = parse_transform(attributes)?.unwrap_or_default();
        Ok(())
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        _parent: Option<&mut Box<dyn Element<'_>>>,
        _scene: &mut SceneInitializer,
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
            seq: SeqElement::new(None),
        }
    }
}

impl<'a> Element<'a> for BodyElement {
    fn parse_attributes(
        &mut self,
        _attributes: &mut Attributes<'_>,
        _span: xml::StrSpan<'_>,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        // No attributes are allowed
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan<'_>,
        parent_span: xml::StrSpan<'_>,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        self.seq.open_child_element(name, parent_span)
    }

    fn add_files_and_transformers(
        &mut self,
        files: Vec<PlaylistEntry>,
        transformers: Vec<TransformerInstance>,
        duration: u64,
        span: xml::StrSpan<'_>,
    ) -> Result<(), ParseError> {
        self.seq
            .add_files_and_transformers(files, transformers, duration, span)
    }

    fn close(
        self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element<'_>>>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        Box::new(self.seq).close(span, parent, scene)
    }
}

#[derive(Default)]
struct SeqElement {
    files: Vec<PlaylistEntry>,
    transformers: Vec<TransformerInstance>,
    /// End of current iteration
    end: u64,
    parent_duration: Option<Seconds>,
    iterations: Iterations,
}

impl SeqElement {
    fn new(parent_duration: Option<Seconds>) -> SeqElement {
        SeqElement {
            parent_duration,
            ..Default::default()
        }
    }
}

impl<'a> Element<'a> for SeqElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes<'_>,
        _span: xml::StrSpan<'_>,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(repeat_value) = attributes.get_value("repeat") {
            self.iterations = parse_attribute(repeat_value)?;
        }
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan<'_>,
        parent_span: xml::StrSpan<'_>,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        child_in_container(
            name,
            parent_span,
            self.parent_duration.map(|d| d / self.iterations),
        )
    }

    fn add_files_and_transformers(
        &mut self,
        files: Vec<PlaylistEntry>,
        transformers: Vec<TransformerInstance>,
        duration: u64,
        _span: xml::StrSpan<'_>,
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
        parent: Option<&mut Box<dyn Element<'_>>>,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        let mut files = Vec::new();
        let mut transformers = Vec::new();
        for i in 0..self.iterations.get() {
            files.extend(self.files.iter().map(|entry| PlaylistEntry {
                begin: i * self.end + entry.begin,
                ..*entry
            }));
            transformers.extend(
                self.transformers
                    .iter()
                    .map(|instance| TransformerInstance {
                        begin: i * self.end + instance.begin,
                        ..*instance
                    }),
            );
        }
        parent.unwrap().add_files_and_transformers(
            files,
            transformers,
            self.end * self.iterations.get(),
            span,
        )
    }
}

#[derive(Default)]
struct ParElement {
    parent_duration: Option<Seconds>,
    files: Vec<PlaylistEntry>,
    transformers: Vec<TransformerInstance>,
    /// Duration of a single iteration
    duration_frames: Option<u64>,
    iterations: Iterations,
    samplerate: u32,
}

impl ParElement {
    fn new(parent_duration: Option<Seconds>) -> ParElement {
        ParElement {
            parent_duration,
            ..Default::default()
        }
    }
}

impl<'a> Element<'a> for ParElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes<'_>,
        _span: xml::StrSpan<'_>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(repeat_value) = attributes.get_value("repeat") {
            self.iterations = parse_attribute(repeat_value)?;
        }
        // TODO: better way to access samplerate?
        self.samplerate = scene.samplerate;
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan<'_>,
        parent_span: xml::StrSpan<'_>,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        let parent_duration = self
            .duration_frames
            .map(|f| frames2seconds(f, self.samplerate))
            .or_else(|| self.parent_duration.map(|d| d / self.iterations));
        child_in_container(name, parent_span, parent_duration)
    }

    fn add_files_and_transformers(
        &mut self,
        files: Vec<PlaylistEntry>,
        transformers: Vec<TransformerInstance>,
        duration: u64,
        span: xml::StrSpan<'_>,
    ) -> Result<(), ParseError> {
        if let Some(self_duration) = self.duration_frames {
            if duration > self_duration {
                // NB: This is a limitation to simplify parsing, it may be lifted at some point.
                parse_error!(span, "The first element in <par> must be the longest");
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
        parent: Option<&mut Box<dyn Element<'_>>>,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        let duration = self.duration_frames.unwrap_or_default();
        let mut files = Vec::new();
        let mut transformers = Vec::new();
        for i in 0..self.iterations.get() {
            files.extend(self.files.iter().map(|entry| PlaylistEntry {
                begin: i * duration + entry.begin,
                ..*entry
            }));
            transformers.extend(
                self.transformers
                    .iter()
                    .map(|instance| TransformerInstance {
                        begin: i * duration + instance.begin,
                        ..*instance
                    }),
            );
        }
        parent.unwrap().add_files_and_transformers(
            files,
            transformers,
            duration * self.iterations.get(),
            span,
        )?;
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
        attributes: &mut Attributes<'_>,
        span: xml::StrSpan<'_>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        self.clip_id = scene.get_id(attributes)?;
        self.source_id = scene.get_file_source_id(attributes)?;

        // TODO: check source_id (if non-empty) for source properties
        // TODO: depending on this, the rest may be treated differently
        // NB: only model="point" (default) and model="plane" is allowed for now
        // TODO: allow other source models (e.g. binaural, ambisonics, ...)

        let iterations = if let Some(repeat_value) = attributes.get_value("repeat") {
            parse_attribute(repeat_value)?
        } else {
            Default::default()
        };

        if let Some(file_value) = attributes.get_value("file") {
            let mut path = PathBuf::from(file_value.as_str());

            if path == Path::new("") {
                parse_error!(file_value, "Empty file name");
            }
            if path.is_relative() {
                path = scene.dir.join(path);
            }

            self.file = Some(
                load_audio_file(path, scene.samplerate, iterations).map_err(|e| {
                    ParseError::from_source(e, "Error loading audio file", file_value)
                })?,
            );
        } else {
            parse_error!(span, "\"file\" attribute is required in <clip> element");
        }
        self.transform = parse_transform(attributes)?;
        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan<'_>,
        _parent_span: xml::StrSpan<'_>,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        if name.as_str() == "channel" {
            if self.source_id.is_none() {
                let file_channels = self.file.as_ref().unwrap().channels();
                if self.channel_map.len() >= file_channels as usize {
                    parse_error!(
                        name,
                        "Only as many <channel> elements as channels in the file are allowed ({})",
                        file_channels
                    );
                }
                Ok(Box::<ChannelElement>::default())
            } else {
                parse_error!(
                    name,
                    "No <channel> elements are allowed if <clip> has a \"source\" attribute"
                )
            }
        } else {
            parse_error!(
                name,
                "Only <channel> elements are allowed in <clip>, not <{}>",
                name.as_str()
            )
        }
    }

    fn close(
        mut self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element<'_>>>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        let file = self.file.take().unwrap();
        let duration = file.frames();
        let file_channels = file.channels();
        if self.channels.is_empty() {
            if file_channels != 1 {
                parse_error!(
                    span,
                    "Multi-channel <clip> must have at least one <channel> sub-element"
                );
            }
            // Implicit <channel> element for mono file
            let channel = ChannelElement {
                source_id: self.source_id.clone(),
                ..Default::default()
            };
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
                    parse_error!(
                        span,
                        "Too many <channel> elements: file has only {} channel(s)",
                        file_channels
                    );
                }
                let source_number = if let Some(source_id) = channel.source_id {
                    // Source must already exist
                    scene
                        .file_sources
                        .iter()
                        .filter_map(|s| s.id.as_ref())
                        .position(|id| *id == source_id)
                        .unwrap()
                } else {
                    scene.file_sources.push(Default::default());
                    scene.file_sources.last_mut().unwrap().id = Some(scene.create_new_id());
                    scene.file_sources.len() - 1
                };
                self.channel_map.push(Some(source_number));

                // IDs are required for the parent transformer to work
                let channel_id = channel.channel_id.unwrap_or_else(|| scene.create_new_id());
                self.channel_ids.push(channel_id.clone());

                // <channel> transformer

                let transformer = if let Some(transform) = channel.transform {
                    Box::new(ConstantTransformer {
                        id: Some(channel_id),
                        transform,
                    }) as Box<dyn Transformer>
                } else {
                    Box::new(EmptyTransformer {
                        id: Some(channel_id),
                    }) as Box<dyn Transformer>
                };
                let targets = vec![scene.file_sources[source_number].id.clone().unwrap()];
                let _idx =
                    scene.add_transformer(transformer, 0, duration, &targets, &mut transformers);
            }
        }

        // <clip> transformer that applies to all <channel> elements

        let transformer = if let Some(transform) = self.transform {
            Box::new(ConstantTransformer {
                id: self.clip_id,
                transform,
            }) as Box<dyn Transformer>
        } else {
            Box::new(EmptyTransformer { id: self.clip_id }) as Box<dyn Transformer>
        };
        scene.add_transformer(
            transformer,
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
        attributes: &mut Attributes<'_>,
        _span: xml::StrSpan<'_>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if let Some(skip) = attributes.get_value("skip") {
            self.skip = Some(parse_attribute(skip)?);
            if let Some(&(name, _)) = attributes.first() {
                parse_error!(
                    name,
                    "No further attributes are allowed in <channel> if \"skip\" is given"
                );
            }
        } else {
            self.channel_id = scene.get_id(attributes)?;
            self.source_id = scene.get_file_source_id(attributes)?;
            assert!(self.transform.is_none());
            self.transform = parse_transform(attributes)?;
        }
        Ok(())
    }

    fn close(
        self: Box<Self>,
        _span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element<'_>>>,
        _scene: &mut SceneInitializer,
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
    parent_duration: Option<Seconds>,
    targets: Vec<String>,
    transform: Option<Transform>,
    global_tension: Option<f32>,
    global_continuity: Option<f32>,
    global_bias: Option<f32>,
    nodes: Vec<TransformNodeElement>,
    iterations: Iterations,
}

impl TransformElement {
    fn new(parent_duration: Option<Seconds>) -> TransformElement {
        TransformElement {
            parent_duration,
            ..Default::default()
        }
    }
}

impl<'a> Element<'a> for TransformElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes<'_>,
        span: xml::StrSpan<'_>,
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
                parse_error!(apply_to, "There must be at least one target ID");
            }
        } else {
            parse_error!(span, "Attribute \"apply-to\" is required in <transform>");
        }
        self.transform = parse_transform(attributes)?;

        if let Some((_key, value)) = attributes.get_item("tension") {
            self.global_tension = Some(parse_attribute(value)?);
        }
        if let Some((_key, value)) = attributes.get_item("continuity") {
            self.global_continuity = Some(parse_attribute(value)?);
        }
        if let Some((_key, value)) = attributes.get_item("bias") {
            self.global_bias = Some(parse_attribute(value)?);
        }

        if let Some(repeat_value) = attributes.get_value("repeat") {
            self.iterations = parse_attribute(repeat_value)?;
        }
        if let Some(dur_value) = attributes.get_value("dur") {
            self.duration = match parse_attribute(dur_value)? {
                XmlTime::Seconds(s) => Some(s),
                XmlTime::Fraction(f) => {
                    if let Some(parent_duration) = self.parent_duration {
                        Some(Seconds(f * (parent_duration / self.iterations).0))
                    } else {
                        parse_error!(
                            dur_value,
                            "Could not infer parent duration to resolve percentage"
                        );
                    }
                }
            }
        }

        // TODO: if duration is longer than enclosing <par> duration:
        //       do nothing special, this is caught later

        Ok(())
    }

    fn open_child_element(
        &mut self,
        name: xml::StrSpan<'_>,
        _parent_span: xml::StrSpan<'_>,
    ) -> Result<Box<dyn Element<'a>>, ParseError> {
        if name.as_str() == "o" {
            Ok(Box::<TransformNodeElement>::default())
        } else {
            parse_error!(
                name,
                "Only <o> elements are allowed in <transform>, not <{}>",
                name.as_str()
            )
        }
    }

    fn close(
        mut self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element<'_>>>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        assert!(!self.targets.is_empty());

        let parent = parent.unwrap();

        let max_frames = parent_duration_frames(parent, scene);

        if self.nodes.is_empty() {
            self.nodes.push(TransformNodeElement {
                transform: self.transform.unwrap_or_default(),
                ..Default::default()
            });
        } else if self.transform.is_some() {
            parse_error!(
                span,
                "<o> elements are not allowed if <transform> has transform attributes"
            );
        }
        assert!(!self.nodes.is_empty());

        let transform_duration;
        let mut total_frames: Option<u64> = None;

        let transformer = if self.nodes.len() == 1 {
            let node = self.nodes.pop().unwrap();
            if node.time.is_some() {
                parse_error!(
                    span,
                    "time attribute is not allowed with a single transform node"
                );
            }
            if node.closed_pos || node.closed_rot {
                parse_error!(
                    span,
                    "pos=\"closed\" and rot=\"closed\" are not allowed \
                    with a single transform node"
                );
            }

            if let Some(dur) = self.duration {
                transform_duration = dur;
            // TODO: define total_frames?
            // TODO: check if dur * iterations <= max_frames
            } else if let Some(duration_frames) = max_frames {
                // TODO: code re-use, see below
                if duration_frames == 0 {
                    // TODO: this error message only makes sense inside a <seq> element
                    parse_error!(
                        span,
                        "Zero-length <transform>, try shortening preceding element"
                    );
                }
                let total_duration = frames2seconds(duration_frames, scene.samplerate);
                transform_duration = total_duration / self.iterations;
                total_frames = Some(duration_frames);
            } else if let Some(parent_duration) = self.parent_duration {
                // TODO: check if this makes sense for <seq>!
                transform_duration = parent_duration / self.iterations;
            } else {
                parse_error!(span, "Unable to infer <transform> duration")
            }
            Box::new(ConstantTransformer {
                id: self.id,
                transform: node.transform,
            }) as Box<dyn Transformer>
        } else {
            if let Some(last_time) = self.nodes.last().unwrap().time {
                if self.duration.is_some() {
                    parse_error!(
                        span,
                        "Last node cannot have \"time\" if <transform> has explicit duration"
                    );
                }
                transform_duration = match last_time {
                    XmlTime::Seconds(s) => s,
                    XmlTime::Fraction(_) => {
                        parse_error!(span, "Last node cannot have \"time\" percentage");
                    }
                }
            } else if let Some(dur) = self.duration {
                transform_duration = dur;
            } else if let Some(duration_frames) = max_frames {
                if duration_frames == 0 {
                    parse_error!(
                        span,
                        "Zero-length <transform>, try shortening preceding element"
                    );
                }
                let total_duration = frames2seconds(duration_frames, scene.samplerate);
                transform_duration = total_duration / self.iterations;
                total_frames = Some(duration_frames);
            } else if let Some(parent_duration) = self.parent_duration {
                // TODO: check if this makes sense for <seq>!
                transform_duration = parent_duration / self.iterations;
            } else {
                parse_error!(span, "Unable to infer time of last node");
            }

            let mut positions = Vec::<Vec3>::new();
            let mut rotations = Vec::<Quat>::new();
            let mut volumes = Vec::<f32>::new();
            let mut times_pos = Vec::<Option<f32>>::new();
            let mut times_rot = Vec::<Option<f32>>::new();
            let mut times_vol = Vec::<Option<f32>>::new();
            let mut rot_time_from_pos = Vec::new();
            let mut vol_time_from_pos = Vec::new();
            let mut vol_time_from_rot = Vec::new();
            let mut speeds = Vec::<Option<f32>>::new();
            let mut tcb_pos = Vec::<[f32; 3]>::new();
            let mut tcb_rot = Vec::<[f32; 3]>::new();
            let mut closed_pos = false;
            let mut closed_rot = false;
            let mut closed_vol = false;

            for node in &self.nodes {
                let time = node.time.map(|t| t.resolve(transform_duration).0);

                if node.closed_pos && node.closed_rot {
                    // This has been checked during parsing:
                    assert!(node.tension.is_none());
                    assert!(node.continuity.is_none());
                    assert!(node.bias.is_none());
                }

                if time.is_none() {
                    if node.transform.translation.is_some() {
                        if node.transform.rotation.is_some() {
                            rot_time_from_pos.push((times_rot.len(), times_pos.len()));
                        }
                        if node.transform.volume.is_some() {
                            vol_time_from_pos.push((times_vol.len(), times_pos.len()));
                        }
                    } else if node.transform.rotation.is_some() && node.transform.volume.is_some() {
                        vol_time_from_rot.push((times_vol.len(), times_rot.len()));
                    }
                }

                let tcb = [
                    node.tension.or(self.global_tension).unwrap_or_default(),
                    node.continuity
                        .or(self.global_continuity)
                        .unwrap_or_default(),
                    node.bias.or(self.global_bias).unwrap_or_default(),
                ];

                if node.closed_pos {
                    // This has been checked during parsing:
                    assert!(node.speed.is_none());
                    assert!(!closed_pos);
                    assert!(!closed_rot);
                    assert!(!closed_vol);
                    closed_pos = true;
                    times_pos.push(time);
                } else if let Some(position) = node.transform.translation {
                    times_pos.push(time);
                    positions.push(position);
                    speeds.push(node.speed);
                    tcb_pos.push(tcb);
                } else {
                    assert!(node.speed.is_none());
                }

                if node.closed_rot {
                    assert!(!closed_rot);
                    assert!(!closed_vol);
                    if !node.closed_pos {
                        assert!(!closed_pos);
                    }
                    closed_rot = true;
                    times_rot.push(time);
                } else if let Some(rotation) = node.transform.rotation {
                    times_rot.push(time);
                    rotations.push(rotation);
                    tcb_rot.push(tcb);
                }

                if node.closed_vol {
                    assert!(!closed_vol);
                    if !node.closed_pos {
                        assert!(!closed_pos);
                    }
                    if !node.closed_rot {
                        assert!(!closed_rot);
                    }
                    closed_vol = true;
                    times_vol.push(time);
                } else if let Some(volume) = node.transform.volume {
                    times_vol.push(time);
                    volumes.push(volume);
                }

                // TODO: handle more fields?
            }

            if let Some(first_time) = self.nodes.first().unwrap().time {
                match first_time {
                    XmlTime::Seconds(s) => {
                        // TODO: Temporary restriction until "begin" semantics are sorted out:
                        if s != Seconds(0.0) {
                            parse_error!(
                                span,
                                "The first <transform> node is not allowed \
                                to have a time != 0 (for now)"
                            );
                        }
                    }
                    XmlTime::Fraction(_) => {
                        parse_error!(span, "First node cannot have \"time\" percentage");
                    }
                }
            } else {
                if let [first, ..] = &mut times_pos[..] {
                    *first = Some(0.0);
                }
                if let [first, ..] = &mut times_rot[..] {
                    *first = Some(0.0);
                }
                if let [first, ..] = &mut times_vol[..] {
                    *first = Some(0.0);
                }
            }
            if let [.., last] = &mut times_pos[..] {
                if last.is_none() {
                    *last = Some(transform_duration.0);
                }
            }
            if let [.., last] = &mut times_rot[..] {
                if last.is_none() {
                    *last = Some(transform_duration.0);
                }
            }
            if let [.., last] = &mut times_vol[..] {
                if last.is_none() {
                    *last = Some(transform_duration.0);
                }
            }

            if (positions.is_empty() || !closed_pos) && (rotations.is_empty() || !closed_rot) {
                // NB: if both are empty, no TCB values should exist at all
                let first_node = self.nodes.first().unwrap();
                if first_node.tension.is_some()
                    || first_node.continuity.is_some()
                    || first_node.bias.is_some()
                {
                    parse_error!(
                        span,
                        "tension/continuity/bias are not allowed in the first node \
                         (except if pos=\"closed\" or rot=\"closed\")"
                    );
                }
                let last_node = self.nodes.last().unwrap();
                if last_node.tension.is_some()
                    || last_node.continuity.is_some()
                    || last_node.bias.is_some()
                {
                    // NB: they are not allowed in closed curves either
                    parse_error!(
                        span,
                        "tension/continuity/bias are not allowed in the last node"
                    );
                }
            }
            if !tcb_pos.is_empty() && !closed_pos {
                tcb_pos.remove(0);
                tcb_pos.pop();
            }
            if !tcb_rot.is_empty() && !closed_rot {
                tcb_rot.remove(0);
                tcb_rot.pop();
            }

            let pos_spline = if positions.is_empty() {
                None
            } else {
                let first_node = self.nodes.first().unwrap();
                if first_node.transform.translation.is_none() {
                    parse_error!(
                        span,
                        "If any <transform> node has \"pos\", the first one needs it as well"
                    );
                }
                let last_node = self.nodes.last().unwrap();
                if last_node.transform.translation.is_none() && !closed_pos {
                    parse_error!(
                        span,
                        "If any <transform> node has \"pos\", the last one needs it as well"
                    );
                }
                Some(
                    AsdfPosSpline::new(positions, times_pos, speeds, tcb_pos, closed_pos).map_err(
                        |e| ParseError::from_source(e, "Error creating ASDF position spline", span),
                    )?,
                )
            };

            for (rot_idx, pos_idx) in rot_time_from_pos {
                // Begin and end time have been set above.
                if rot_idx == 0 || rot_idx == times_rot.len() - 1 {
                    continue;
                }
                assert!(times_rot[rot_idx].is_none());
                times_rot[rot_idx] = Some(pos_spline.as_ref().unwrap().grid()[pos_idx]);
            }

            let rot_spline = if rotations.is_empty() {
                None
            } else {
                let first_node = self.nodes.first().unwrap();
                if first_node.transform.rotation.is_none() {
                    parse_error!(
                        span,
                        "If any <transform> node has \"rot\", the first one needs it as well"
                    );
                }
                let last_node = self.nodes.last().unwrap();
                if last_node.transform.rotation.is_none() && !closed_rot {
                    parse_error!(
                        span,
                        "If any <transform> node has \"rot\", the last one needs it as well"
                    );
                }
                Some(
                    AsdfRotSpline::new(rotations, times_rot, tcb_rot, closed_rot).map_err(|e| {
                        ParseError::from_source(e, "Error creating ASDF rotations spline", span)
                    })?,
                )
            };

            for (vol_idx, pos_idx) in vol_time_from_pos {
                // Begin and end time have been set above.
                if vol_idx == 0 || vol_idx == times_vol.len() - 1 {
                    continue;
                }
                assert!(times_vol[vol_idx].is_none());
                times_vol[vol_idx] = Some(pos_spline.as_ref().unwrap().grid()[pos_idx]);
            }
            for (vol_idx, rot_idx) in vol_time_from_rot {
                // Begin and end time have been set above.
                if vol_idx == 0 || vol_idx == times_vol.len() - 1 {
                    continue;
                }
                assert!(times_vol[vol_idx].is_none());
                times_vol[vol_idx] = Some(rot_spline.as_ref().unwrap().grid()[rot_idx]);
            }

            if !times_vol.is_empty() {
                let mut unknown_times = vec![];
                let mut last_known_time = times_vol[0].unwrap();
                for i in 1..times_vol.len() {
                    if let Some(current_time) = times_vol[i] {
                        let diff =
                            (current_time - last_known_time) / (unknown_times.len() + 1) as f32;
                        let mut time = last_known_time;
                        for idx in unknown_times.drain(..) {
                            time += diff;
                            times_vol[idx] = Some(time);
                        }
                        last_known_time = current_time;
                    } else {
                        unknown_times.push(i);
                    }
                }
            }

            let vol_spline = if volumes.is_empty() {
                None
            } else {
                let first_node = self.nodes.first().unwrap();
                if first_node.transform.volume.is_none() {
                    parse_error!(
                        span,
                        "If any <transform> node has \"vol\", the first one needs it as well"
                    );
                }
                let last_node = self.nodes.last().unwrap();
                if last_node.transform.volume.is_none() && !closed_vol {
                    parse_error!(
                        span,
                        "If any <transform> node has \"vol\", the last one needs it as well"
                    );
                }
                Some(
                    PiecewiseCubicCurve::new_piecewise_monotone(
                        volumes,
                        times_vol.into_iter().collect::<Option<Vec<f32>>>().unwrap(),
                        closed_vol,
                    )
                    .map_err(|e| {
                        ParseError::from_source(e, "Error creating ASDF volumes spline", span)
                    })?,
                )
            };

            Box::new(SplineTransformer {
                id: self.id,
                pos_spline,
                rot_spline,
                vol_spline,
                samplerate: scene.samplerate,
            }) as Box<dyn Transformer>
        };
        let frames = seconds2frames(transform_duration, scene.samplerate);
        let total_frames = total_frames.unwrap_or(frames * self.iterations.get());
        let mut transformers = Vec::new();
        scene.add_transformer(transformer, 0, frames, &self.targets, &mut transformers);

        let mut duplicated_transformers = Vec::new();
        for i in 0..self.iterations.get() {
            duplicated_transformers.extend(transformers.iter().map(|instance| {
                TransformerInstance {
                    begin: i * frames + instance.begin,
                    ..*instance
                }
            }));
        }

        parent.add_files_and_transformers(vec![], duplicated_transformers, total_frames, span)
    }
}

#[derive(Default)]
struct TransformNodeElement {
    time: Option<XmlTime>,
    closed_pos: bool,
    closed_rot: bool,
    closed_vol: bool,
    transform: Transform,
    speed: Option<f32>,
    tension: Option<f32>,
    continuity: Option<f32>,
    bias: Option<f32>,
}

impl<'a> Element<'a> for TransformNodeElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes<'_>,
        span: xml::StrSpan<'_>,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        if attributes.is_empty() {
            parse_error!(span, "empty <o> elements are not allowed");
        }
        if let Some(time_value) = attributes.get_value("time") {
            let time = parse_attribute(time_value)?;
            self.time = Some(time);
        }
        let mut pos = None;
        if let Some(pos_value) = attributes.get_value("pos") {
            if pos_value.as_str() == "closed" {
                self.closed_pos = true;
            } else {
                pos = Some(parse_pos(pos_value)?);
            }
        }
        let mut rot = None;
        if let Some(rot_value) = attributes.get_value("rot") {
            if rot_value.as_str() == "closed" {
                self.closed_rot = true;
            } else {
                rot = Some(parse_rot(rot_value)?);
            }
        }
        let mut vol = None;
        if let Some(vol_value) = attributes.get_value("vol") {
            if vol_value.as_str() == "closed" {
                self.closed_vol = true;
            } else {
                vol = Some(parse_vol(vol_value)?);
            }
        }
        self.transform = parse_transform(attributes)?.unwrap_or_default();
        if !self.closed_pos && pos.is_some() {
            assert!(self.transform.translation.is_none());
            self.transform.translation = pos;
        }
        if !self.closed_rot && rot.is_some() {
            assert!(self.transform.rotation.is_none());
            self.transform.rotation = rot;
        }
        if !self.closed_vol && vol.is_some() {
            assert!(self.transform.volume.is_none());
            self.transform.volume = vol;
        }

        if let Some((speed_key, speed_value)) = attributes.get_item("speed") {
            if self.closed_pos {
                parse_error!(speed_key, "\"speed\" is not allowed when pos=\"closed\"");
            } else if self.transform.translation.is_none() {
                parse_error!(speed_key, "\"speed\" is only allowed when \"pos\" is given");
            }
            // TODO: disallow negative values?
            self.speed = Some(parse_attribute(speed_value)?);
        }
        if (self.closed_pos || self.closed_vol) && self.transform.rotation.is_none() {
            self.closed_rot = true;
        }
        if (self.closed_rot || self.closed_vol) && self.transform.translation.is_none() {
            self.closed_pos = true;
        }
        if (self.closed_pos || self.closed_rot) && self.transform.volume.is_none() {
            self.closed_vol = true;
        }
        if let Some((tension_key, tension_value)) = attributes.get_item("tension") {
            if self.closed_pos && self.closed_rot {
                parse_error!(
                    tension_key,
                    "\"tension\" is not allowed when pos=\"closed\" and rot=\"closed\""
                );
            } else if self.transform.translation.is_none() && self.transform.rotation.is_none() {
                parse_error!(
                    tension_key,
                    "\"tension\" is only allowed when \"pos\" and/or \"rot\" are given"
                );
            }
            self.tension = Some(parse_attribute(tension_value)?);
        }
        if let Some((continuity_key, continuity_value)) = attributes.get_item("continuity") {
            if self.closed_pos && self.closed_rot {
                parse_error!(
                    continuity_key,
                    "\"continuity\" is not allowed when pos=\"closed\" and rot=\"closed\""
                );
            } else if self.transform.translation.is_none() && self.transform.rotation.is_none() {
                parse_error!(
                    continuity_key,
                    "\"continuity\" is only allowed when \"pos\" and/or \"rot\" are given"
                );
            }
            self.continuity = Some(parse_attribute(continuity_value)?);
        }
        if let Some((bias_key, bias_value)) = attributes.get_item("bias") {
            if self.closed_pos && self.closed_rot {
                parse_error!(
                    bias_key,
                    "\"bias\" is not allowed when pos=\"closed\" and rot=\"closed\""
                );
            } else if self.transform.translation.is_none() && self.transform.rotation.is_none() {
                parse_error!(
                    bias_key,
                    "\"bias\" is only allowed when \"pos\" and/or \"rot\" are given"
                );
            }
            self.bias = Some(parse_attribute(bias_value)?);
        }
        Ok(())
    }

    fn close(
        self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element<'_>>>,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        let parent = parent.unwrap();
        let parent = (**parent)
            .as_any_mut()
            .downcast_mut::<TransformElement>()
            .unwrap();
        if let Some(previous) = parent.nodes.last() {
            if previous.closed_pos || previous.closed_rot {
                parse_error!(
                    span,
                    "No further <o> element allowed after pos=\"closed\" or rot=\"closed\""
                );
            }
        }
        parent.nodes.push(*self);
        Ok(())
    }
}

#[derive(Default)]
struct WaitElement {
    duration: Option<Seconds>,
    parent_duration: Option<Seconds>,
}

impl WaitElement {
    fn new(parent_duration: Option<Seconds>) -> Self {
        Self {
            parent_duration,
            ..Default::default()
        }
    }
}

impl<'a> Element<'a> for WaitElement {
    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes<'_>,
        span: xml::StrSpan<'_>,
        _scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        // TODO: code reuse
        if let Some(dur_value) = attributes.get_value("dur") {
            self.duration = match parse_attribute(dur_value)? {
                XmlTime::Seconds(s) => Some(s),
                XmlTime::Fraction(f) => {
                    if let Some(parent_duration) = self.parent_duration {
                        Some(Seconds(f * parent_duration.0))
                    } else {
                        parse_error!(
                            dur_value,
                            "Could not infer parent duration to resolve percentage"
                        );
                    }
                }
            }
        } else {
            parse_error!(span, "\"dur\" attribute is required in <wait> element");
        }
        Ok(())
    }

    fn close(
        self: Box<Self>,
        span: xml::StrSpan<'a>,
        parent: Option<&mut Box<dyn Element<'_>>>,
        scene: &mut SceneInitializer,
    ) -> Result<(), ParseError> {
        let parent = parent.unwrap();
        let frames = seconds2frames(self.duration.unwrap(), scene.samplerate);
        parent.add_files_and_transformers(vec![], vec![], frames, span)
    }
}

fn child_in_container<'a>(
    name: xml::StrSpan<'_>,
    parent_span: xml::StrSpan<'_>,
    parent_duration: Option<Seconds>,
) -> Result<Box<dyn Element<'a>>, ParseError> {
    match name.as_str() {
        "seq" => Ok(Box::new(SeqElement::new(parent_duration))),
        "par" => Ok(Box::new(ParElement::new(parent_duration))),
        // TODO: pass parent_duration?
        "clip" => Ok(Box::new(ClipElement::new())),
        "transform" => Ok(Box::new(TransformElement::new(parent_duration))),
        "wait" => Ok(Box::new(WaitElement::new(parent_duration))),
        _ => parse_error!(
            name,
            "No <{}> element allowed in <{}>",
            name.as_str(),
            parent_span.as_str()
        ),
    }
}

fn parent_duration_frames(
    parent: &mut Box<dyn Element<'_>>,
    scene: &mut SceneInitializer,
) -> Option<u64> {
    let parent_any = (**parent).as_any();
    if let Some(par) = parent_any.downcast_ref::<ParElement>() {
        par.duration_frames
    } else if let Some(seq) = parent_any.downcast_ref::<SeqElement>() {
        if let Some(parent_duration) = seq.parent_duration.map(|d| d / seq.iterations) {
            let duration_frames = seconds2frames(parent_duration, scene.samplerate);
            Some(duration_frames.saturating_sub(seq.end))
        } else {
            None
        }
    } else {
        None
    }
}

/// Convenience function for automatic deduction of return type.
fn parse_attribute<T>(span: xml::StrSpan<'_>) -> Result<T, ParseError>
where
    T: ParseAttribute,
{
    T::parse_attribute(span)
}

trait ParseAttribute {
    fn parse_attribute(span: xml::StrSpan<'_>) -> Result<Self, ParseError>
    where
        Self: Sized;
}

impl ParseAttribute for Iterations {
    fn parse_attribute(span: xml::StrSpan<'_>) -> Result<Self, ParseError> {
        NonZeroU64::from_str(span.as_str())
            .map_err(|e| {
                ParseError::from_source(e, "Error parsing attribute as positive integer", span)
            })
            .map(Iterations)
    }
}

impl ParseAttribute for u32 {
    fn parse_attribute(span: xml::StrSpan<'_>) -> Result<Self, ParseError> {
        u32::from_str(span.as_str()).map_err(|e| {
            ParseError::from_source(e, "Error parsing attribute as non-negative integer", span)
        })
    }
}

impl ParseAttribute for f32 {
    fn parse_attribute(span: xml::StrSpan<'_>) -> Result<Self, ParseError> {
        f32::from_str(span.as_str()).map_err(|e| {
            ParseError::from_source(e, "Error parsing attribute as decimal value(s)", span)
        })
    }
}

impl ParseAttribute for XmlTime {
    fn parse_attribute(span: xml::StrSpan<'_>) -> Result<Self, ParseError> {
        XmlTime::from_str(span.as_str())
            .map_err(|e| ParseError::from_source(e, "Invalid time value", span))
    }
}
