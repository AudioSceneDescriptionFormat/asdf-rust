use std::io;
use std::iter;

use xmlparser as xml;

#[derive(thiserror::Error, Debug)]
#[error("{msg}\n---{context}\n---")]
pub struct ParseError {
    msg: String,
    context: String,
    source: Option<Box<dyn std::error::Error>>,
}

impl ParseError {
    pub fn new(msg: impl Into<String>, span: xml::StrSpan<'_>) -> ParseError {
        // TODO: make configurable?
        const LINES_ABOVE: usize = 9;
        const MARKER: char = '^';

        let source = span.full_str();
        let mut context = String::new();

        let last_char_idx = source[..span.end()]
            .char_indices()
            .next_back()
            .map(|(idx, _)| idx)
            .unwrap_or(0);
        // Look forwards for end of line:
        let context_end = source[last_char_idx..]
            .match_indices('\n')
            .next()
            .map(|(idx, _)| last_char_idx + idx + 1)
            .unwrap_or_else(|| source.len());
        // Look backwards for preceding lines (to get some context for the error):
        let mut after_line_breaks: Vec<_> = source[..span.start()]
            .rmatch_indices('\n')
            .map(|(idx, _)| idx + 1) // Index following the line break
            .chain(iter::once(0)) // Add index 0 in case we reach the beginning
            .take(LINES_ABOVE + 1)
            .collect();
        after_line_breaks.reverse();
        for (&a, &b) in after_line_breaks
            .iter()
            .zip(after_line_breaks.iter().skip(1))
        {
            context.push('\n');
            context.push_str(&source[a..(b - 1)]);
        }
        let mut line_start = after_line_breaks.into_iter().next_back().unwrap_or(0);
        while line_start < span.end() {
            context.push('\n');
            let line_end = source[line_start..context_end]
                .match_indices('\n')
                .next()
                .map(|(idx, _)| line_start + idx + 1)
                .unwrap_or(context_end);
            // NB: Tabs are not shown properly, but at least the markers are at correct positions:
            context.push_str(&source[line_start..line_end].replace('\t', " "));

            // NB: The marker position doesn't consider Unicode combining characters nor fullwidth
            //     characters, but that would probably be overkill here ...
            let marker_start = if (line_start..line_end).contains(&span.start()) {
                context.extend(
                    iter::repeat(' ').take(source[line_start..span.start()].chars().count()),
                );
                span.start()
            } else {
                line_start
            };
            let marker_end = if (line_start..line_end).contains(&span.end()) {
                span.end()
            } else {
                line_end
            };
            let chars = source[marker_start..marker_end].chars().count().max(1);
            context.extend(iter::repeat(MARKER).take(chars));
            line_start = line_end;
        }
        ParseError {
            msg: msg.into(),
            context,
            source: None,
        }
    }

    pub fn from_source(
        source: impl Into<Box<dyn std::error::Error>>,
        msg: impl Into<String>,
        span: xml::StrSpan<'_>,
    ) -> ParseError {
        let mut e = ParseError::new(msg, span);
        e.source = Some(source.into());
        e
    }
}

#[derive(thiserror::Error, Debug)]
pub enum IntegrityError {
    #[error("Non-existing ID used in \"apply-to\": {:?}", .0)]
    NonExistingId(String),
    #[error("Multiple rotations applying to ID {:?}", .0)]
    MultipleRotations(String),
    #[error("\"apply-to\" cycle involving ID {:?}", .0)]
    CyclicDependency(String),
}

#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub enum LoadError {
    ReadFile(#[from] io::Error),
    Tokenize(#[from] xml::Error),
    Parse(#[from] ParseError),
    CheckIntegrity(#[from] IntegrityError),
}
