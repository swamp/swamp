/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod analyze;
mod dep;
pub mod loader;
pub mod parse;
pub mod prelude;
pub mod script_resolve;
pub mod semantic;

use eira::{Color, Kind, Pos, PosSpan, SourceLines};
use std::fmt::Display;
use std::io;
use std::io::{Write, stderr};
use std::path::Path;
use swamp_dep_loader::{DepLoaderError, DependencyError};
use swamp_program_analyzer::LoaderErr;

use source_map_cache::{FileId, SourceMap};
use source_map_node::Span;
use swamp_semantic::err;

#[derive(Debug)]
pub enum ScriptResolveError {
    AnalyzerError(err::Error),
    DepLoaderError(DepLoaderError),
    DependencyError(DependencyError),
    LoaderError(LoaderErr),
}

impl From<DependencyError> for ScriptResolveError {
    fn from(err: DependencyError) -> Self {
        Self::DependencyError(err)
    }
}

impl From<err::Error> for ScriptResolveError {
    fn from(err: err::Error) -> Self {
        Self::AnalyzerError(err)
    }
}

impl From<DepLoaderError> for ScriptResolveError {
    fn from(err: DepLoaderError) -> Self {
        Self::DepLoaderError(err)
    }
}

impl From<LoaderErr> for ScriptResolveError {
    fn from(err: LoaderErr) -> Self {
        Self::LoaderError(err)
    }
}

// -------------------------------------------

pub struct SourceLinesWrap<'a> {
    pub file_id: FileId,
    pub source_map: &'a SourceMap,
}

impl SourceLines for SourceLinesWrap<'_> {
    fn get_line(&self, line_number: usize) -> Option<&str> {
        self.source_map.get_source_line(self.file_id, line_number)
    }
}

pub struct Report<C> {
    config: Builder<C>,
}

impl<C: Display + Clone> Report<C> {
    pub fn build(kind: Kind, code: C, error_name: &str, primary_span: &Span) -> Builder<C> {
        Builder {
            primary_span: primary_span.clone(),
            kind,
            error_code: code,
            error_name: error_name.to_string(),
            labels: vec![],
            note: None,
            error_module: String::new(),
        }
    }

    pub const fn new(config: Builder<C>) -> Self {
        Self { config }
    }

    /// # Errors
    ///
    pub fn print(
        &self,
        source_map: &SourceMap,
        current_dir: &Path,
        mut writer: impl Write,
    ) -> io::Result<()> {
        let header = eira::Header {
            header_kind: self.config.kind,
            code: self.config.error_code.clone(),
            code_prefix: self.config.error_module.clone(),
            message: self.config.error_name.clone(),
        };
        header.write(&mut writer)?;
        let primary_span = &self.config.primary_span;
        if primary_span.file_id == 0 {
            eprintln!("{}", format!("header {} {}", header.message, header.code));
        }
        if primary_span.file_id != 0 {
            let (row, col) = source_map
                .get_span_location_utf8(primary_span.file_id, primary_span.offset as usize);
            let filename = source_map.get_relative_path_to(primary_span.file_id, current_dir)?;

            eira::FileSpanMessage::write(
                filename.to_str().unwrap(),
                &PosSpan {
                    pos: Pos { x: col, y: row },
                    length: primary_span.length as usize,
                },
                &mut writer,
            )?;

            let mut source_file_section = eira::SourceFileSection::new();
            for label in &self.config.labels {
                let (row, col) = source_map
                    .get_span_location_utf8(label.span.file_id, label.span.offset as usize);

                source_file_section.labels.push(eira::Label {
                    start: Pos { x: col, y: row },
                    character_count: label.span.length as usize,
                    text: label.description.clone(),
                    color: Color::default(),
                });
            }

            if self.config.labels.is_empty() {
                source_file_section.labels.push(eira::Label {
                    start: Pos { x: col, y: row },
                    character_count: primary_span.length as usize,
                    text: self.config.error_name.clone(),
                    color: Color::default(),
                });
            }

            source_file_section.layout();

            let source_line_wrap = SourceLinesWrap {
                file_id: primary_span.file_id,
                source_map,
            };
            source_file_section.draw(&source_line_wrap, &mut writer)?;
        }

        if let Some(found_note) = &self.config.note {
            let header = eira::Header {
                header_kind: Kind::Note,
                code: 100,
                code_prefix: String::new(),
                message: found_note.to_string(),
            };
            header.write(&mut writer)?;
        }

        Ok(())
    }
}

pub struct Label {
    pub span: Span,
    pub description: String,
}

pub struct Builder<C> {
    pub primary_span: Span,
    pub kind: Kind,
    pub error_code: C,
    pub error_name: String,
    pub error_module: String,
    pub labels: Vec<Label>,
    pub note: Option<String>,
}

impl<C: Display + Clone> Builder<C> {
    #[must_use]
    pub fn with_label(mut self, label: &str, span: Span) -> Self {
        let l = Label {
            span,
            description: label.to_string(),
        };

        self.labels.push(l);
        self
    }

    #[must_use]
    pub fn with_note(mut self, note: &str) -> Self {
        self.note = Some(note.to_string());
        self
    }

    pub const fn build(self) -> Report<C> {
        Report::new(self)
    }
}

pub fn build_and_print(builder: Builder<usize>, source_map: &SourceMap, current_dir: &Path) {
    let report = builder.build();
    report.print(source_map, current_dir, stderr()).unwrap();
}
