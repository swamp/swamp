/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Report;
use eira::Kind;
use source_map_cache::SourceMap;
use source_map_node::Span;
use std::io::stderr;
use std::path::Path;
use swamp_dep_loader::{DepLoaderError, DependencyError, ParseRootError};

use crate::Builder;
use crate::parse::build_parser_error;

#[must_use]
pub fn build_dependency_error(err: &DependencyError) -> Builder<usize> {
    let span = &Span::default();
    match err {
        DependencyError::CircularDependency(_err) => {
            Report::build(Kind::Error, 10, "circular dependency", span)
        }
        DependencyError::ParseRootError(parse_root) => match parse_root {
            ParseRootError::ParserError(err) => build_parser_error(err),
        },
        DependencyError::ReadFileError(err) => {
            Report::build(Kind::Error, 20, &format!("read file error {err}"), span)
        }
    }
}

pub fn build_dep_loader_error(err: &DepLoaderError) -> Builder<usize> {
    match err {
        DepLoaderError::DependencyError(dependency_err) => build_dependency_error(dependency_err),
    }
}

/// # Panics
///
pub fn show_dependency_error(err: &DependencyError, source_map: &SourceMap, current_dir: &Path) {
    let builder = build_dependency_error(err);
    let report = builder.build();
    report.print(source_map, current_dir, stderr()).unwrap();
}
