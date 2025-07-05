/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::ScriptResolveError;
use crate::analyze::build_analyzer_error;
use crate::dep::build_dep_loader_error;
use crate::dep::build_dependency_error;
use crate::loader::build_loader_error;
use source_map_cache::SourceMap;
use std::io::stderr;
use std::path::Path;

use crate::Builder;
#[must_use]
pub fn build_script_error(err: &ScriptResolveError) -> Builder<usize> {
    match err {
        ScriptResolveError::AnalyzerError(err) => build_analyzer_error(err),
        ScriptResolveError::DepLoaderError(err) => build_dep_loader_error(err),
        ScriptResolveError::DependencyError(err) => build_dependency_error(err),
        ScriptResolveError::LoaderError(err) => build_loader_error(err),
    }
}

/// # Panics
///
pub fn show_script_resolve_error(
    err: &ScriptResolveError,
    source_map: &SourceMap,
    current_dir: &Path,
) {
    let builder = build_script_error(err);
    let report = builder.build();
    report.print(source_map, current_dir, stderr()).unwrap();
}
