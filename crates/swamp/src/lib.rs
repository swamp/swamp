/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMap;
use swamp_dep_loader::DepLoaderError;
pub mod prelude;

use swamp_analyzer::Program;
use swamp_compile::CompileOptions;
use swamp_error_report::ScriptResolveError;
use swamp_semantic::prelude::Error;
use swamp_semantic::SemanticError;

#[derive(Debug)]
pub enum ScriptError {
    Error(Error),
    DepLoaderError(DepLoaderError),
}

impl From<Error> for ScriptError {
    fn from(err: Error) -> Self {
        Self::Error(err)
    }
}

impl From<DepLoaderError> for ScriptError {
    fn from(err: DepLoaderError) -> Self {
        Self::DepLoaderError(err)
    }
}

#[derive(Debug)]
pub enum LoaderErr {
    CouldNotLoad,
    SemanticError(SemanticError),
    AnalyzerError(Error),
}

impl From<Error> for LoaderErr {
    fn from(value: Error) -> Self {
        Self::AnalyzerError(value)
    }
}
pub fn compile_and_analyze(
    module_path: &[String],
    source_map: &mut SourceMap,
) -> Result<Program, ScriptResolveError> {
    let options = CompileOptions {
        show_semantic: false,
        show_modules: false,
        show_types: false,
        show_errors: true,
        show_warnings: true,
        show_hints: false,
        show_information: false,
    };
    swamp_compile::bootstrap_and_compile(source_map, module_path, &options)
}
