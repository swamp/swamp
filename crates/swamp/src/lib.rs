/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMap;
use swamp_dep_loader::{DepLoaderError, RunMode};
pub mod prelude;
use crate::prelude::CompileAndCodeGenOptions;
use swamp_analyzer::Program;
use swamp_compile::CompileOptions;
use swamp_error_report::ScriptResolveError;
use swamp_runtime::CompileAndMaybeCodeGenResult;
use swamp_runtime::prelude::CodeGenOptions;
use swamp_semantic::SemanticError;
use swamp_semantic::prelude::Error;

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

pub fn compile_and_codegen(
    module_path: &[String],
    source_map: &mut SourceMap,
) -> Option<CompileAndMaybeCodeGenResult> {
    let options = CompileAndCodeGenOptions {
        compile_options: CompileOptions {
            show_semantic: false,
            show_modules: false,
            show_errors: false,
            show_warnings: false,
            show_hints: false,
            show_information: false,
            show_types: false,
        },
        code_gen_options: CodeGenOptions {
            show_disasm: false,
            disasm_filter: None,
            show_debug: false,
            show_types: false,
            ignore_host_call: false,
        },
        skip_codegen: false,
        run_mode: RunMode::Deployed,
    };
    swamp_runtime::compile_and_code_gen(source_map, module_path, &options)
}
