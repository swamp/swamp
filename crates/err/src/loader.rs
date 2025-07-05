/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Builder;
use crate::Report;
use crate::analyze::build_analyzer_error;
use crate::semantic::build_semantic_error;
use eira::Kind;
use source_map_node::Span;
use swamp_program_analyzer::LoaderErr;

#[must_use]
pub fn build_loader_error(err: &LoaderErr) -> Builder<usize> {
    match err {
        LoaderErr::CouldNotLoad => {
            Report::build(Kind::Error, 140, "could not load", &Span::default())
        }
        LoaderErr::SemanticError(semantic_err) => {
            build_semantic_error(semantic_err, &Span::default())
        }
        LoaderErr::AnalyzerError(analyzer_err) => build_analyzer_error(analyzer_err),
    }
}
