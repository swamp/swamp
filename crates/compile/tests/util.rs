/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMapWrapper;
use swamp_analyzer::Program;
use swamp_compile::compile_string;
use swamp_pretty_print::{SourceMapDisplay, SymbolTableDisplay};
use tracing::info;
use swamp_dep_loader::RunMode;

fn internal_compile(script: &str) -> Program {
    let (program, test_module, source_map) = compile_string(script, &RunMode::Development);

    let source_map_lookup = SourceMapWrapper {
        source_map: &source_map,
        current_dir: Default::default(),
    };
    let pretty_printer = SourceMapDisplay {
        source_map: &source_map_lookup,
    };

    let symbol_table_display = SymbolTableDisplay {
        symbol_table: &test_module.symbol_table,
        source_map_display: &pretty_printer,
    };

    info!(%symbol_table_display, "symbol table");

    program
}

/// # Panics
/// Intentionally panics if error message is not equal to the error encountered.
pub fn should_fail(script: &str, expected_error_message: &str) {
    let program = internal_compile(script);
    assert!(
        !program.state.errors.is_empty(),
        "should have failed but didn't"
    );

    let first_error = program.state.errors.first().unwrap();
    let error_string = first_error.kind.to_string();

    assert_eq!(error_string, expected_error_message.trim());
}

/// # Panics
/// Intentionally panics if error message is not equal to the error encountered.
pub fn should_hint(script: &str, expected_error_message: &str) {
    let program = internal_compile(script);
    assert!(
        program.state.errors.is_empty(),
        "should have worked but didn't"
    );

    let first_hint = program.state.hints.first().unwrap();
    let error_string = first_hint.kind.to_string();

    assert_eq!(error_string, expected_error_message.trim());
}

/// # Panics
/// Intentionally panics if output is not the same as the `expected_output`
pub fn should_work(script: &str) {
    let program = internal_compile(script);
    if !program.state.errors.is_empty() {
        let first_error = program.state.errors.first().unwrap();

        panic!("should have worked but failed {first_error:?}");
    }
}
