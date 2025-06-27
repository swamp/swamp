/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use std::path::PathBuf;
use swamp_test_runner::{StepBehavior, TestRunOptions, run_tests};

#[must_use]
pub fn get_fixture_dir(sub_dirs: &[&str]) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("tests");
    path.push("fixtures");
    for sub_dir in sub_dirs {
        path.push(sub_dir);
    }

    path
}

/// Helper function to run Swamp tests with minimal boilerplate
///
/// # Arguments
/// * `test_filter` - The test filter (e.g., "`with::`", "`stack::init_and_push`", "`intrinsic::`")
/// * `print_output` - Whether to print test output (useful for debugging)
///
/// # Examples
/// ```rust
/// run_swamp_test("with::");                    // Run all with statement tests
/// run_swamp_test("stack::init_and_push");     // Run specific test
/// run_swamp_test("intrinsic::", true);        // Run with output for debugging
/// ```
fn run_swamp_test(test_filter: &str, print_output: bool) {
    let test_dir = get_fixture_dir(&["basic"]);
    let result = run_tests(
        &test_dir,
        &TestRunOptions {
            should_run: true,
            print_output,
            iteration_count: 1,
            debug_output: false,
            debug_opcodes: false,
            debug_operations: false,
            debug_stats: false,
            show_semantic: false,
            show_assembly: false,
            show_modules: false,
            show_types: false,
            step_behaviour: StepBehavior::ResumeExecution,
        },
        test_filter,
        "lib",
    );
    assert!(
        result.failed_tests.is_empty(),
        "Swamp test '{test_filter}' failed"
    );
}

fn run_swamp_test_quiet(test_filter: &str) {
    run_swamp_test(test_filter, false);
}

fn run_swamp_test_verbose(test_filter: &str) {
    run_swamp_test(test_filter, true);
}
/*

#[test_log::test]
fn very_basic() {
    run_swamp_test_quiet("stack::init_and_push");
}

#[test_log::test]
fn with_statement_basic_borrow() {
    run_swamp_test_quiet("with::basic_borrow");
}

#[test_log::test]
fn with_statement_all_variants() {
    run_swamp_test_quiet("with::");
}

#[test_log::test]
fn test_arithmetic() {
    run_swamp_test_quiet("arithmetic::");
}

#[test_log::test]
fn test_bool_operations() {
    run_swamp_test_quiet("bool::");
}

#[test_log::test]
fn test_enum_functionality() {
    run_swamp_test_quiet("enum::");
}

#[test_log::test]
fn test_guard_statements() {
    run_swamp_test_quiet("guard::");
}

#[test_log::test]
fn test_intrinsic_calls() {
    run_swamp_test_quiet("intrinsic::");
}

#[test_log::test]
fn test_logical_operations() {
    run_swamp_test_quiet("logical::");
}

#[test_log::test]
fn test_map_operations() {
    run_swamp_test_quiet("map::");
}

#[test_log::test]
fn test_option_handling() {
    run_swamp_test_quiet("option::");
}

#[test_log::test]
fn test_string_operations() {
    run_swamp_test_quiet("string::");
}

#[test_log::test]
fn test_struct_functionality() {
    run_swamp_test_quiet("struct::");
}

#[test_log::test]
fn test_tuple_operations() {
    run_swamp_test_quiet("tuple::");
}

#[test_log::test]
fn test_vec_operations() {
    run_swamp_test_quiet("vec::");
}

#[test_log::test]
fn test_when_expressions() {
    run_swamp_test_quiet("when::");
}

#[test_log::test]
fn test_while_loops() {
    run_swamp_test_quiet("while::");
}

#[test_log::test]
fn test_struct_tests() {
    run_swamp_test_quiet("struct::");
}

#[test_log::test]
fn test_constant_tests() {
    run_swamp_test_quiet("constant::");
}

#[test_log::test]
fn test_rest_operator() {
    run_swamp_test_quiet("rest_operator::");
}
*/
