/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::path::PathBuf;
use swamp_test_runner::{run_tests_source_map, StepBehavior, TestRunOptions};

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
    let result = run_tests_source_map(
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
            assembly_filter: None,
            show_modules: false,
            show_types: false,
            step_behaviour: StepBehavior::ResumeExecution,
            debug_memory_enabled: false,
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
#[test_log::test]
fn swamp_tests() {
    run_swamp_test_verbose("");
}
