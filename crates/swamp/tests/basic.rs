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

#[test_log::test]
fn very_basic() {
    let test_dir = get_fixture_dir(&["basic"]);
    let module_suffix = "lib";
    let result = run_tests(
        &test_dir,
        &TestRunOptions {
            should_run: true,
            print_output: false,
            iteration_count: 1,
            debug_output: false,
            debug_opcodes: true,
            debug_operations: true,
            debug_stats: false,
            show_semantic: false,
            show_assembly: true,
            show_modules: false,
            step_behaviour: StepBehavior::ResumeExecution,
        },
        "constant::check_constant_map",
        module_suffix,
    );
    assert!(result.failed_tests.is_empty());
}
