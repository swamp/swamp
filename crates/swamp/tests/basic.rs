use std::path::{Path, PathBuf};
use swamp_test_runner::{TestRunOptions, run_tests};

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
    let result = run_tests(
        &test_dir,
        &TestRunOptions {
            should_run: true,
            print_output: false,
            iteration_count: 32,
            debug_output: false,
            debug_opcodes: false,
        },
        "",
    );
    assert!(result.failed_tests.is_empty());
}
