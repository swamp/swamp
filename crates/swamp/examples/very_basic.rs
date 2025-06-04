use std::path::Path;

use swamp_test_runner::{TestRunOptions, init_logger, run_tests};

/// # Panics
/// if any test fails
pub fn very_basic(test_dir: &Path) {
    init_logger();

    let test_result = run_tests(
        test_dir,
        &TestRunOptions {
            should_run: true,
            print_output: false,
            iteration_count: 2,
            debug_output: false,
            debug_opcodes: false,
            debug_operations: false,
            debug_stats: true,
        },
        "",
    );

    assert!(test_result.succeeded());
}

fn main() {
    very_basic(Path::new("../tests/fixtures/basic"));
}
