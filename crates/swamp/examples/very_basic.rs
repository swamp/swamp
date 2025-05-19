use std::path::Path;

use swamp_test_runner::{TestRunOptions, run_tests};

pub fn very_basic(test_dir: &Path) {
    run_tests(
        test_dir,
        TestRunOptions {
            should_run: true,
            print_output: false,
            iteration_count: 2,
            debug_output: false,
        },
    )
}

fn main() {
    very_basic(Path::new("../tests/fixtures/basic"));
}
