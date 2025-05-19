use std::path::Path;

use swamp_test_runner::{TestRunOptions, init_logger, run_tests};

pub fn very_basic(test_dir: &Path) {
    init_logger();

    run_tests(
        test_dir,
        TestRunOptions {
            should_run: true,
            print_output: false,
            iteration_count: 8,
            debug_output: false,
            debug_opcodes: false,
        },
    )
}

fn main() {
    very_basic(Path::new("../tests/fixtures/basic"));
}
