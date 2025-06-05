use pico_args::Arguments;
use std::env;
use std::path::PathBuf;
use std::process::ExitCode;
use swamp_test_runner::{TestRunOptions, init_logger, run_tests};

fn print_usage() {
    eprintln!(
        "Usage: swamp-test --path <path> [--filter <pattern>]\n\n\
         Options:\n\
         \t--path <path>\t  (required) directory or file to load tests from\n\
         \t--filter <pattern>\t(optional) only run tests matching this substring\n\
         \t-h, --help \t  print this help and exit"
    );
}

fn main() -> ExitCode {
    let mut args = Arguments::from_env();

    if args.contains(["-h", "--help"]) {
        print_usage();
        return ExitCode::from(1);
    }

    let test_path: PathBuf = match args.opt_value_from_str::<_, String>("--path") {
        Ok(Some(path_str)) => PathBuf::from(path_str),
        Ok(None) => env::current_dir().unwrap_or_else(|e| {
            eprintln!("error: failed to get current directory: {e}");
            std::process::exit(1);
        }),
        Err(e) => {
            eprintln!("error: invalid value for `--path`: {e}\n");
            print_usage();
            return ExitCode::from(1);
        }
    };

    let filter_pattern: Option<String> = match args.opt_value_from_str("--filter") {
        Ok(opt) => opt,
        Err(e) => {
            eprintln!("error: invalid value for `--filter`: {e}\n");
            print_usage();
            return ExitCode::from(1);
        }
    };

    let repeat_count: usize = match args.opt_value_from_str::<_, usize>("--repeat") {
        Ok(Some(n)) if n >= 1 => n,
        Ok(Some(0)) => {
            eprintln!("error: `--repeat` must be at least 1\n");
            print_usage();
            return ExitCode::from(1);
        }
        Err(e) => {
            eprintln!("error: invalid value for `--repeat`: {e}\n");
            print_usage();
            return ExitCode::from(1);
        }
        _ => 1,
    };

    // Check for any unexpected positional arguments
    let leftover = args.finish();
    if !leftover.is_empty() {
        eprintln!("error: unexpected positional arguments: {leftover:?}\n");
        print_usage();
        return ExitCode::from(1);
    }

    init_logger();

    let test_result = run_tests(
        &test_path,
        &TestRunOptions {
            should_run: true,
            print_output: false,
            iteration_count: repeat_count,
            debug_output: false,
            debug_opcodes: false,
            debug_operations: false,
            debug_stats: true,
            show_semantic: false,
            show_disasm: false,
            show_modules: false,
        },
        &filter_pattern.unwrap_or_default(),
    );

    if !test_result.succeeded() {
        eprintln!("Some tests failed.");
        return ExitCode::from(1);
    }

    ExitCode::SUCCESS
}
