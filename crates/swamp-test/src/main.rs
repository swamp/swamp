use pico_args::Arguments;
use std::io::Write;
use std::path::PathBuf;
use std::process::ExitCode;
use std::{env, io};
use swamp_test_runner::{TestRunOptions, init_logger, run_tests};

fn print_usage<W: Write>(mut out: W) {
    let _ = write!(
        out,
        "\
Usage: swamp-test [--path <path>] [--filter <pattern>] [--repeat <count>] [--module module]

Options:
  --path <path>           (optional) directory or file to load tests from
  --filter <pattern>      (optional) only run tests matching this substring
  --repeat <count>        (optional) how many times to rerun each test
  --module <module>       (optional) which module to use, default is `lib`
  -d, --debug             (show a lot of debug)
  -a, --show-assembly     (show assembly)
  -i, --show-instructions (show Swamp VM instructions when running)
  -h, --help              print this help and exit
"
    );
}

fn main() -> ExitCode {
    let mut args = Arguments::from_env();

    if args.contains(["-h", "--help"]) {
        print_usage(io::stdout());
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
            print_usage(io::stderr());
            return ExitCode::from(1);
        }
    };

    let filter_pattern: Option<String> = match args.opt_value_from_str("--filter") {
        Ok(opt) => opt,
        Err(e) => {
            eprintln!("error: invalid value for `--filter`: {e}\n");
            print_usage(io::stderr());
            return ExitCode::from(1);
        }
    };

    let repeat_count: usize = match args.opt_value_from_str::<_, usize>("--repeat") {
        Ok(Some(n)) if n >= 1 => n,
        Ok(Some(0)) => {
            eprintln!("error: `--repeat` must be at least 1\n");
            print_usage(io::stderr());
            return ExitCode::from(1);
        }
        Err(e) => {
            eprintln!("error: invalid value for `--repeat`: {e}\n");
            print_usage(io::stderr());
            return ExitCode::from(1);
        }
        _ => 1,
    };

    let module: Option<String> = match args.opt_value_from_str("--module") {
        Ok(None) => Some("lib".to_string()),
        Ok(opt) => opt,
        Err(e) => {
            eprintln!("error: invalid value for `--module`: {e}\n");
            print_usage(io::stderr());
            return ExitCode::from(1);
        }
    };

    let show_debug = args.contains(["-d", "--debug"]);
    let show_assembly = args.contains(["-a", "--show-assembly"]);
    let show_instructions = args.contains(["-i", "--show-instructions"]);

    // Check for any unexpected positional arguments
    let leftover = args.finish();
    if !leftover.is_empty() {
        eprintln!("error: unexpected positional arguments: {leftover:?}\n");
        print_usage(io::stderr());
        return ExitCode::from(1);
    }

    init_logger();

    let test_result = run_tests(
        &test_path,
        &TestRunOptions {
            should_run: true,
            print_output: false,
            iteration_count: repeat_count,
            debug_output: show_debug,
            debug_opcodes: show_debug | show_instructions,
            debug_operations: show_debug,
            debug_stats: true,
            show_semantic: false,
            show_disasm: show_debug | show_assembly,
            show_modules: false,
        },
        &filter_pattern.unwrap_or_default(),
        &module.unwrap_or_else(|| "lib".to_string()),
    );

    if !test_result.succeeded() {
        eprintln!("Some tests failed.");
        return ExitCode::from(1);
    }

    ExitCode::SUCCESS
}
