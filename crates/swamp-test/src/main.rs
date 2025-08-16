/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use pico_args::Arguments;
use std::io::Write;
use std::path::PathBuf;
use std::process::ExitCode;
use std::{env, io};
use swamp_test_runner::prelude::create_default_source_map_crate_only;
use swamp_test_runner::{StepBehavior, TestRunOptions, init_logger, run_tests};
use swamp_yini::{ProjectType, read_yini_cwd_with_defaults};

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
  -s, --show-semantic     (show semantic output before the tests run)
  -m, --show-modules      (show semantic symbol tables before the tests run)
  -a, --show-assembly     (show assembly output before the tests run)
  -i, --show-instructions (show Swamp VM instructions when running)
  -o, --show-operations   (show Swamp VM operations for complex opcodes)
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

    let assembly_filter_pattern: Option<String> = match args.opt_value_from_str("--assembly_filter")
    {
        Ok(opt) => opt,
        Err(e) => {
            eprintln!("error: invalid value for `--assembly_filter`: {e}\n");
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

    let ini = read_yini_cwd_with_defaults();

    let crate_module_root = if let Some(found_module) = module {
        found_module
    } else {
        match ini.ty {
            ProjectType::Library => "lib".to_string(),
            ProjectType::Executable => "main".to_string(),
        }
    };

    let show_debug = args.contains(["-d", "--debug"]);
    let show_assembly = args.contains(["-a", "--show-assembly"]);
    let show_semantic = args.contains(["-s", "--show-semantic"]);
    let verify_memory = args.contains(["-x", "--verify-memory"]);
    let show_types = args.contains(["-t", "--show-types"]);
    let show_modules = args.contains(["-m", "--show-modules"]);
    let show_instructions = args.contains(["-i", "--show-instructions"]);
    let show_operations = args.contains(["-o", "--show-operations"]);

    let step_behavior: StepBehavior = match args
        .opt_value_from_str::<_, StepBehavior>("--step")
        .expect("should work")
    {
        Some(sb) => sb,
        None => StepBehavior::ResumeExecution,
    };

    // Check for any unexpected positional arguments
    let leftover = args.finish();
    if !leftover.is_empty() {
        eprintln!("error: unexpected positional arguments: {leftover:?}\n");
        print_usage(io::stderr());
        return ExitCode::from(1);
    }

    init_logger();

    let mut source_map = create_default_source_map_crate_only(&test_path).unwrap();
    let test_result = run_tests(
        &mut source_map,
        &TestRunOptions {
            should_run: true,
            print_output: false,
            iteration_count: repeat_count,
            debug_output: show_debug,
            debug_opcodes: show_debug | show_instructions,
            debug_operations: show_debug | show_operations,
            debug_stats: true,
            show_semantic,
            show_assembly: show_debug | show_assembly,
            assembly_filter: assembly_filter_pattern,
            show_modules,
            show_types,
            step_behaviour: step_behavior,
            debug_memory_enabled: verify_memory,
        },
        &filter_pattern.unwrap_or_default(),
        &crate_module_root,
    );

    if !test_result.succeeded() {
        eprintln!("Some tests failed.");
        return ExitCode::from(1);
    }

    ExitCode::SUCCESS
}
