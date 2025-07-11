/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use pico_args::Arguments;
use std::io::Write;
use std::path::PathBuf;
use std::process::ExitCode;
use std::{env, io};
use swamp_runtime::prelude::{CodeGenOptions, RunMode};
use swamp_runtime::{CompileAndCodeGenOptions, CompileOptions, compile_and_code_gen};
use tracing_subscriber::filter::LevelFilter;
use tracing_subscriber::{EnvFilter, fmt};

pub fn init_logger() {
    let filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::OFF.into())
        .from_env_lossy();

    fmt()
        .with_env_filter(filter)
        .with_ansi(true)
        .with_writer(std::io::stderr)
        .init();
}
fn print_usage<W: Write>(mut out: W) {
    let _ = write!(
        out,
        "\
Usage: swamp-build --path <path> [OPTIONS]\n\n\
Options:\n\
  --path <path>           (required) directory or file to build from\n\
  --module <pattern>      (optional) build from this module (default: `lib`)\n\
  -s, --show-semantic     (show semantic information)\n\
  -m, --show-modules      (show module structure)\n\
  -a, --show-assembly     (show Swamp VM disassembly)\n\
  --skip-codegen          (skip Swamp VM code generation)\n\
  -h, --help              (print this help and exit)\n"
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

    let module: Option<String> = match args.opt_value_from_str("--module") {
        Ok(None) => Some("lib".to_string()),
        Ok(opt) => opt,
        Err(e) => {
            eprintln!("error: invalid value for `--module`: {e}\n");
            print_usage(io::stderr());

            return ExitCode::from(1);
        }
    };

    let show_semantic = args.contains(["-s", "--show-semantic"]);
    let show_types = args.contains(["-t", "--show-types"]);
    let show_modules = args.contains(["-m", "--show-modules"]);
    let show_assembly = args.contains(["-a", "--show-assembly"]);
    let debug_codegen = args.contains("--debug-codegen");
    let skip_codegen = args.contains("--skip-codegen");

    // Check for any unexpected positional arguments
    let leftover = args.finish();
    if !leftover.is_empty() {
        eprintln!("error: unexpected positional arguments: {leftover:?}\n");
        print_usage(io::stderr());

        return ExitCode::from(1);
    }

    init_logger();

    let compile_and_code_gen_options = CompileAndCodeGenOptions {
        skip_codegen,
        compile_options: CompileOptions {
            show_semantic,
            show_modules,
            show_errors: true,
            show_warnings: true,
            show_hints: true,
            show_information: true,
            show_types,
        },
        code_gen_options: CodeGenOptions {
            show_disasm: show_assembly,
            disasm_filter: None,
            show_debug: debug_codegen,
            show_types,
            ignore_host_call: false,
        },
        run_mode: RunMode::Development,
    };

    let test_result = compile_and_code_gen(
        &test_path,
        &["crate".to_string(), module.unwrap_or_default()],
        &compile_and_code_gen_options,
    );

    if test_result.is_none() {
        eprintln!("build failed.");
        return ExitCode::from(1);
    }

    ExitCode::SUCCESS
}
