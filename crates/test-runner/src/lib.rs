/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use source_map_cache::SourceMapWrapper;
use std::fmt::{Display, Formatter};
use std::io;
use std::io::Write;
use std::num::ParseIntError;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::thread::sleep;
use std::time::Duration;
use swamp_runtime::prelude::CodeGenOptions;
use swamp_runtime::{
    CompileAndCodeGenOptions, CompileAndVmResult, CompileOptions, RunConstantsOptions, RunOptions,
    compile_codegen_and_create_vm,
};
use swamp_std::print::print_fn;
use swamp_vm::VmState;
use swamp_vm::host::HostFunctionCallback;
use time_dilation::ScopedTimer;
use tracing::error;

#[derive(Debug, Default, Clone, Copy)]
struct TestContext;

#[must_use]
pub fn colorize_parts(parts: &[String]) -> String {
    let new_parts: Vec<_> = parts
        .iter()
        .map(|x| format!("{}", tinter::bright_cyan(x)))
        .collect();

    new_parts.join("::")
}

#[must_use]
pub fn colorful_module_name(parts: &[String]) -> String {
    let x = if parts[0] == "crate" {
        &parts[1..]
    } else {
        parts
    };

    colorize_parts(x)
}

#[must_use]
pub fn pretty_module_parts(parts: &[String]) -> String {
    let new_parts: Vec<_> = parts.iter().map(std::string::ToString::to_string).collect();

    new_parts.join("::")
}

#[must_use]
pub fn pretty_module_name(parts: &[String]) -> String {
    let x = if parts[0] == "crate" {
        &parts[1..]
    } else {
        parts
    };

    pretty_module_parts(x)
}

#[must_use]
pub fn matches_pattern(test_name: &str, pattern: &str) -> bool {
    if pattern.ends_with("::") {
        test_name.starts_with(pattern)
    } else if pattern.contains('*') {
        let parts: Vec<&str> = pattern.split('*').collect();
        if parts.len() > 2 {
            return false;
        }

        let prefix = parts[0];
        let suffix = if parts.len() == 2 { parts[1] } else { "" }; // Handle "*suffix" or "prefix*"

        if !test_name.starts_with(prefix) {
            return false;
        }

        let remaining_name = &test_name[prefix.len()..];
        remaining_name.ends_with(suffix)
    } else {
        test_name == pattern
    }
}
#[must_use]
pub fn test_name_matches_filter(test_name: &str, filter_string: &str) -> bool {
    if filter_string.trim().is_empty() {
        return true;
    }

    let patterns: Vec<&str> = filter_string.split(',').collect();

    for pattern in patterns {
        let trimmed_pattern = pattern.trim();

        if matches_pattern(test_name, trimmed_pattern) {
            return true;
        }
    }

    false
}

pub enum StepBehavior {
    ResumeExecution,
    WaitForUserInput,
    Pause(Duration),
}

pub enum StepParseError {
    UnknownVariant(String),
    MissingDuration,
    ParseInt(ParseIntError),
}

impl Display for StepParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownVariant(_) => write!(f, "unknown"),
            Self::MissingDuration => write!(f, "missing duration"),
            Self::ParseInt(_) => write!(f, "parse int failed"),
        }
    }
}

impl FromStr for StepBehavior {
    type Err = StepParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_lowercase();
        let mut parts = lower.splitn(2, ':');
        let variant = parts.next().unwrap();

        match variant {
            "resume" => Ok(Self::ResumeExecution),
            "wait" => Ok(Self::WaitForUserInput),
            "pause" => match parts.next() {
                Some(ms_str) => {
                    let ms: u64 = ms_str.parse().map_err(StepParseError::ParseInt)?;
                    Ok(Self::Pause(Duration::from_millis(ms)))
                }
                None => Err(StepParseError::MissingDuration),
            },

            other => Err(StepParseError::UnknownVariant(other.to_string())),
        }
    }
}

pub struct TestRunOptions {
    pub should_run: bool,
    pub iteration_count: usize,
    pub debug_output: bool,
    pub print_output: bool,
    pub debug_opcodes: bool,
    pub debug_operations: bool,
    pub debug_stats: bool,
    pub show_semantic: bool,
    pub show_assembly: bool,
    pub show_modules: bool,
    pub step_behaviour: StepBehavior,
}

pub fn init_logger() {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_writer(std::io::stderr)
        .init();
}

#[derive(Clone)]
pub struct TestInfo {
    pub name: String,
}
impl Display for TestInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", self.name)
    }
}

pub struct TestResult {
    pub passed_tests: Vec<TestInfo>,
    pub failed_tests: Vec<TestInfo>,
}

impl TestResult {
    #[must_use]
    pub const fn succeeded(&self) -> bool {
        self.failed_tests.is_empty()
    }
}

pub struct TestExternals {}

impl HostFunctionCallback for TestExternals {
    fn dispatch_host_call(&mut self, args: swamp_vm::host::HostArgs) {
        if args.function_id == 1 {
            print_fn(args);
        }
    }
}

/// # Panics
#[allow(clippy::too_many_lines)]
pub fn run_tests(
    test_dir: &Path,
    options: &TestRunOptions,
    filter: &str,
    module_suffix: &str,
) -> TestResult {
    let crate_main_path = &["crate".to_string(), module_suffix.to_string()];
    let compile_and_code_gen_options = CompileAndCodeGenOptions {
        compile_options: CompileOptions {
            show_semantic: options.show_semantic,
            show_modules: options.show_modules,
        },
        code_gen_options: CodeGenOptions {
            show_disasm: options.show_assembly,
            show_debug: options.debug_output,
        },
        skip_codegen: false,
    };
    let internal_result =
        compile_codegen_and_create_vm(test_dir, crate_main_path, compile_and_code_gen_options)
            .unwrap();

    let CompileAndVmResult::CompileAndVm(mut result) = internal_result else {
        panic!("didn't work to compile")
    };

    let mut passed_tests = Vec::new();

    let mut panic_tests = Vec::new();
    let mut trap_tests = Vec::new();
    let mut failed_tests = Vec::new();
    let mut expected_panic_passed: Vec<TestInfo> = Vec::new();
    let mut expected_trap_passed: Vec<TestInfo> = Vec::new();

    if options.should_run {
        let some_form_of_debug = options.debug_opcodes || options.debug_output;
        eprintln!("running in debug: {some_form_of_debug}");

        let run_first_options = RunConstantsOptions {
            stderr_adapter: None,
        };

        swamp_runtime::run_first_time(
            &mut result.codegen.vm,
            &result.codegen.code_gen_result.constants_in_order,
            &mut TestExternals {},
            run_first_options,
        );

        {
            let _bootstrap_timer = ScopedTimer::new("run tests a bunch of times");

            for (module_name, module) in result.compile.program.modules.modules() {
                let mut has_shown_mod_name = false;
                for internal_fn in module.symbol_table.internal_functions() {
                    if !internal_fn.attributes.has_attribute("test") {
                        continue;
                    }
                    if options.debug_output && !has_shown_mod_name {
                        //eprintln!(">> module {module_name:?}");
                        has_shown_mod_name = true;
                    }
                    let function_to_run = result
                        .codegen
                        .code_gen_result
                        .functions
                        .get(&internal_fn.program_unique_id)
                        .unwrap();

                    let all_attributes = &function_to_run.internal_function_definition.attributes;

                    let mut expected_vm_state = VmState::Normal;

                    if !all_attributes.is_empty() {
                        let code =
                            all_attributes.get_string_from_fn_arg("should_trap", "expected", 0);
                        if let Some(code) = code {
                            expected_vm_state = VmState::Trap(code.parse().unwrap());
                        } else {
                            let panic_message = all_attributes.get_string_from_fn_arg(
                                "should_panic",
                                "expected",
                                0,
                            );
                            if let Some(panic_message) = panic_message {
                                expected_vm_state = VmState::Panic(panic_message.clone());
                            }
                        }
                    }

                    let complete_name = format!(
                        "{}::{}",
                        colorful_module_name(module_name),
                        tinter::blue(&function_to_run.internal_function_definition.assigned_name)
                    );
                    let formal_name = format!(
                        "{}::{}",
                        pretty_module_name(module_name),
                        &function_to_run.internal_function_definition.assigned_name
                    );

                    if !test_name_matches_filter(&formal_name, filter) {
                        continue;
                    }

                    let test_info = TestInfo { name: formal_name };

                    eprintln!("🚀starting test '{complete_name}'");

                    if some_form_of_debug {
                        for _ in 0..options.iteration_count {
                            swamp_runtime::run_function_with_debug(
                                &mut result.codegen.vm,
                                function_to_run,
                                &mut TestExternals {},
                                RunOptions {
                                    debug_stats_enabled: options.debug_stats,
                                    debug_opcodes_enabled: options.debug_opcodes,
                                    debug_operations_enabled: options.debug_operations,
                                    max_count: 0,
                                    use_color: true,
                                    debug_info: &result.codegen.code_gen_result.debug_info,
                                    source_map_wrapper: SourceMapWrapper {
                                        source_map: &result.codegen.source_map,
                                        current_dir: PathBuf::default(),
                                    },
                                },
                            );

                            while result.codegen.vm.state == VmState::Step {
                                handle_step(&options.step_behaviour);
                                result.codegen.vm.state = VmState::Normal;
                                result.codegen.vm.resume(&mut TestExternals {});
                            }

                            if result.codegen.vm.state != expected_vm_state {
                                break;
                            }
                        }
                    } else {
                        for _ in 0..options.iteration_count {
                            swamp_runtime::run_as_fast_as_possible(
                                &mut result.codegen.vm,
                                function_to_run,
                                &mut TestExternals {},
                                RunOptions {
                                    debug_stats_enabled: options.debug_stats,
                                    debug_opcodes_enabled: options.debug_opcodes,
                                    debug_operations_enabled: options.debug_operations,
                                    max_count: 0,
                                    use_color: true,
                                    debug_info: &result.codegen.code_gen_result.debug_info,
                                    source_map_wrapper: SourceMapWrapper {
                                        source_map: &result.codegen.source_map,
                                        current_dir: PathBuf::default(),
                                    },
                                },
                            );
                            while result.codegen.vm.state == VmState::Step {
                                handle_step(&options.step_behaviour);
                                result.codegen.vm.state = VmState::Normal;
                                result.codegen.vm.resume(&mut TestExternals {});
                            }
                            if result.codegen.vm.state != expected_vm_state {
                                break;
                            }
                        }
                    }

                    if expected_vm_state == VmState::Normal {
                        match &result.codegen.vm.state {
                            VmState::Panic(message) => {
                                panic_tests.push(test_info);
                                error!(message, "PANIC!");
                                eprintln!("❌ Panic {complete_name} {message}");
                            }
                            VmState::Normal => {
                                passed_tests.push(test_info);
                                eprintln!("✅ {complete_name} worked!");
                            }
                            VmState::Trap(trap_code) => {
                                trap_tests.push(test_info);
                                error!(%trap_code, "TRAP");
                                eprintln!("❌ trap {complete_name} {trap_code}");
                            }

                            VmState::Step | VmState::Halt => {
                                panic_tests.push(test_info);
                                error!("Step or Halt");
                                eprintln!("❌ trap {complete_name}");
                            }
                        }
                    } else if let VmState::Trap(expected_trap_code) = expected_vm_state {
                        match &result.codegen.vm.state {
                            VmState::Trap(actual_trap_code) => {
                                if actual_trap_code == &expected_trap_code {
                                    expected_trap_passed.push(test_info.clone());
                                    eprintln!(
                                        "✅ Expected Trap {complete_name} (code: {actual_trap_code})"
                                    );
                                } else {
                                    failed_tests.push(test_info.clone());
                                    error!(%expected_trap_code, %actual_trap_code, "WRONG TRAP CODE");
                                    eprintln!(
                                        "❌ Wrong Trap Code {complete_name} (Expected: {expected_trap_code}, Got: {actual_trap_code})"
                                    );
                                }
                            }
                            VmState::Normal => {
                                failed_tests.push(test_info.clone());
                                error!("Expected TRAP, got NORMAL");
                                eprintln!("❌ Expected Trap {complete_name}, but it ran normally.");
                            }
                            VmState::Panic(message) => {
                                failed_tests.push(test_info.clone());
                                error!(message, "Expected TRAP, got PANIC");
                                eprintln!(
                                    "❌ Expected Trap {complete_name}, but it panicked: {message}"
                                );
                            }
                            VmState::Step | VmState::Halt => {
                                panic_tests.push(test_info);
                                error!("Step or Halt");
                                eprintln!("❌ trap {complete_name}");
                            }
                        }
                    } else if let VmState::Panic(expected_panic_message) = expected_vm_state {
                        match &result.codegen.vm.state {
                            VmState::Panic(actual_panic_message) => {
                                if actual_panic_message.contains(&expected_panic_message) {
                                    expected_panic_passed.push(test_info.clone());
                                    eprintln!(
                                        "✅ Expected Panic {complete_name} (message contains: \"{expected_panic_message}\")",
                                    );
                                } else {
                                    failed_tests.push(test_info.clone());
                                    error!(
                                        expected_panic_message,
                                        actual_panic_message, "WRONG PANIC MESSAGE"
                                    );
                                    eprintln!(
                                        "❌ Wrong Panic Message {complete_name} (Expected contains: \"{expected_panic_message}\", Got: \"{actual_panic_message}\")"
                                    );
                                }
                            }
                            VmState::Normal => {
                                failed_tests.push(test_info.clone());
                                error!("Expected PANIC, got NORMAL");
                                eprintln!(
                                    "❌ Expected Panic {complete_name}, but it ran normally."
                                );
                            }
                            VmState::Trap(trap_code) => {
                                failed_tests.push(test_info.clone());
                                error!(%trap_code, "Expected PANIC, got TRAP");
                                eprintln!(
                                    "❌ Expected Panic {complete_name}, but it trapped: {trap_code}"
                                );
                            }
                            VmState::Step | VmState::Halt => {
                                panic_tests.push(test_info);
                                error!("Step or Halt");
                                eprintln!("❌ trap {complete_name}");
                            }
                        }
                    }
                }
            }
        }

        // Calculate counts for each category
        let passed_normal_count = passed_tests.len();
        let unexpected_panic_count = panic_tests.len();
        let unexpected_trap_count = trap_tests.len();
        let failed_mismatch_count = failed_tests.len(); // These are tests that failed for reasons other than an unexpected panic/trap

        let expected_panic_pass_count = expected_panic_passed.len();
        let expected_trap_pass_count = expected_trap_passed.len();

        // Total counts
        let total_passed_count =
            passed_normal_count + expected_panic_pass_count + expected_trap_pass_count;
        let total_failed_count =
            unexpected_panic_count + unexpected_trap_count + failed_mismatch_count;
        let total_tests_run = total_passed_count + total_failed_count;

        // ---
        // ## Test Run Summary
        // ---
        println!("\n---\n🚀 Test Run Complete! 🚀\n");

        println!("Results:");
        println!("  ✅ Passed Normally: {passed_normal_count}");
        println!("  ✅ Passed (Expected Panic): {expected_panic_pass_count}");
        println!("  ✅ Passed (Expected Trap): {expected_trap_pass_count}");

        if total_failed_count > 0 {
            println!("  ❌ **TOTAL FAILED:** {total_failed_count}",);
        }

        println!("  Total Tests Run: {total_tests_run}",);

        // ---
        // ## Failing Test Details
        // ---
        if total_failed_count > 0 {
            println!("\n--- Failing Tests Details ---");

            if unexpected_panic_count > 0 {
                println!("\n### Unexpected Panics:");
                for test in &panic_tests {
                    println!("- ❌ {}", test.name);
                }
            }

            if unexpected_trap_count > 0 {
                println!("\n### Unexpected Traps:");
                for test in &trap_tests {
                    println!("- ❌ {}", test.name);
                }
            }

            if failed_mismatch_count > 0 {
                println!("\n### Other Failures:");
                for test in &failed_tests {
                    println!("- ❌ {}", test.name);
                }
            }
        }

        eprintln!("\n\nvm stats {:?}", result.codegen.vm.debug);
    }

    let failed_tests = [trap_tests, panic_tests].concat();
    TestResult {
        passed_tests,
        failed_tests,
    }
}

fn handle_step(step_behavior: &StepBehavior) {
    match step_behavior {
        StepBehavior::ResumeExecution => {}
        StepBehavior::WaitForUserInput => {
            wait_for_user_pressing_enter();
        }
        StepBehavior::Pause(duration) => {
            eprintln!("step. waiting {duration:?}");
            sleep(*duration);
        }
    }
}

fn wait_for_user_pressing_enter() {
    let mut buf = String::new();
    print!("Step detected. press ENTER to continue");
    io::stdout().flush().unwrap();

    // Blocks until the user hits Enter
    io::stdin().read_line(&mut buf).expect("should work");
}
