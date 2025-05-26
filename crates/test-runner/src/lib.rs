use source_map_cache::SourceMapWrapper;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use swamp_runtime::{RunConstantsOptions, RunOptions};
use swamp_std::print::register_print;
use swamp_vm::VmState;
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

pub struct TestRunOptions {
    pub should_run: bool,
    pub iteration_count: usize,
    pub debug_output: bool,
    pub print_output: bool,
    pub debug_opcodes: bool,
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

/// # Panics
#[allow(clippy::too_many_lines)]
pub fn run_tests(test_dir: &Path, options: &TestRunOptions, filter: &str) -> TestResult {
    let crate_main_path = &["crate".to_string(), "lib".to_string()];
    let (code_gen_result, source_map) =
        swamp_runtime::compile_and_code_gen(test_dir, crate_main_path);
    let mut vm = swamp_runtime::create_vm_with_standard_settings(
        &code_gen_result.instructions,
        &code_gen_result.prepared_constant_memory,
    );

    let mut panic_tests = Vec::new();
    let mut passed_tests = Vec::new();
    let mut trap_tests = Vec::new();

    if options.should_run {
        register_print::<TestContext>(&mut vm, &code_gen_result.program.modules, TestContext);
        let run_first_options = RunConstantsOptions {
            stderr_adapter: None,
        };

        swamp_runtime::run_first_time(
            &mut vm,
            code_gen_result.constants_in_order,
            run_first_options,
        );

        {
            let bootstrap_timer = ScopedTimer::new("run tests a bunch of times");

            for (module_name, module) in code_gen_result.program.modules.modules() {
                let mut has_shown_mod_name = false;
                for internal_fn in module.symbol_table.internal_functions() {
                    if !internal_fn.attributes.has_attribute("test") {
                        continue;
                    }
                    if options.debug_output && !has_shown_mod_name {
                        //eprintln!(">> module {module_name:?}");
                        has_shown_mod_name = true;
                    }
                    let function_to_run = code_gen_result
                        .functions
                        .get(&internal_fn.program_unique_id)
                        .unwrap();
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

                    for _ in 0..options.iteration_count {
                        swamp_runtime::run_function_with_debug(
                            &mut vm,
                            function_to_run,
                            RunOptions {
                                debug_stats_enabled: true,
                                debug_opcodes_enabled: options.debug_opcodes,
                                debug_info: &code_gen_result.debug_info,
                                source_map_wrapper: SourceMapWrapper {
                                    source_map: &source_map,
                                    current_dir: PathBuf::default(),
                                },
                            },
                        );
                        if vm.state != VmState::Normal {
                            break;
                        }
                    }

                    match &vm.state {
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
                            error!(trap_code, "TRAP");
                            eprintln!("❌ trap {complete_name} {trap_code}");
                        }
                    }
                }
            }
        }

        let pass_count = passed_tests.len();
        let fail_count = panic_tests.len() + trap_tests.len();
        let total_count = pass_count + fail_count;
        println!("---\n🚀 Test Run Complete! 🚀\n");
        println!("Results:");
        println!("  ✅ Passed: {pass_count}");
        if fail_count > 0 {
            println!("  ❌ Failed: {fail_count}");
        }
        println!("  Total Tests: {total_count}");

        if fail_count > 0 {
            println!("failing tests:");
            for test in &panic_tests {
                println!("- {test}");
            }
            for test in &trap_tests {
                println!("- {test}");
            }
        }

        eprintln!("vm stats {:?}", vm.debug);
    }
    let failed_tests = [trap_tests, panic_tests].concat();
    TestResult {
        passed_tests,
        failed_tests,
    }
}
