use std::path::Path;
use swamp_runtime::{RunConstantsOptions, RunOptions};
use swamp_std::print::register_print;
use swamp_vm::VmState;
use time_dilation::ScopedTimer;
use tracing::error;

#[derive(Debug, Default, Clone, Copy)]
struct TestContext;

pub fn colorize_parts(parts: &[String]) -> String {
    let new_parts: Vec<_> = parts
        .iter()
        .map(|x| format!("{}", tinter::bright_cyan(x)))
        .collect();

    new_parts.join("::")
}

pub fn colorful_module_name(parts: &[String]) -> String {
    let x = if parts[0] == "crate" {
        &parts[1..]
    } else {
        parts
    };

    colorize_parts(x)
}

pub struct TestRunOptions {
    pub should_run: bool,
    pub iteration_count: usize,
    pub debug_output: bool,
    pub print_output: bool,
}

pub fn run_tests(test_dir: &Path, options: TestRunOptions) {
    let crate_main_path = &["crate".to_string(), "lib".to_string()];
    let code_gen_result = swamp_runtime::compile_and_run(&test_dir, crate_main_path);
    let mut vm = swamp_runtime::create_vm_with_standard_settings(
        &code_gen_result.instructions,
        &code_gen_result.prepared_constant_memory,
    );

    let mut panic_count = 0usize;
    let mut pass_count = 0usize;
    let mut trap_count = 0usize;

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
                    if options.debug_output {
                        if !has_shown_mod_name {
                            //eprintln!(">> module {module_name:?}");
                            has_shown_mod_name = true;
                        }
                    }
                    let function_to_run = code_gen_result
                        .functions
                        .get(&internal_fn.program_unique_id)
                        .unwrap();
                    let complete_name = format!(
                        "{}:{}",
                        colorful_module_name(module_name),
                        tinter::blue(&function_to_run.internal_function_definition.assigned_name)
                    );

                    if options.debug_output {
                        eprintln!("running test '{}'", complete_name);
                    }
                    for _ in 0..options.iteration_count {
                        swamp_runtime::run_function(
                            &mut vm,
                            function_to_run,
                            RunOptions {
                                debug_stats_enabled: true,
                                debug_opcodes_enabled: false,
                            },
                        );
                        if vm.state != VmState::Normal {
                            break;
                        }
                    }

                    match &vm.state {
                        VmState::Panic(message) => {
                            panic_count += 1;
                            error!(message, "PANIC!");
                            eprintln!("❌ Panic {complete_name} {message}");
                        }
                        VmState::Normal => {
                            pass_count += 1;
                            eprintln!("✅ {complete_name} worked!");
                        }
                        VmState::Trap(trap_code) => {
                            trap_count += 1;
                            error!(trap_code, "TRAP");
                            eprintln!("❌ trap {complete_name} {trap_code}");
                        }
                    }
                }
            }
        }

        let fail_count = panic_count + trap_count;

        if fail_count == 0 {
            eprintln!("summary: ✅ {pass_count} passed")
        } else {
            eprintln!("summary: ✅ {pass_count} passed, ❌ {fail_count} failed!")
        }

        eprintln!("vm stats {:?}", vm.debug);
    }
}
