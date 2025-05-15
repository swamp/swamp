use std::path::Path;
use swamp_runtime::{RunConstantsOptions, RunOptions};
use swamp_std::print::register_print;
use time_dilation::ScopedTimer;

#[derive(Debug, Default, Clone, Copy)]
struct TestContext;

pub fn very_basic(test_dir: &Path) {
    const SHOULD_RUN: bool = true;
    const RUN_ITERATIONS: usize = 1; //32_000;
    const DEBUG_OUTPUT: bool = true;

    let crate_main_path = &["crate".to_string(), "lib".to_string()];
    let code_gen_result = swamp_runtime::compile_and_run(&test_dir, crate_main_path);
    let mut vm = swamp_runtime::create_vm_with_standard_settings(
        &code_gen_result.instructions,
        &code_gen_result.prepared_constant_memory,
    );

    if SHOULD_RUN {
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
                    if DEBUG_OUTPUT {
                        if !has_shown_mod_name {
                            eprintln!(">> module {module_name:?}");
                            has_shown_mod_name = true;
                        }
                    }
                    let function_to_run = code_gen_result
                        .functions
                        .get(&internal_fn.program_unique_id)
                        .unwrap();

                    if DEBUG_OUTPUT {
                        eprintln!(
                            "running test '{}'",
                            function_to_run.internal_function_definition.assigned_name
                        );
                    }
                    for _ in 0..RUN_ITERATIONS {
                        swamp_runtime::run_function(
                            &mut vm,
                            function_to_run,
                            RunOptions {
                                debug_stats_enabled: true,
                                debug_opcodes_enabled: false,
                            },
                        );
                    }
                }
            }
        }

        eprintln!("vm stats {:?}", vm.debug);
    }
}

fn main() {
    very_basic(Path::new("../tests/fixtures/basic"));
}
