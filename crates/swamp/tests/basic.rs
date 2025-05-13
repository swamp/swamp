use std::path::PathBuf;
use swamp_runtime::RunConstantsOptions;

#[must_use]
pub fn get_fixture_dir(sub_dirs: &[&str]) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    path.push("tests");
    path.push("fixtures");
    for sub_dir in sub_dirs {
        path.push(sub_dir);
    }

    path
}

#[test_log::test]
pub fn very_basic() {
    let crate_main_path = &["crate".to_string(), "arithmetic".to_string()];
    let test_dir = get_fixture_dir(&["basic"]);
    let code_gen_result = swamp_runtime::compile_and_run(&test_dir, crate_main_path);
    let mut vm = swamp_runtime::create_vm_with_standard_settings(
        &code_gen_result.instructions,
        &code_gen_result.prepared_constant_memory,
    );
    let run_first_options = RunConstantsOptions {
        stderr_adapter: None,
    };
    swamp_runtime::run_first_time(
        &mut vm,
        code_gen_result.constants_in_order,
        run_first_options,
    );

    let main_module = code_gen_result
        .program
        .modules
        .get(crate_main_path)
        .unwrap();

    let add_fn = main_module
        .symbol_table
        .get_internal_function("add")
        .unwrap();

    let function_to_run = code_gen_result
        .functions
        .get(&add_fn.program_unique_id)
        .unwrap();

    swamp_runtime::run_function(&mut vm, function_to_run);
}
