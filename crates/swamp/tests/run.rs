/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use source_map_cache::SourceMap;
use source_map_cache::SourceMapWrapper;
use std::path::{Path, PathBuf};
use swamp::prelude::SeqMap;
use swamp_code_gen_program::{CodeGenOptions, code_gen_program};
use swamp_dep_loader::swamp_registry_path;
use swamp_parser::Rule::op_add;
use swamp_vm::{Vm, VmSetup};

#[test_log::test]
fn compile_and_run() {
    let mut mounts = SeqMap::new();
    let path_buf = Path::new("/Users/peter/external/swamp_autobattler/scripts").to_path_buf();
    mounts.insert("crate".to_string(), path_buf).unwrap();

    let registry_path = swamp_registry_path().unwrap();
    mounts
        .insert("registry".to_string(), registry_path)
        .unwrap();

    let mut source_map = SourceMap::new(&mounts).expect("source map failed");

    let crate_main_path = &["crate".to_string(), "main".to_string()];

    let program = swamp_compile::bootstrap_and_compile(&mut source_map, crate_main_path)
        .expect("TODO: panic message");

    let source_map_wrapper = SourceMapWrapper {
        source_map: &source_map,
        current_dir: PathBuf::from(Path::new("")),
    };

    let options = CodeGenOptions { show_disasm: false };

    let top_gen_state = code_gen_program(&program, &source_map_wrapper, &options);

    let (instructions, _constant_info, emit_function_infos, constant_memory) =
        top_gen_state.take_instructions_and_constants();

    let vm_setup = VmSetup {
        stack_memory_size: 16 * 1024,
        heap_memory_size: 1024 * 1024,
        constant_memory,
        debug_stats_enabled: false,
        debug_opcodes_enabled: false,
    };

    let mut vm = Vm::new(instructions, vm_setup);

    let main_module = program
        .modules
        .get(&["crate".to_string(), "main".to_string()])
        .unwrap();

    let simulation_fn = main_module
        .symbol_table
        .get_internal_function("simulation")
        .unwrap();

    let simulation_fn_id = simulation_fn.program_unique_id;
    let simulation_emit_info = emit_function_infos.get(&simulation_fn_id).unwrap();

    // It takes no parameters, so we can just run the function
    vm.execute_from_ip(&simulation_emit_info.ip_range.start);

    //info!(?program, "program");
}
