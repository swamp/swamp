/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_cache::SourceMap;
use source_map_cache::SourceMapWrapper;
use std::cell::RefCell;
use std::io::stderr;
use std::path::{Path, PathBuf};
use std::ptr;
use swamp::prelude::SeqMap;
use swamp_code_gen_program::{CodeGenOptions, code_gen_program};
use swamp_dep_loader::swamp_registry_path;
use swamp_vm::{Vm, VmSetup};
use swamp_vm_types::types::{BasicType, OffsetMemoryItem};
use swamp_vm_types::{MemoryOffset, StackMemoryAddress};

#[derive(Debug, Default, Clone, Copy)]
pub struct StderrWriter;

use std::fmt::{self, Write as FmtWrite};
use std::io::{self, Write as IoWrite};
use std::rc::Rc;
use swamp_vm::host::HostArgs;
use test_log::tracing_subscriber;
use tracing::info;

impl FmtWrite for StderrWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let mut stderr = io::stderr();

        stderr.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
}

struct Context {
    simulation_layout: BasicType,
}

fn print_fn(ctx: &mut Context, mut args: HostArgs) {
    let output = args.get_str();
    eprintln!("PRINT! {output}");
    let mut stderr_adapter = StderrWriter;
    /*
       let frame = args.get_heap_raw;
       let heap = args.heap_memory();

       swamp_vm_pretty_print::print_value(
           &mut stderr_adapter,
           frame,
           &heap,
           StackMemoryAddress(0),
           &ctx.simulation_layout,
           "Simulation",
       )
           .unwrap();

    */
}

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_writer(std::io::stderr)
        .init();

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

    let mut stderr_adapter = StderrWriter;

    let source_map_wrapper = SourceMapWrapper {
        source_map: &source_map,
        current_dir: PathBuf::from(Path::new("")),
    };

    let options = CodeGenOptions { show_disasm: true };

    let top_gen_state = code_gen_program(&program, &source_map_wrapper, &options);

    let (instructions, constants_in_order, emit_function_infos, constant_memory) =
        top_gen_state.take_instructions_and_constants();

    let should_run = false;
    if should_run {
        let vm_setup = VmSetup {
            stack_memory_size: 16 * 1024 * 20,
            heap_memory_size: 1024 * 1024,
            constant_memory,
            debug_enabled: true,
        };
        let mut vm = Vm::new(instructions, vm_setup);

        for (_key, constant) in constants_in_order {
            //info!(?constant.constant_ref, "ordered constant");
            // do not reset heap, all allocations from heap should remain (for now)
            // TODO: compact the heap after each constant
            vm.reset_frame();
            unsafe {
                // Place pointer in return
                *(vm.memory_mut().get_frame_ptr_as_u32(0)) =
                    constant.target_constant_memory.addr().0;
            }
            vm.execute_from_ip(&constant.ip_range.start);

            vm.protect_heap_up_to_current_allocator();

            let return_layout =
                swamp_code_gen::layout::layout_type(&constant.constant_ref.resolved_type);

            assert_eq!(
                constant.target_constant_memory.size(),
                return_layout.total_size
            );

            eprintln!(
                "value: written to %${:08X} --------",
                constant.target_constant_memory.addr().0
            );

            swamp_vm_pretty_print::print_value(
                &mut stderr_adapter,
                vm.frame_memory(),
                vm.memory(),
                StackMemoryAddress(0),
                &return_layout,
                &constant.constant_ref.assigned_name,
            )
            .unwrap();
        }

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

        let simulation_layout =
            swamp_code_gen::layout::layout_type(&simulation_fn.signature.signature.return_type);
        let context = Context {
            simulation_layout: simulation_layout.clone(),
        };

        {
            swamp_vm_host::register_context_aware::<Context, _>(
                &mut vm,
                1,
                &Rc::new(RefCell::new(context)),
                print_fn,
            );
        }

        // It takes no parameters, so we can just run the function
        eprintln!("============= SIMULATION STARTS ============");
        {
            vm.reset_stack_and_heap_to_constant_limit();
            vm.reset_debug();
            vm.debug_enabled = true;
            vm.execute_from_ip(&simulation_emit_info.ip_range.start);
        }

        let mut f = String::new();

        swamp_vm_pretty_print::print_value(
            &mut stderr_adapter,
            vm.frame_memory(),
            vm.memory(),
            StackMemoryAddress(0),
            &simulation_layout,
            "Simulation",
        )
        .unwrap();
    }

    //info!(?program, "program");
}
