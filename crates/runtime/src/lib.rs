mod err_wrt;
mod trace;
use source_map_cache::{FileId, SourceMap, SourceMapWrapper};
use std::fmt::Write as FmtWrite;
use std::path::{Path, PathBuf};
use swamp_analyzer::Program;
use swamp_code_gen::{ConstantInfo, GenFunctionInfo};
use swamp_code_gen_program::{CodeGenOptions, code_gen_program};
use swamp_core_extra::prelude::SeqMap;
use swamp_dep_loader::swamp_registry_path;
use swamp_semantic::{ConstantId, InternalFunctionId};
use swamp_vm::{Vm, VmSetup, VmState};
use swamp_vm_debug_info::{DebugInfo, KeepTrackOfSourceLine, SourceFileLineInfo};
use swamp_vm_disasm::{disasm_color, display_lines};
use swamp_vm_types::types::BasicTypeKind;
use swamp_vm_types::{BinaryInstruction, InstructionPosition, StackMemoryAddress};

pub struct RunConstantsOptions {
    pub stderr_adapter: Option<Box<dyn FmtWrite>>,
}

pub struct RunOptions<'a> {
    //pub stderr_adapter: Option<Box<dyn FmtWrite>>,
    pub debug_stats_enabled: bool,
    pub debug_opcodes_enabled: bool,
    pub debug_info: &'a DebugInfo,
    pub source_map_wrapper: SourceMapWrapper<'a>,
}

pub fn run_constants_in_order(
    vm: &mut Vm,
    constants_in_order: SeqMap<ConstantId, ConstantInfo>,
    mut options: RunConstantsOptions,
) {
    for (_key, constant) in constants_in_order {
        // do not reset heap, all allocations from heap should remain (for now)
        vm.reset_frame();

        if constant
            .target_constant_memory
            .ty()
            .can_be_contained_inside_register()
        {
        } else {
            // set memory location into to r0
            vm.registers[0] = constant.target_constant_memory.addr().0;
        }
        vm.execute_from_ip(&constant.ip_range.start);

        if constant
            .target_constant_memory
            .ty()
            .can_be_contained_inside_register()
        {
            match constant.target_constant_memory.ty().kind {
                BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                    let heap_ptr = vm
                        .memory_mut()
                        .get_heap_ptr(constant.target_constant_memory.addr().0 as usize)
                        .cast::<u32>();
                    unsafe {
                        *heap_ptr = vm.registers[0];
                    }
                }
                _ => todo!(),
            }
        }

        vm.protect_heap_up_to_current_allocator();

        let return_layout =
            swamp_code_gen::layout::layout_type(&constant.constant_ref.resolved_type);

        assert_eq!(
            constant.target_constant_memory.size(),
            return_layout.total_size
        );

        if let Some(x) = &mut options.stderr_adapter {
            swamp_vm_pretty_print::print_value(
                x.as_mut(),
                vm.frame_memory(),
                vm.memory(),
                StackMemoryAddress(0),
                &return_layout,
                &constant.constant_ref.assigned_name,
            )
            .unwrap();
        }
    }
}

// "/Users/peter/external/swamp_autobattler/scripts"
#[must_use]
pub fn crate_and_registry(path_to_swamp: &Path) -> SourceMap {
    let mut mounts = SeqMap::new();
    //  let path_buf = Path::new(path_to_swamp).to_path_buf();
    mounts
        .insert("crate".to_string(), path_to_swamp.to_path_buf())
        .unwrap();

    let registry_path = swamp_registry_path().unwrap();
    mounts
        .insert("registry".to_string(), registry_path)
        .unwrap();

    SourceMap::new(&mounts).expect("source map failed")
}

pub struct CodeGenResult {
    pub instructions: Vec<BinaryInstruction>,
    pub constants_in_order: SeqMap<ConstantId, ConstantInfo>,
    pub functions: SeqMap<InternalFunctionId, GenFunctionInfo>,
    pub prepared_constant_memory: Vec<u8>,
    pub program: Program,
    pub debug_info: DebugInfo,
}

pub fn compile_and_codegen_main_path(
    source_map: &mut source_map_cache::SourceMap,
    root_module_path: &[String],
    current_dir: &Path,
    options: CodeGenOptions,
) -> CodeGenResult {
    let program = swamp_compile::bootstrap_and_compile(source_map, root_module_path)
        .expect("TODO: panic message");

    let source_map_wrapper = SourceMapWrapper {
        source_map,
        current_dir: current_dir.to_path_buf(),
    };

    let top_gen_state = code_gen_program(&program, &source_map_wrapper, &options);

    let (instructions, constants_in_order, emit_function_infos, constant_memory, debug_info) =
        top_gen_state.take_instructions_and_constants();

    CodeGenResult {
        debug_info,
        instructions,
        constants_in_order,
        functions: emit_function_infos,
        prepared_constant_memory: constant_memory,
        program,
    }
}

#[must_use]
pub fn create_vm_with_standard_settings(
    instructions: &[BinaryInstruction],
    prepared_constant_memory: &[u8],
) -> Vm {
    let vm_setup = VmSetup {
        stack_memory_size: 16 * 1024 * 20,
        heap_memory_size: 1024 * 1024,
        constant_memory: prepared_constant_memory.to_vec(),
        debug_opcodes_enabled: false,
        debug_stats_enabled: false,
    };

    Vm::new(instructions.to_vec(), vm_setup)
}

pub fn run_first_time(
    vm: &mut Vm,
    constants_in_order: SeqMap<ConstantId, ConstantInfo>,
    options: RunConstantsOptions,
) {
    run_constants_in_order(vm, constants_in_order, options);
}

pub fn run_function_with_debug(
    vm: &mut Vm,
    function_to_run: &GenFunctionInfo,
    run_options: RunOptions,
) {
    vm.reset_stack_and_heap_to_constant_limit();
    //vm.reset_debug();
    vm.state = VmState::Normal;
    vm.debug_opcodes_enabled = run_options.debug_opcodes_enabled;
    vm.debug_stats_enabled = run_options.debug_stats_enabled;
    vm.set_pc(&function_to_run.ip_range.start);

    let mut last_line_info = KeepTrackOfSourceLine::new();

    while !vm.is_execution_complete() {
        let pc = vm.pc();
        #[cfg(feature = "debug_vm")]
        if run_options.debug_opcodes_enabled {
            let regs = [0, 1, 2, 3, 4, 128, 129, 130];

            eprint!(
                "{}",
                tinter::bright_black(&format!("fp:{:08X}, sp:{:08X}, ", vm.fp(), vm.sp()))
            );

            eprint!(
                "{}",
                tinter::bright_black(&format!("t:{} ", if vm.flags.t { "1" } else { "0" }))
            );

            for reg in regs {
                let reg_name = &format!("r{reg}");
                eprint!(
                    "{}",
                    tinter::bright_black(&format!("{reg_name:>3}:{:08X}, ", vm.registers[reg]))
                );
            }
            eprintln!();
        }

        vm.step();

        #[cfg(feature = "debug_vm")]
        if run_options.debug_opcodes_enabled {
            let info = run_options.debug_info.fetch(pc).unwrap();

            if info.meta.node.span.file_id != 0 {
                let (line, column) = run_options
                    .source_map_wrapper
                    .source_map
                    .get_span_location_utf8(
                        info.meta.node.span.file_id,
                        info.meta.node.span.offset as usize,
                    );
                let source_line_info = SourceFileLineInfo {
                    row: line,
                    file_id: info.meta.node.span.file_id as usize,
                };

                if let Some((start_row, end_row)) =
                    last_line_info.check_if_new_line(&source_line_info)
                {
                    let mut string = String::new();
                    display_lines(
                        &mut string,
                        source_line_info.file_id as FileId,
                        start_row,
                        end_row,
                        &run_options.source_map_wrapper,
                    );
                    eprint!("{string}");
                }
            }

            let instruction = &vm.instructions()[pc];
            let string = disasm_color(
                instruction,
                &info.function_debug_info.frame_memory,
                &info.meta,
                &InstructionPosition(pc as u32),
            );
            eprintln!("{pc:04X}> {string}");
        }
    }
}

#[must_use]
pub fn compile_and_code_gen(
    path_to_root_of_swamp_files: &Path,
    main_module_path: &[String],
) -> (CodeGenResult, SourceMap) {
    let mut source_map = crate_and_registry(path_to_root_of_swamp_files);
    let current_dir = PathBuf::from(Path::new(""));
    let options = CodeGenOptions { show_disasm: true };

    let result =
        compile_and_codegen_main_path(&mut source_map, main_module_path, &current_dir, options);
    (result, source_map)
}
