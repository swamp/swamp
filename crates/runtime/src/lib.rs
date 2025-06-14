/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

mod err_wrt;
pub mod prelude;
mod trace;

use source_map_cache::KeepTrackOfSourceLine;
use source_map_cache::SourceFileLineInfo;
use source_map_cache::{FileId, SourceMap, SourceMapWrapper};
use std::fmt::Write as FmtWrite;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Path, PathBuf};
use swamp_analyzer::Program;
use swamp_code_gen::{ConstantInfo, GenFunctionInfo};
use swamp_code_gen_program::{CodeGenOptions, code_gen_program};
pub use swamp_compile::CompileOptions;
use swamp_core_extra::prelude::SeqMap;
use swamp_dep_loader::swamp_registry_path;
use swamp_semantic::{ConstantId, InternalFunctionDefinitionRef, InternalFunctionId};
use swamp_types::Type;
use swamp_vm::host::HostFunctionCallback;
use swamp_vm::{TrapCode, Vm, VmSetup, VmState};
use swamp_vm_debug_info::DebugInfo;
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
    pub debug_operations_enabled: bool,
    pub max_count: usize, // dangerous
    pub use_color: bool,
}

pub fn run_constants_in_order(
    vm: &mut Vm,
    constants_in_order: &SeqMap<ConstantId, ConstantInfo>,
    host_function_callback: &mut dyn HostFunctionCallback,
    mut options: RunConstantsOptions,
) {
    for (_key, constant) in constants_in_order {
        // do not reset heap, all allocations from heap should remain (for now)
        vm.reset_call_stack();

        if constant.target_constant_memory.ty().is_scalar() {
        } else {
            // set memory location into to r0
            vm.registers[0] = constant.target_constant_memory.addr().0;
        }
        vm.execute_from_ip(&constant.ip_range.start, host_function_callback);

        if constant.target_constant_memory.ty().is_scalar() {
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
                BasicTypeKind::B8 | BasicTypeKind::U8 => {
                    let heap_ptr = vm
                        .memory_mut()
                        .get_heap_ptr(constant.target_constant_memory.addr().0 as usize)
                        .cast::<u8>();
                    unsafe {
                        *heap_ptr = vm.registers[0] as u8;
                    }
                }
                _ => todo!(),
            }
        }

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

    if let Some(found_swamp_home) = swamp_registry_path() {
        mounts
            .insert("registry".to_string(), found_swamp_home)
            .unwrap();
    }

    SourceMap::new(&mounts).expect("source map failed")
}

pub struct CompileAndMaybeCodeGenResult {
    pub compile: CompileResult,
    pub codegen: Option<CodeGenResult>,
}

pub struct CompileResult {
    pub program: Program,
}

pub struct CodeGenResult {
    pub instructions: Vec<BinaryInstruction>,
    pub constants_in_order: SeqMap<ConstantId, ConstantInfo>,
    pub functions: SeqMap<InternalFunctionId, GenFunctionInfo>,
    pub prepared_constant_memory: Vec<u8>,
    pub debug_info: DebugInfo,
}

impl CodeGenResult {
    #[must_use]
    pub fn find_function(&self, formal_name: &str) -> Option<&GenFunctionInfo> {
        self.functions
            .values()
            .find(|&func| func.internal_function_definition.assigned_name == formal_name)
    }
}

pub fn compile_main_path(
    source_map: &mut SourceMap,
    root_module_path: &[String],
    options: &CompileOptions,
) -> Option<CompileResult> {
    let program =
        swamp_compile::bootstrap_and_compile(source_map, root_module_path, options).ok()?;

    Some(CompileResult { program })
}

#[must_use]
pub fn code_gen(
    program: &Program,
    source_map_wrapper: &SourceMapWrapper,
    code_gen_options: &CodeGenOptions,
) -> CodeGenResult {
    let top_gen_state = code_gen_program(program, source_map_wrapper, code_gen_options);

    let (instructions, constants_in_order, emit_function_infos, constant_memory, debug_info) =
        top_gen_state.take_instructions_and_constants();

    CodeGenResult {
        debug_info,
        instructions,
        constants_in_order,
        functions: emit_function_infos,
        prepared_constant_memory: constant_memory,
    }
}

#[must_use]
pub fn create_vm_with_standard_settings(
    instructions: &[BinaryInstruction],
    prepared_constant_memory: &[u8],
) -> Vm {
    let vm_setup = VmSetup {
        stack_memory_size: 16 * 1024 * 1024,
        constant_memory: prepared_constant_memory.to_vec(),
        debug_opcodes_enabled: false,
        debug_stats_enabled: false,
        debug_operations_enabled: false,
    };

    Vm::new(instructions.to_vec(), vm_setup)
}

pub fn run_first_time(
    vm: &mut Vm,
    constants_in_order: &SeqMap<ConstantId, ConstantInfo>,
    host_function_callback: &mut dyn HostFunctionCallback,
    options: RunConstantsOptions,
) {
    run_constants_in_order(vm, constants_in_order, host_function_callback, options);
}

pub fn run_as_fast_as_possible(
    vm: &mut Vm,
    function_to_run: &GenFunctionInfo,
    host_function_callback: &mut dyn HostFunctionCallback,
    run_options: RunOptions,
) {
    vm.reset_stack_and_heap_to_constant_limit();
    vm.state = VmState::Normal;
    vm.debug_opcodes_enabled = false;
    vm.debug_operations_enabled = run_options.debug_operations_enabled;
    vm.debug_stats_enabled = run_options.debug_stats_enabled;

    vm.execute_from_ip(&function_to_run.ip_range.start, host_function_callback);
}

fn calculate_memory_checksum(memory: &[u8]) -> u64 {
    let mut hasher = DefaultHasher::new();
    memory.hash(&mut hasher);
    hasher.finish()
}

pub fn run_function(
    vm: &mut Vm,
    function_to_run: &GenFunctionInfo,
    host_function_callback: &mut dyn HostFunctionCallback,
    run_options: RunOptions,
) {
    vm.reset_stack_and_heap_to_constant_limit();
    //vm.reset_debug();
    vm.state = VmState::Normal;
    // vm.debug_opcodes_enabled = run_options.debug_opcodes_enabled;
    //vm.debug_operations_enabled = run_options.debug_operations_enabled;
    //vm.debug_stats_enabled = run_options.debug_stats_enabled;
    vm.execute_from_ip(&function_to_run.ip_range.start, host_function_callback);
}

pub fn run_function_with_debug(
    vm: &mut Vm,
    function_to_run: &GenFunctionInfo,
    host_function_callback: &mut dyn HostFunctionCallback,
    run_options: RunOptions,
) {
    vm.reset_stack_and_heap_to_constant_limit();
    //vm.reset_debug();
    vm.state = VmState::Normal;
    vm.debug_opcodes_enabled = run_options.debug_opcodes_enabled;
    vm.debug_operations_enabled = run_options.debug_operations_enabled;
    vm.debug_stats_enabled = run_options.debug_stats_enabled;
    vm.set_pc(&function_to_run.ip_range.start);

    let mut debug_count = 0;

    let use_color = run_options.use_color;

    let mut last_line_info = KeepTrackOfSourceLine::new();

    while !vm.is_execution_complete() {
        debug_count += 1;

        if run_options.max_count != 0 && (debug_count >= run_options.max_count) {
            if vm.state == VmState::Normal {
                vm.internal_trap(TrapCode::StoppedByTestHarness);
            }
            break;
        }

        let pc = vm.pc();
        #[cfg(feature = "debug_vm")]
        if run_options.debug_opcodes_enabled {
            let regs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 128, 129, 130, 131, 132];
            let special_reg: u32 = unsafe {
                let ptr = vm.memory().get_heap_const_ptr(0x1E00).cast::<u32>();
                *ptr
            };

            if use_color {
                eprint!(
                    "{}",
                    tinter::bright_black(&format!(
                        "fp:{:08X}, sp:{:08X}, 1E00:{:04X}",
                        vm.fp(),
                        vm.sp(),
                        special_reg
                    ))
                );

                for reg in regs {
                    let reg_name = &format!("r{reg}");
                    eprint!(
                        "{}",
                        tinter::bright_black(&format!("{reg_name:>3}:{:08X}, ", vm.registers[reg]))
                    );
                }
                eprintln!();
            } else {
                // TODO!: Use style instead
                eprint!("{}", &format!("fp:{:08X}, sp:{:08X}, ", vm.fp(), vm.sp()));

                for reg in regs {
                    let reg_name = &format!("r{reg}");
                    eprint!("{}", &format!("{reg_name:>3}:{:08X}, ", vm.registers[reg]));
                }
                eprintln!();
            }

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

        let saved_fp = vm.memory().frame_offset();
        let hash_before: u64 = if run_options.debug_operations_enabled {
            calculate_memory_checksum(vm.all_memory_up_to(saved_fp))
        } else {
            0
        };

        //        eprintln!("constant (from address 0x34): {:?}", &vm.heap_memory()[0x34..0x34+32]);

        vm.step(host_function_callback);

        if run_options.debug_operations_enabled {
            let hash_after = calculate_memory_checksum(vm.all_memory_up_to(saved_fp));
            if hash_after != hash_before {
                eprintln!("WARN: below FP has been written to");
            }
            /*
            assert_eq!(
                hash_before, hash_after,
                "constant memory has been clobbered"
            );

             */
        }
    }
}

pub struct CompileAndCodeGenOptions {
    pub compile_options: CompileOptions,
    pub code_gen_options: CodeGenOptions,
    pub skip_codegen: bool,
}

#[must_use]
pub fn compile_and_code_gen(
    path_to_root_of_swamp_files: &Path,
    main_module_path: &[String],
    options: CompileAndCodeGenOptions,
) -> Option<(CompileAndMaybeCodeGenResult, SourceMap)> {
    let mut source_map = crate_and_registry(path_to_root_of_swamp_files);
    let current_dir = PathBuf::from(Path::new(""));

    let compile_result =
        compile_main_path(&mut source_map, main_module_path, &options.compile_options)?;

    let source_map_wrapper = SourceMapWrapper {
        source_map: &source_map,
        current_dir,
    };

    let maybe_code_gen_result = if options.skip_codegen {
        None
    } else {
        let code_gen_result = code_gen(
            &compile_result.program,
            &source_map_wrapper,
            &options.code_gen_options,
        );
        Some(code_gen_result)
    };

    Some((
        CompileAndMaybeCodeGenResult {
            compile: compile_result,
            codegen: maybe_code_gen_result,
        },
        source_map,
    ))
}

pub struct CompileCodeGenVmResult {
    pub compile: CompileResult,
    pub codegen: CodeGenAndVmResult,
}

pub enum CompileAndVmResult {
    CompileOnly(CompileResult),
    CompileAndVm(CompileCodeGenVmResult),
}

pub struct CodeGenAndVmResult {
    pub vm: Vm,
    pub code_gen_result: CodeGenResult,
    pub source_map: SourceMap,
}

impl CompileCodeGenVmResult {
    #[must_use]
    pub fn get_internal_member_function(
        &self,
        ty: &Type,
        member_function_str: &str,
    ) -> Option<&InternalFunctionDefinitionRef> {
        self.compile
            .program
            .state
            .associated_impls
            .get_internal_member_function(ty, member_function_str)
    }
    #[must_use]
    pub fn get_gen_internal_member_function(
        &self,
        ty: &Type,
        member_function_str: &str,
    ) -> Option<&GenFunctionInfo> {
        let x = self
            .get_internal_member_function(ty, member_function_str)
            .unwrap();

        self.codegen
            .code_gen_result
            .functions
            .get(&x.program_unique_id)
    }
}

/// The root module is needed so it knows which mod that should be considered.
#[must_use]
pub fn compile_codegen_and_create_vm(
    root_directory: &Path,
    root_module: &[String],
    compile_and_code_gen_options: CompileAndCodeGenOptions,
) -> Option<CompileAndVmResult> {
    let (compile_and_maybe_code_gen, source_map) =
        compile_and_code_gen(root_directory, root_module, compile_and_code_gen_options)?;

    if let Some(code_gen_result) = compile_and_maybe_code_gen.codegen {
        let vm = create_vm_with_standard_settings(
            &code_gen_result.instructions,
            &code_gen_result.prepared_constant_memory,
        );

        let code_gen_and_vm = CodeGenAndVmResult {
            vm,
            code_gen_result,
            source_map,
        };

        Some(CompileAndVmResult::CompileAndVm(CompileCodeGenVmResult {
            compile: compile_and_maybe_code_gen.compile,
            codegen: code_gen_and_vm,
        }))
    } else {
        Some(CompileAndVmResult::CompileOnly(
            compile_and_maybe_code_gen.compile,
        ))
    }
}
