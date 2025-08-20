/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod err_wrt;
pub mod prelude;

use seq_map::SeqMap;
use source_map_cache::{FileId, KeepTrackOfSourceLine, SourceFileLineInfo, SourceMapLookup};
use source_map_cache::{SourceMap, SourceMapWrapper};
use std::env::current_dir;
use std::fmt::Write as FmtWrite;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Path, PathBuf};
use swamp_analyzer::Program;
use swamp_code_gen::{ConstantInfo, GenFunctionInfo};
use swamp_code_gen_program::{CodeGenOptions, code_gen_program};
pub use swamp_compile::CompileOptions;
use swamp_dep_loader::{RunMode, swamp_registry_path};
use swamp_semantic::{ConstantId, InternalFunctionDefinitionRef, InternalFunctionId};
use swamp_std::pack::pack;
use swamp_std::print::print_fn;
use swamp_types::TypeRef;
use swamp_vm::host::{HostArgs, HostFunctionCallback};
use swamp_vm::memory::ExecutionMode;
use swamp_vm::{Vm, VmSetup, VmState};
use swamp_vm_debug_info::{DebugInfo, DebugInfoForPc};
use swamp_vm_disasm::{disasm_color, display_lines};
use swamp_vm_isa::{BinaryInstruction, InstructionPosition};
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::InstructionRange;
use swamp_vm_types::types::BasicTypeKind;

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
    pub debug_memory_enabled: bool,
}

pub fn run_constants_in_order(
    vm: &mut Vm,
    constants_in_order: &SeqMap<ConstantId, ConstantInfo>,
    host_function_callback: &mut dyn HostFunctionCallback,
    options: &RunOptions,
) {
    vm.memory_mut().set_heap_directly_after_constant_area();
    for (_key, constant) in constants_in_order {
        // do not reset heap, all allocations from heap should remain (for now)
        vm.reset_call_stack();

        if constant.target_constant_memory.ty().is_scalar() {
        } else {
            // set memory location into to r0
            vm.registers[0] = constant.target_constant_memory.addr().0;
        }

        run_function_with_debug(vm, &constant.ip_range, host_function_callback, options);

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

        let return_layout = constant.target_constant_memory.ty();

        assert_eq!(
            constant.target_constant_memory.size(),
            return_layout.total_size
        );

        // TODO: Bring this back
        /*
        if let Some(x) = &mut options.stderr_adapter {
            swamp_vm_pretty_print::print_value(
                x.as_mut(),
                vm.frame_memory(),
                vm.memory(),
                StackMemoryAddress(0),
                return_layout,
                &constant.constant_ref.assigned_name,
            )
                .unwrap();
        }

         */
    }

    // We need to properly preserve string headers when incorporating heap into constant area
    vm.memory_mut().incorporate_heap_into_constant_area();
}

// "/Users/peter/external/swamp_autobattler/scripts"
#[must_use]
pub fn crate_and_registry(path_to_swamp: &Path, run_mode: &RunMode) -> SourceMap {
    let mut mounts = SeqMap::new();
    //  let path_buf = Path::new(path_to_swamp).to_path_buf();
    mounts
        .insert("crate".to_string(), path_to_swamp.to_path_buf())
        .unwrap();

    if let Some(found_swamp_home) = swamp_registry_path(run_mode) {
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
    pub layout_cache: LayoutCache,
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

    let all_types = top_gen_state.codegen_state.layout_cache.clone();
    let (instructions, constants_in_order, emit_function_infos, constant_memory, debug_info) =
        top_gen_state.take_instructions_and_constants();

    CodeGenResult {
        debug_info,
        instructions,
        constants_in_order,
        functions: emit_function_infos,
        prepared_constant_memory: constant_memory,
        layout_cache: all_types,
    }
}

#[must_use]
pub fn create_vm_with_standard_settings(
    instructions: &[BinaryInstruction],
    prepared_constant_memory: &[u8],
) -> Vm {
    let vm_setup = VmSetup {
        stack_memory_size: 1024 * 1024 * 1024, // 1 GiB
        heap_memory_size: 512 * 1024,          // 512 KiB for transient heap allocation (strings)
        constant_memory: prepared_constant_memory.to_vec(),
        debug_opcodes_enabled: false,
        debug_stats_enabled: false,
        debug_operations_enabled: false,
    };

    Vm::new(instructions.to_vec(), vm_setup)
}

pub struct VmOptions {
    pub stack_size: usize,
    pub heap_size: usize,
}

#[must_use]
pub fn create_vm_with_options(
    instructions: &[BinaryInstruction],
    prepared_constant_memory: &[u8],
    vm_options: &VmOptions,
) -> Vm {
    let vm_setup = VmSetup {
        stack_memory_size: vm_options.stack_size,
        heap_memory_size: vm_options.heap_size,
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
    options: &RunOptions,
) {
    run_constants_in_order(vm, constants_in_order, host_function_callback, options);
}

pub fn run_as_fast_as_possible(
    vm: &mut Vm,
    function_to_run: &GenFunctionInfo,
    host_function_callback: &mut dyn HostFunctionCallback,
    run_options: RunOptions,
) {
    vm.reset_minimal_stack_and_fp();
    vm.state = VmState::Normal;
    vm.debug_opcodes_enabled = false;
    vm.debug_operations_enabled = run_options.debug_operations_enabled;
    vm.debug_stats_enabled = run_options.debug_stats_enabled;

    vm.execute_from_ip(&function_to_run.ip_range.start, host_function_callback);
    if matches!(vm.state, VmState::Trap(_) | VmState::Panic(_)) {
        show_crash_info(vm, run_options.debug_info, &run_options.source_map_wrapper);
    }
}

fn calculate_memory_checksum(memory: &[u8]) -> u64 {
    let mut hasher = DefaultHasher::new();
    memory.hash(&mut hasher);
    hasher.finish()
}

pub fn run_function(
    vm: &mut Vm,
    ip_range: &InstructionRange,
    host_function_callback: &mut dyn HostFunctionCallback,
    run_options: &RunOptions,
) {
    vm.reset_minimal_stack_and_fp();

    vm.state = VmState::Normal;

    vm.execute_from_ip(&ip_range.start, host_function_callback);

    if matches!(vm.state, VmState::Trap(_) | VmState::Panic(_)) {
        show_crash_info(vm, run_options.debug_info, &run_options.source_map_wrapper);
    }
}

pub fn show_call_stack(
    vm: &Vm,
    debug_info: &DebugInfo,
    info: &DebugInfoForPc,
    source_map_wrapper: &SourceMapWrapper,
) {
    // Show call stack
    eprintln!("\n📞 Call stack:");

    // Current function (where the crash occurred)
    if info.meta.node.span.file_id != 0 {
        let (line, _column) = source_map_wrapper.source_map.get_span_location_utf8(
            info.meta.node.span.file_id,
            info.meta.node.span.offset as usize,
        );
        let relative_path = source_map_wrapper
            .source_map
            .fetch_relative_filename(info.meta.node.span.file_id);

        // Get the actual source line
        if let Some(source_line) =
            source_map_wrapper.get_source_line(info.meta.node.span.file_id as FileId, line)
        {
            eprintln!(
                "  {}: {} {}",
                tinter::bright_cyan("0"),
                tinter::blue(&info.function_debug_info.name),
                tinter::bright_black("(current)")
            );
            eprintln!(
                "{:4} {} {}",
                tinter::bright_black(&line.to_string()),
                tinter::green("|"),
                source_line.trim()
            );
            eprintln!(
                "{} {}",
                tinter::bright_black("->"),
                tinter::cyan(&format!("{relative_path}:{line}"))
            );
        } else {
            eprintln!(
                "  {}: {} {}",
                tinter::bright_cyan("0"),
                tinter::blue(&info.function_debug_info.name),
                tinter::bright_black("(current)")
            );
            eprintln!(
                "{} {}",
                tinter::bright_black("->"),
                tinter::cyan(&format!("{relative_path}:{line}"))
            );
        }
    } else {
        eprintln!(
            "  {}: {} {}",
            tinter::bright_cyan("0"),
            tinter::blue(&info.function_debug_info.name),
            tinter::bright_black("(current)")
        );
    }

    let call_stack = vm.call_stack();
    for (i, frame) in call_stack.iter().enumerate().rev() {
        let frame_pc = if frame.return_address > 0 {
            frame.return_address - 1
        } else {
            frame.return_address
        };

        if let Some(frame_info) = debug_info.fetch(frame_pc) {
            if frame_info.meta.node.span.file_id != 0 {
                let (line, _column) = source_map_wrapper.source_map.get_span_location_utf8(
                    frame_info.meta.node.span.file_id,
                    frame_info.meta.node.span.offset as usize,
                );
                let relative_path = source_map_wrapper
                    .source_map
                    .fetch_relative_filename(frame_info.meta.node.span.file_id);

                if let Some(source_line) = source_map_wrapper
                    .get_source_line(frame_info.meta.node.span.file_id as FileId, line)
                {
                    eprintln!(
                        "  {}: {}",
                        tinter::bright_cyan(&(i + 1).to_string()),
                        tinter::blue(&frame_info.function_debug_info.name)
                    );
                    eprintln!(
                        "{:4} {} {}",
                        tinter::bright_black(&line.to_string()),
                        tinter::green("|"),
                        source_line.trim()
                    );
                    eprintln!(
                        "{} {}",
                        tinter::bright_black("->"),
                        tinter::cyan(&format!("{relative_path}:{line}"))
                    );
                } else {
                    eprintln!(
                        "  {}: {}",
                        tinter::bright_cyan(&(i + 1).to_string()),
                        tinter::blue(&frame_info.function_debug_info.name)
                    );
                    eprintln!(
                        "{} {}",
                        tinter::bright_black("->"),
                        tinter::cyan(&format!("{relative_path}:{line}"))
                    );
                }
            } else {
                eprintln!(
                    "  {}: {}",
                    tinter::bright_cyan(&(i + 1).to_string()),
                    tinter::blue(&frame_info.function_debug_info.name)
                );
            }
        } else {
            eprintln!(
                "  {}: {} {}",
                tinter::bright_cyan(&(i + 1).to_string()),
                tinter::red("<unknown function>"),
                tinter::bright_black(&format!("(pc: {frame_pc:04X})"))
            );
        }
    }
}

pub fn show_crash_info(vm: &Vm, debug_info: &DebugInfo, source_map_wrapper: &SourceMapWrapper) {
    let pc = vm.pc();

    // PC has advanced past the instruction that caused the trap/panic, so look at pc - 1

    let trap_pc = if pc > 0 { pc - 1 } else { pc };

    if let Some(info) = debug_info.fetch(trap_pc) {
        match &vm.state {
            VmState::Trap(trap_code) => {
                eprintln!("\n🚫 VM TRAP: {trap_code}");
            }
            VmState::Panic(message) => {
                eprintln!("\n💥 VM PANIC: {message}");
            }
            _ => unreachable!(),
        }

        if info.meta.node.span.file_id != 0 {
            let (line, column) = source_map_wrapper.source_map.get_span_location_utf8(
                info.meta.node.span.file_id,
                info.meta.node.span.offset as usize,
            );

            eprintln!("📍 Source location:");
            let mut string = String::new();
            display_lines(
                &mut string,
                info.meta.node.span.file_id as FileId,
                line.saturating_sub(2), // Show a couple lines before
                line + 2,               // Show a couple lines after
                source_map_wrapper,
                true,
            );
            eprint!("{string}");

            let relative_path = source_map_wrapper
                .source_map
                .fetch_relative_filename(info.meta.node.span.file_id);
            eprintln!("   at {relative_path}:{line}:{column}");
        }

        // Also show the instruction that caused the trap/panic
        let instruction = &vm.instructions()[trap_pc];
        let disassembler_string = disasm_color(
            instruction,
            &info.function_debug_info.frame_memory,
            &info.meta,
            &InstructionPosition(trap_pc as u32),
        );
        eprintln!("💻 Failing instruction: {trap_pc:04X}> {disassembler_string}");

        show_call_stack(vm, debug_info, &info, source_map_wrapper);
    }
}

pub fn run_function_with_debug(
    vm: &mut Vm,
    function_to_run: &InstructionRange,
    host_function_callback: &mut dyn HostFunctionCallback,
    run_options: &RunOptions,
) {
    vm.reset_minimal_stack_and_fp();

    vm.state = VmState::Normal;
    vm.debug_opcodes_enabled = run_options.debug_opcodes_enabled;
    vm.debug_operations_enabled = run_options.debug_operations_enabled;
    vm.debug_stats_enabled = run_options.debug_stats_enabled;
    vm.set_pc(&function_to_run.start);

    let use_color = run_options.use_color;

    let mut last_line_info = KeepTrackOfSourceLine::new();

    let saved_fp = vm.memory().constant_memory_size;
    let hash_before: u64 = if run_options.debug_memory_enabled {
        calculate_memory_checksum(vm.all_memory_up_to(saved_fp))
    } else {
        0
    };

    while !vm.is_execution_complete() {
        let pc = vm.pc();
        #[cfg(feature = "debug_vm")]
        if run_options.debug_opcodes_enabled {
            let regs = [
                0, 1, 2, 3, 4, 5, 6, 7, 8, 128, 129, 130, 131, 132, 133, 134, 135,
            ];
            if use_color {
                print!(
                    "{}",
                    tinter::bright_black(&format!("fp:{:08X}, sp:{:08X} ", vm.fp(), vm.sp(),))
                );

                for reg in regs {
                    let reg_name = &format!("r{reg}");
                    print!(
                        "{}",
                        tinter::bright_black(&format!("{reg_name:>3}:{:08X}, ", vm.registers[reg]))
                    );
                }
                println!();
            } else {
                // TODO!: Use style instead
                print!("{}", &format!("fp:{:08X}, sp:{:08X}, ", vm.fp(), vm.sp()));

                for reg in regs {
                    let reg_name = &format!("r{reg}");
                    print!("{}", &format!("{reg_name:>3}:{:08X}, ", vm.registers[reg]));
                }
                println!();
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
                        true,
                    );
                    print!("{string}");
                }
            }

            let instruction = &vm.instructions()[pc];
            let string = disasm_color(
                instruction,
                &info.function_debug_info.frame_memory,
                &info.meta,
                &InstructionPosition(pc as u32),
            );
            println!("{pc:04X}> {string}");
        }

        vm.step(host_function_callback);

        /*
        let start_map_addr = 0x1154A8;
        let start_buckets = start_map_addr + MAP_BUCKETS_OFFSET.0;
        let bucket_index = 61;
        let bucket_size = 8156;
        let value_offset_in_bucket = 8;
        //let total_addr = start_buckets + bucket_index * bucket_size;
        let total_addr = 0x1194C8;
        //
        let read = unsafe { slice::from_raw_parts(vm.memory().get_heap_const_ptr(total_addr as usize), 32) };
        let data = read[0];
        if data == 1 || (debug_count % 60) == 0 {
            eprintln!("data: {read:?}");
            if data == 1 {
                vm.internal_trap(TrapCode::StoppedByTestHarness);
            }
        }
        debug_count += 1;

         */

        // Check if VM encountered a trap or panic and show source information
        if matches!(vm.state, VmState::Trap(_) | VmState::Panic(_)) {
            show_crash_info(vm, run_options.debug_info, &run_options.source_map_wrapper);
        }

        if run_options.debug_memory_enabled
            && vm.memory().execution_mode != ExecutionMode::ConstantEvaluation
        {
            // During constant execution, of course the constant memory changes
            let hash_after = calculate_memory_checksum(vm.all_memory_up_to(saved_fp));
            //vm.state = VmState::Trap(TrapCode::VecBoundsFail);
            assert!(
                (hash_after == hash_before),
                "INTERNAL ERROR: constant memory has been written to"
            );
        }
    }
}

pub struct CompileAndCodeGenOptions {
    pub compile_options: CompileOptions,
    pub code_gen_options: CodeGenOptions,
    pub skip_codegen: bool,
    pub run_mode: RunMode,
}

pub struct CompileCodeGenAndVmOptions {
    pub vm_options: VmOptions,
    pub codegen: CompileAndCodeGenOptions,
}

#[must_use]
pub fn compile_and_code_gen(
    source_map: &mut SourceMap,
    main_module_path: &[String],
    options: &CompileAndCodeGenOptions,
) -> Option<CompileAndMaybeCodeGenResult> {
    let current_dir = PathBuf::from(Path::new(""));

    let compile_result = compile_main_path(source_map, main_module_path, &options.compile_options)?;

    let source_map_wrapper = SourceMapWrapper {
        source_map,
        current_dir,
    };

    let maybe_code_gen_result =
        if options.skip_codegen || !compile_result.program.state.errors().is_empty() {
            None
        } else {
            let code_gen_result = code_gen(
                &compile_result.program,
                &source_map_wrapper,
                &options.code_gen_options,
            );
            Some(code_gen_result)
        };

    Some(CompileAndMaybeCodeGenResult {
        compile: compile_result,
        codegen: maybe_code_gen_result,
    })
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
}

pub struct StandardOnlyHostCallbacks {}

impl HostFunctionCallback for StandardOnlyHostCallbacks {
    fn dispatch_host_call(&mut self, args: HostArgs) {
        match args.function_id {
            1 => print_fn(args),
            2 => pack(args),

            _ => panic!("unknown external {}", args.function_id),
        }
    }
}

impl CompileCodeGenVmResult {
    #[must_use]
    pub fn get_internal_member_function(
        &self,
        ty: &TypeRef,
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
        ty: &TypeRef,
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

#[must_use]
pub fn compile_codegen_and_create_vm_and_run_first_time(
    source_map: &mut SourceMap,
    root_module: &[String],
    compile_and_code_gen_options: CompileCodeGenAndVmOptions,
) -> Option<CompileAndVmResult> {
    let result =
        compile_codegen_and_create_vm(source_map, root_module, &compile_and_code_gen_options);
    if let Some(mut first_result) = result {
        if let CompileAndVmResult::CompileAndVm(found_result) = &mut first_result {
            let mut callbacks = StandardOnlyHostCallbacks {};
            let run_first_options = RunOptions {
                debug_stats_enabled: false,
                debug_opcodes_enabled: false,
                debug_operations_enabled: false,
                use_color: true,
                max_count: 0,
                debug_info: &found_result.codegen.code_gen_result.debug_info,
                source_map_wrapper: SourceMapWrapper {
                    source_map,
                    current_dir: current_dir().unwrap(),
                },
                debug_memory_enabled: false,
            };

            run_first_time(
                &mut found_result.codegen.vm,
                &found_result.codegen.code_gen_result.constants_in_order,
                &mut callbacks,
                &run_first_options,
            );
            Some(first_result)
        } else {
            None
        }
    } else {
        None
    }
}

/// The root module is needed so it knows which mod that should be considered.
#[must_use]
pub fn compile_codegen_and_create_vm(
    source_map: &mut SourceMap,
    root_module: &[String],
    compile_and_code_gen_options: &CompileCodeGenAndVmOptions,
) -> Option<CompileAndVmResult> {
    let compile_and_maybe_code_gen = compile_and_code_gen(
        source_map,
        root_module,
        &compile_and_code_gen_options.codegen,
    )?;

    if let Some(code_gen_result) = compile_and_maybe_code_gen.codegen {
        let vm = create_vm_with_options(
            &code_gen_result.instructions,
            &code_gen_result.prepared_constant_memory,
            &compile_and_code_gen_options.vm_options,
        );

        let code_gen_and_vm = CodeGenAndVmResult {
            vm,
            code_gen_result,
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
