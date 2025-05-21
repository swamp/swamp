mod err_wrt;
mod trace;
use source_map_cache::{SourceMap, SourceMapWrapper};
use std::fmt::Write as FmtWrite;
use std::path::{Path, PathBuf};
use swamp_analyzer::Program;
use swamp_code_gen::{ConstantInfo, GenFunctionInfo};
use swamp_code_gen_program::{CodeGenOptions, code_gen_program};
use swamp_core_extra::prelude::SeqMap;
use swamp_dep_loader::swamp_registry_path;
use swamp_semantic::{ConstantId, InternalFunctionId};
use swamp_vm::{Vm, VmSetup, VmState};
use swamp_vm_types::{BinaryInstruction, StackMemoryAddress};

pub struct RunConstantsOptions {
    pub stderr_adapter: Option<Box<dyn FmtWrite>>,
}

pub struct RunOptions {
    //pub stderr_adapter: Option<Box<dyn FmtWrite>>,
    pub debug_stats_enabled: bool,
    pub debug_opcodes_enabled: bool,
}

pub fn run_constants_in_order(
    vm: &mut Vm,
    constants_in_order: SeqMap<ConstantId, ConstantInfo>,
    mut options: RunConstantsOptions,
) {
    for (_key, constant) in constants_in_order {
        //info!(?constant.constant_ref, "ordered constant");
        // do not reset heap, all allocations from heap should remain (for now)
        // TODO: compact the heap after each constant
        vm.reset_frame();
        unsafe {
            // Place pointer in return
            *(vm.memory_mut().get_frame_ptr_as_u32(0)) = constant.target_constant_memory.addr().0;
        }
        vm.execute_from_ip(&constant.ip_range.start);

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

    let source_map = SourceMap::new(&mounts).expect("source map failed");

    source_map
}

pub struct CodeGenResult {
    pub instructions: Vec<BinaryInstruction>,
    pub constants_in_order: SeqMap<ConstantId, ConstantInfo>,
    pub functions: SeqMap<InternalFunctionId, GenFunctionInfo>,
    pub prepared_constant_memory: Vec<u8>,
    pub program: Program,
}

pub fn compile_and_codegen_main_path(
    source_map: &mut SourceMap,
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

    let (instructions, constants_in_order, emit_function_infos, constant_memory) =
        top_gen_state.take_instructions_and_constants();

    CodeGenResult {
        instructions,
        constants_in_order,
        functions: emit_function_infos,
        prepared_constant_memory: constant_memory,
        program,
    }
}

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

pub fn run_function(vm: &mut Vm, function_to_run: &GenFunctionInfo, run_options: RunOptions) {
    // It takes no parameters, so we can just run the function
    //eprintln!("============= RUN STARTS ============");
    {
        vm.reset_stack_and_heap_to_constant_limit();
        //vm.reset_debug();
        vm.state = VmState::Normal;
        vm.debug_opcodes_enabled = run_options.debug_opcodes_enabled;
        vm.debug_stats_enabled = run_options.debug_stats_enabled;
        vm.execute_from_ip(&function_to_run.ip_range.start);
    }
}

#[must_use]
pub fn compile_and_run(
    path_to_root_of_swamp_files: &Path,
    main_module_path: &[String],
) -> CodeGenResult {
    let mut source_map = crate_and_registry(path_to_root_of_swamp_files);
    let current_dir = PathBuf::from(Path::new(""));
    let options = CodeGenOptions { show_disasm: true };

    compile_and_codegen_main_path(&mut source_map, main_module_path, &current_dir, options)
}
