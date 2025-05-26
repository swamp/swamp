use crate::alloc::ScopeAllocator;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::{layout_type, layout_variables};
use crate::reg_pool::HwmTempRegisterPool;
use crate::state::GenOptions;
use crate::top_state::TopLevelGenState;
use crate::{
    FunctionInData, FunctionIp, FunctionIpKind, GenFunctionInfo, RepresentationOfRegisters,
    SpilledRegister, SpilledRegisterRegion,
};
use source_map_cache::SourceMapWrapper;
use source_map_node::Node;
use swamp_semantic::{InternalFunctionDefinitionRef, InternalMainExpression, pretty_module_name};
use swamp_vm_instr_build::InstructionBuilder;
use swamp_vm_types::types::{
    Destination, FunctionInfo, FunctionInfoKind, TypedRegister, VmType, VmTypeOrigin,
    is_callee_save,
};
use swamp_vm_types::{
    FrameMemoryRegion, InstructionPosition, InstructionPositionOffset, InstructionRange,
    MemoryLocation, MemoryOffset, MemorySize, PatchPosition, REG_ON_FRAME_ALIGNMENT,
    REG_ON_FRAME_SIZE,
};

impl TopLevelGenState {
    pub fn emit_function_def(
        &mut self,
        internal_fn_def: &InternalFunctionDefinitionRef,
        source_map_wrapper: &SourceMapWrapper,
    ) {
        assert_ne!(internal_fn_def.program_unique_id, 0);

        let complete_function_name = format!(
            "{}::{}",
            pretty_module_name(&internal_fn_def.defined_in_module_path),
            internal_fn_def.assigned_name
        );

        let in_data = FunctionInData {
            function_name_node: internal_fn_def.name.0.clone(),
            kind: FunctionInfoKind::Normal(internal_fn_def.program_unique_id as usize),
            assigned_name: complete_function_name,
            function_variables: internal_fn_def.function_variables.clone(),
            parameter_variables: internal_fn_def.parameters.clone(),
            return_type: *internal_fn_def.signature.signature.return_type.clone(),
            expression: internal_fn_def.body.clone(),
        };

        let is_host_call = Self::is_host_call(&internal_fn_def.attributes);

        let (start_ip, end_ip, function_info) =
            self.emit_function_preamble(&in_data, source_map_wrapper, is_host_call);

        let count_ip = end_ip.0 - start_ip.0;

        let range = InstructionRange {
            start: start_ip,
            count: InstructionPositionOffset(count_ip),
        };

        self.codegen_state
            .function_infos
            .insert(
                internal_fn_def.program_unique_id,
                GenFunctionInfo {
                    ip_range: range.clone(),
                    internal_function_definition: internal_fn_def.clone(),
                },
            )
            .unwrap();

        self.codegen_state.function_ips.ranges.push(FunctionIp {
            ip_range: range.clone(),
            kind: FunctionIpKind::Normal(internal_fn_def.program_unique_id),
        });

        self.codegen_state
            .function_debug_infos
            .insert(range.start, function_info)
            .unwrap();
    }

    /// # Errors
    ///
    pub fn emit_main_function(
        &mut self,
        main: &InternalMainExpression,
        options: &GenOptions,
        source_map_lookup: &SourceMapWrapper,
    ) {
        let variable_and_frame_memory = layout_variables(
            &main.expression.node,
            &main.function_parameters,
            &main.function_variables,
            &main.expression.ty,
        );

        let in_data = FunctionInData {
            function_name_node: main.expression.node.clone(),
            kind: FunctionInfoKind::Normal(main.program_unique_id as usize),
            assigned_name: "main_expr".to_string(),
            function_variables: main.function_variables.clone(),
            parameter_variables: main.function_parameters.clone(),
            return_type: main.expression.ty.clone(),
            expression: main.expression.clone(),
        };

        let (start_ip, end_ip, function_info) =
            self.emit_function_preamble(&in_data, source_map_lookup, true);

        let function_info = FunctionInfo {
            kind: FunctionInfoKind::Normal(main.program_unique_id as usize),
            frame_memory: variable_and_frame_memory.frame_memory,
            return_type: variable_and_frame_memory.return_type,
            parameters: variable_and_frame_memory.parameters,
            name: "main".to_string(),
            ip_range: InstructionRange {
                start: start_ip,
                count: InstructionPositionOffset(end_ip.0 - start_ip.0),
            },
        };
    }

    pub fn function_prologue(
        instruction_builder: &mut InstructionBuilder,
        function_info: &FunctionInfo,
        temp_frame_allocator: &mut ScopeAllocator,
        node: &Node,
    ) -> (Option<SpilledRegisterRegion>, PatchPosition) {
        let enter_patch_position =
            instruction_builder.add_enter_placeholder(&Node::default(), "prologue");

        let mut saved_registers = Vec::new();
        let maybe_first_reg_position_that_needs_to_be_spilled = function_info
            .frame_memory
            .variable_registers
            .iter()
            .position(|reg| is_callee_save(reg.register.index));

        let maybe_spilled = maybe_first_reg_position_that_needs_to_be_spilled.map(|first_index| {
            let variable_registers_that_needs_to_be_spilled: Vec<_> = function_info
                .frame_memory
                .variable_registers
                .iter()
                .skip(first_index)
                .collect();

            for variable_reg in &variable_registers_that_needs_to_be_spilled {
                let frame_placed_addr =
                    temp_frame_allocator.allocate(REG_ON_FRAME_SIZE, REG_ON_FRAME_ALIGNMENT);

                let frame_region = FrameMemoryRegion {
                    addr: frame_placed_addr,
                    size: REG_ON_FRAME_SIZE,
                };

                let spilled = SpilledRegister {
                    register: variable_reg.register.clone(),
                    frame_memory_region: frame_region,
                };
                saved_registers.push(spilled);
            }

            let registers_that_needs_to_be_spilled: Vec<_> =
                variable_registers_that_needs_to_be_spilled
                    .iter()
                    .map(|var_reg| var_reg.register.clone())
                    .collect();

            let first = saved_registers.first().unwrap();
            let first_address = first.frame_memory_region.addr;
            let last_frame_location = saved_registers.last().unwrap();
            let last_addr = last_frame_location.frame_memory_region.addr
                + MemoryOffset(last_frame_location.frame_memory_region.size.0);
            let total_frame_size_for_vars = MemorySize(last_addr.0 - first_address.0);
            instruction_builder.add_st_regs_to_frame_using_range(
                first.frame_memory_region,
                registers_that_needs_to_be_spilled[0].index,
                variable_registers_that_needs_to_be_spilled.len() as u8,
                node,
                "save registers to stack (that will be later used in function)",
            );
            SpilledRegisterRegion {
                registers: RepresentationOfRegisters::Individual(
                    registers_that_needs_to_be_spilled,
                ),
                frame_memory_region: FrameMemoryRegion {
                    addr: first_address,
                    size: total_frame_size_for_vars,
                },
            }
        });

        for variable_reg in &function_info.frame_memory.variable_registers {
            if let VmTypeOrigin::Frame(frame_region) = variable_reg.register.ty.origin {
                instruction_builder.add_lea(
                    &variable_reg.register,
                    frame_region.addr,
                    node,
                    &format!("define frame placed register {variable_reg}"),
                );

                instruction_builder.add_frame_memory_clear(
                    frame_region,
                    node,
                    &format!("clear memory for indirect variable {variable_reg}"),
                );
            }
        }

        (maybe_spilled, enter_patch_position)
    }

    fn function_epilogue(
        instruction_builder: &mut CodeBuilder,
        maybe_spilled_registers: Option<SpilledRegisterRegion>,
        node: &Node,
    ) {
        if let Some(spilled_register_region) = maybe_spilled_registers {
            match spilled_register_region.registers {
                RepresentationOfRegisters::Individual(registers) => {
                    instruction_builder.add_ld_regs_from_frame(
                        &registers[0],
                        spilled_register_region.frame_memory_region,
                        registers.len() as u8,
                        node,
                        "restoring spilled arguments in epilogue",
                    );
                }
                RepresentationOfRegisters::Mask(_) => todo!(),
                RepresentationOfRegisters::Range { .. } => todo!(),
            }
        }
    }

    pub fn emit_function_preamble(
        &mut self,
        in_data: &FunctionInData,
        source_map_wrapper: &SourceMapWrapper,
        is_called_by_host: bool,
    ) -> (InstructionPosition, InstructionPosition, FunctionInfo) {
        let start_ip = self.ip();

        let frame_and_variable_info = layout_variables(
            &in_data.function_name_node,
            &in_data.parameter_variables,
            &in_data.function_variables,
            &in_data.return_type,
        );

        let mut function_info = FunctionInfo {
            kind: in_data.kind.clone(),
            frame_memory: frame_and_variable_info.frame_memory,
            return_type: frame_and_variable_info.return_type,
            parameters: frame_and_variable_info.parameters,
            name: in_data.assigned_name.clone(),
            ip_range: InstructionRange {
                start: start_ip,
                count: InstructionPositionOffset(0),
            },
        };

        let _complete_function_info = self.codegen_state.add_function(
            function_info.clone(),
            &in_data.function_name_node,
            "function",
        );

        let mut instruction_builder = InstructionBuilder::new(&mut self.builder_state);

        let mut temp_frame_allocator =
            ScopeAllocator::new(frame_and_variable_info.temp_allocator_region);

        let (maybe_spilled_registers, enter_patch_position) = Self::function_prologue(
            &mut instruction_builder,
            &function_info,
            &mut temp_frame_allocator,
            &in_data.function_name_node,
        );

        let temp_pool = HwmTempRegisterPool::new(128, 64);

        let ctx = Context {};

        let mut function_code_builder = CodeBuilder::new(
            &mut self.codegen_state,
            &mut instruction_builder,
            frame_and_variable_info.parameter_and_variable_offsets,
            frame_and_variable_info.frame_registers,
            temp_pool,
            temp_frame_allocator,
            source_map_wrapper,
        );

        let return_basic_type = layout_type(&in_data.return_type);
        let return_register =
            TypedRegister::new_vm_type(0, VmType::new_unknown_placement(return_basic_type));

        let destination = if return_register.ty.basic_type.is_scalar() {
            Destination::Register(return_register)
        } else {
            let memory_location = MemoryLocation {
                ty: VmType::new_unknown_placement(return_register.ty().clone()),
                base_ptr_reg: return_register,
                offset: MemoryOffset(0),
            };
            Destination::Memory(memory_location)
        };

        function_code_builder.emit_expression(&destination, &in_data.expression, &ctx);

        function_code_builder.patch_enter(enter_patch_position);

        Self::function_epilogue(
            &mut function_code_builder,
            maybe_spilled_registers,
            &in_data.expression.node,
        );

        self.finalize_function(&GenOptions {
            is_halt_function: is_called_by_host,
        });

        let end_ip = self.ip();

        function_info.ip_range.count = InstructionPositionOffset(end_ip.0 - start_ip.0);

        (start_ip, end_ip, function_info)
    }

    pub fn finalize_function(&mut self, options: &GenOptions) {
        if options.is_halt_function {
            self.builder_state.add_hlt(&Node::default(), "");
        } else {
            self.builder_state.add_ret(&Node::default(), "");
        }
    }
}
