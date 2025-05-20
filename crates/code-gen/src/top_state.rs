use crate::alloc::ScopeAllocator;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::{layout_type, layout_variables};
use crate::reg_pool::HwmTempRegisterPool;
use crate::state::{CodeGenState, GenOptions};
use crate::{
    ConstantInfo, FunctionInData, FunctionIp, FunctionIpKind, FunctionIps, GenFunctionInfo,
    SpilledRegister, SpilledRegisterRegion,
};
use seq_map::SeqMap;
use source_map_cache::SourceMapWrapper;
use source_map_node::Node;
use swamp_semantic::{
    ConstantId, ConstantRef, InternalFunctionDefinitionRef, InternalFunctionId,
    InternalMainExpression, pretty_module_name,
};
use swamp_types::Attributes;
use swamp_vm_instr_build::{InstructionBuilder, InstructionBuilderState, PatchPosition};
use swamp_vm_types::types::{
    CompleteFunctionInfo, FunctionInfo, FunctionInfoKind, TypedRegister, VariableRegister, VmType,
    VmTypeOrigin, is_callee_save, unknown_type,
};
use swamp_vm_types::{
    BinaryInstruction, FrameMemoryRegion, FrameMemorySize, InstructionPosition,
    InstructionPositionOffset, InstructionRange, MemoryLocation, MemoryOffset, MemorySize, Meta,
    REG_ON_FRAME_ALIGNMENT, REG_ON_FRAME_SIZE,
};
use tracing::error;

/// Top-level container that owns both states
pub struct TopLevelGenState {
    pub builder_state: InstructionBuilderState,
    pub codegen_state: CodeGenState,
}

impl Default for TopLevelGenState {
    fn default() -> Self {
        Self::new()
    }
}

impl TopLevelGenState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            builder_state: InstructionBuilderState::new(),
            codegen_state: CodeGenState::new(),
        }
    }

    #[must_use]
    pub fn function_debug_infos(&self) -> &SeqMap<InstructionPosition, FunctionInfo> {
        &self.codegen_state.function_debug_infos
    }
    #[must_use]
    pub fn function_ips(&self) -> &FunctionIps {
        &self.codegen_state.function_ips
    }

    pub fn reserve_space_for_constants(&mut self, constants: &[ConstantRef]) {
        self.codegen_state.reserve_space_for_constants(constants)
    }

    #[must_use]
    pub fn is_host_call(attributes: &Attributes) -> bool {
        !attributes.get_attributes("host_call").is_empty()
            || !attributes.get_attributes("test").is_empty()
    }

    pub fn emit_function_def(
        &mut self,
        internal_fn_def: &InternalFunctionDefinitionRef,
        source_map_wrapper: &SourceMapWrapper,
    ) {
        //info!(internal_fn_def.assigned_name, "gen_function");
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
            parameters: variable_and_frame_memory.parameters,
            name: "main".to_string(),
            ip_range: InstructionRange {
                start: start_ip.clone(),
                count: InstructionPositionOffset(end_ip.0 - start_ip.0),
            },
        };
    }

    pub fn finalize_function(&mut self, options: &GenOptions) {
        if options.is_halt_function {
            self.builder_state.add_hlt(&Node::default(), "");
        } else {
            self.builder_state.add_ret(&Node::default(), "");
        }
    }

    pub fn finalize(&mut self) {
        for function_fixup in &self.codegen_state.function_fixups {
            if let Some(func) = self.codegen_state.function_infos.get(&function_fixup.fn_id) {
                self.builder_state.patch_call(
                    PatchPosition(InstructionPosition(function_fixup.patch_position.0.0)),
                    &func.ip_range.start,
                );
            } else {
                error!(?function_fixup.fn_id, name=function_fixup.internal_function_definition.assigned_name, path=?function_fixup.internal_function_definition.defined_in_module_path,  "couldn't fixup function");
                panic!("couldn't fixup function");
            }
        }

        self.codegen_state.function_fixups.clear();
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
            instruction_builder.add_st_regs_to_frame(
                first_address,
                &registers_that_needs_to_be_spilled[0],
                variable_registers_that_needs_to_be_spilled.len() as u8,
                node,
                "save registers to stack (that will be later used in function)",
            );
            SpilledRegisterRegion {
                registers: registers_that_needs_to_be_spilled,
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
            instruction_builder.add_ld_regs_from_frame(
                &spilled_register_region.registers[0],
                spilled_register_region.frame_memory_region.addr,
                spilled_register_region.registers.len() as u8,
                node,
                &format!("restoring spilled arguments in epilogue"),
            );
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
            parameters: frame_and_variable_info.parameters,
            name: in_data.assigned_name.clone(),
            ip_range: InstructionRange {
                start: start_ip.clone(),
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

        let ctx = Context::new_from_parameters(0, frame_and_variable_info.highest_register_used);

        let mut function_code_builder = CodeBuilder::new(
            &mut self.codegen_state,
            &mut instruction_builder,
            frame_and_variable_info.parameter_and_variable_offsets,
            frame_and_variable_info.frame_registers,
            temp_pool,
            temp_frame_allocator,
            source_map_wrapper,
        );

        //info!(?in_data, "generate");

        let return_basic_type = layout_type(&in_data.return_type);
        let return_register =
            TypedRegister::new_vm_type(0, VmType::new_unknown_placement(return_basic_type));

        if in_data.return_type.is_primitive() {
            function_code_builder.emit_scalar_rvalue_to_specific_register(
                &return_register,
                &in_data.expression,
                &ctx,
            );
        } else {
            let memory_location = MemoryLocation {
                ty: return_register.ty.basic_type.clone(),
                base_ptr_reg: return_register,
                offset: MemoryOffset(0),
            };

            function_code_builder.emit_expression_into_target_memory(
                &memory_location,
                &in_data.expression,
                "function root expression",
                &ctx,
            );
        }

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

    pub fn emit_constants_expression_functions_in_order(
        &mut self,
        constants: &[ConstantRef],
        source_map_wrapper: &SourceMapWrapper,
    ) {
        for constant in constants {
            let target_region = self
                .codegen_state
                .constant_offsets
                .get(&constant.id)
                .unwrap()
                .clone();

            let in_data = FunctionInData {
                function_name_node: constant.name.clone(),
                kind: FunctionInfoKind::Constant(constant.id as usize),
                assigned_name: constant.assigned_name.clone(),
                function_variables: constant.function_scope_state.clone(),
                parameter_variables: vec![],
                return_type: constant.resolved_type.clone(),
                expression: constant.expr.clone(),
            };

            let (start_ip, end_ip, function_info) =
                self.emit_function_preamble(&in_data, source_map_wrapper, true);

            let constant_info = ConstantInfo {
                ip_range: InstructionRange {
                    count: InstructionPositionOffset(end_ip.0 - start_ip.0),
                    start: start_ip.clone(),
                },
                target_constant_memory: target_region,
                constant_ref: constant.clone(),
            };

            self.codegen_state
                .constant_functions_in_order
                .insert(constant.id, constant_info)
                .unwrap();

            self.codegen_state
                .function_debug_infos
                .insert(start_ip, function_info)
                .unwrap();
        }
    }

    #[must_use]
    pub fn take_instructions_and_constants(
        self,
    ) -> (
        Vec<BinaryInstruction>,
        SeqMap<ConstantId, ConstantInfo>,
        SeqMap<InternalFunctionId, GenFunctionInfo>,
        Vec<u8>,
    ) {
        (
            self.builder_state.instructions,
            self.codegen_state.constant_functions_in_order,
            self.codegen_state.function_infos,
            self.codegen_state.constants_manager.take_data(),
        )
    }

    #[must_use]
    pub fn instructions(&self) -> &[BinaryInstruction] {
        &self.builder_state.instructions
    }

    #[must_use]
    pub fn ip(&self) -> InstructionPosition {
        InstructionPosition(self.builder_state.instructions.len() as u32)
    }
    #[must_use]
    pub fn meta(&self) -> &[Meta] {
        &self.builder_state.meta
    }
}
