/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_variables;
use crate::reg_pool::HwmTempRegisterPool;
use crate::state::GenOptions;
use crate::top_state::TopLevelGenState;
use crate::{
    FunctionInData, FunctionIp, FunctionIpKind, GenFunctionInfo, RepresentationOfRegisters,
    SpilledRegisterRegion, MAX_REGISTER_INDEX_FOR_PARAMETERS,
};
use source_map_cache::SourceMapWrapper;
use source_map_node::Node;
use std::collections::HashSet;
use swamp_semantic::{formal_function_name, InternalFunctionDefinitionRef, InternalMainExpression};
use swamp_vm_debug_info::FunctionDebugInfo;
use swamp_vm_instr_build::InstructionBuilder;
use swamp_vm_types::types::{
    Destination, FunctionInfo, FunctionInfoKind, TypedRegister, VariableRegister, VmType,
    VmTypeOrigin,
};
use swamp_vm_types::{
    InstructionPosition, InstructionPositionOffset, InstructionRange, MemoryLocation, MemoryOffset,
    PatchPosition,
};

impl TopLevelGenState {
    /// # Panics
    ///
    pub fn emit_function_def(
        &mut self,
        internal_fn_def: &InternalFunctionDefinitionRef,
        source_map_wrapper: &SourceMapWrapper,
        should_ignore_host_call: bool,
    ) {
        assert_ne!(internal_fn_def.program_unique_id, 0);

        let complete_function_name = formal_function_name(internal_fn_def);
        //info!(complete_function_name, "code generating function def");

        let in_data = FunctionInData {
            function_name_node: internal_fn_def.name.0.clone(),
            kind: FunctionInfoKind::Normal(internal_fn_def.program_unique_id as usize),
            assigned_name: complete_function_name,
            function_variables: internal_fn_def.function_variables.clone(),
            return_type: internal_fn_def.signature.return_type.clone(),
            expression: internal_fn_def.body.clone(),
        };

        let attrs = &internal_fn_def.attributes;

        let should_insert_halt =
            Self::is_test_call(attrs) || (!should_ignore_host_call && Self::is_host_call(attrs));

        let (start_ip, end_ip, function_info) =
            self.emit_function_preamble(&in_data, source_map_wrapper, should_insert_halt);

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
                    params: function_info.params.clone(),
                    return_type: self.codegen_state.layout_cache.layout(&in_data.return_type),
                    internal_function_definition: internal_fn_def.clone(),
                },
            )
            .unwrap();

        self.codegen_state.function_ips.ranges.push(FunctionIp {
            ip_range: range.clone(),
            kind: FunctionIpKind::Normal(internal_fn_def.program_unique_id),
        });

        self.codegen_state
            .debug_info
            .function_table
            .entries
            .push(FunctionDebugInfo {
                start_pc: range.start.0,
                function_id: internal_fn_def.program_unique_id,
            });

        self.codegen_state
            .debug_info
            .function_infos
            .insert(internal_fn_def.program_unique_id, function_info)
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
            &mut self.codegen_state.layout_cache,
            &main.expression.node,
            &main.scopes,
            &main.expression.ty,
        );

        let in_data = FunctionInData {
            function_name_node: main.expression.node.clone(),
            kind: FunctionInfoKind::Normal(main.program_unique_id as usize),
            assigned_name: "main_expr".to_string(),
            function_variables: main.scopes.clone(),
            return_type: main.expression.ty.clone(),
            expression: main.expression.clone(),
        };

        let (start_ip, end_ip, function_info) =
            self.emit_function_preamble(&in_data, source_map_lookup, true);

        let function_info = FunctionInfo {
            kind: FunctionInfoKind::Normal(main.program_unique_id as usize),
            frame_memory: variable_and_frame_memory.frame_memory,
            params: vec![],
            return_type: variable_and_frame_memory.return_type,
            name: "main".to_string(),
            ip_range: InstructionRange {
                start: start_ip,
                count: InstructionPositionOffset(end_ip.0 - start_ip.0),
            },
        };
    }

    #[must_use]
    pub const fn is_callee_save(reg_index: u8) -> bool {
        reg_index > MAX_REGISTER_INDEX_FOR_PARAMETERS
    }

    pub fn spill_callee_save_registers(
        code_builder: &mut CodeBuilder,
        function_info: &FunctionInfo,
        node: &Node,
    ) -> Option<SpilledRegisterRegion> {
        // Collect the actual register indices that need to be saved
        let mut registers_to_save: Vec<u8> = Vec::new();

        for variable_register in &function_info.frame_memory.variable_registers {
            if Self::is_callee_save(variable_register.register.index)
                && !variable_register.register.ty.is_mutable_primitive()
            {
                registers_to_save.push(variable_register.register.index);
            }
        }

        if registers_to_save.is_empty() {
            return None;
        }

        // Remove duplicates and sort
        registers_to_save.sort_unstable();
        registers_to_save.dedup();

        let count = registers_to_save.len() as u8;

        let abi_parameter_frame_memory_region = code_builder.temp_frame_space_for_register(
            count,
            "temporary space for callee_save (and not mutable primitives)",
        );

        // Always use contiguous range approach since callee-save registers are typically >= r6
        // and the mask approach only works for registers 0-7
        let start_reg = registers_to_save[0];

        code_builder.builder.add_st_contiguous_regs_to_frame(
            abi_parameter_frame_memory_region,
            start_reg,
            count,
            node,
            "prologue, spill contiguous range of callee-save registers to stack frame memory",
        );

        Some(SpilledRegisterRegion {
            registers: RepresentationOfRegisters::Range { start_reg, count },
            frame_memory_region: abi_parameter_frame_memory_region,
        })
    }
    pub fn initialize_and_clear_variables_that_are_on_the_frame(
        instruction_builder: &mut InstructionBuilder,
        variable_registers: &[VariableRegister],
        node: &Node,
    ) {
        for variable_reg in variable_registers {
            if let VmTypeOrigin::Frame(frame_region) = variable_reg.register.ty.origin {
                instruction_builder.add_lea_from_frame_region(
                    &variable_reg.register,
                    frame_region,
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
    }

    pub fn function_prologue(
        code_builder: &mut CodeBuilder,
        function_info: &FunctionInfo,
        node: &Node,
    ) -> (Option<SpilledRegisterRegion>, PatchPosition) {
        let enter_patch_position = code_builder
            .builder
            .add_enter_placeholder(&Node::default(), "prologue");

        let maybe_spilled = Self::spill_callee_save_registers(code_builder, function_info, node);

        // Note: Variable initialization (LEA instructions) are now generated
        // at the point of variable definition, not in the function prologue

        (maybe_spilled, enter_patch_position)
    }

    fn function_epilogue(
        instruction_builder: &mut CodeBuilder,
        maybe_spilled_registers: Option<SpilledRegisterRegion>,
        node: &Node,
        comment: &str,
    ) {
        if let Some(spilled_register_region) = maybe_spilled_registers {
            instruction_builder.emit_restore_region(
                spilled_register_region,
                &HashSet::new(),
                node,
                &format!("function epilogue: {comment}"),
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

        //info!(in_data.assigned_name, "emit_function");

        let frame_and_variable_info = layout_variables(
            &mut self.codegen_state.layout_cache,
            &in_data.function_name_node,
            &in_data.function_variables,
            &in_data.return_type,
        );

        // Get the return type layout before borrowing codegen_state mutably
        let return_basic_type = self.codegen_state.layout_cache.layout(&in_data.return_type);

        let mut params = Vec::new();
        for (index, x) in &frame_and_variable_info.parameter_and_variable_offsets {
            params.push(x.ty.basic_type.clone());
        }

        let mut function_info = FunctionInfo {
            kind: in_data.kind.clone(),
            frame_memory: frame_and_variable_info.frame_memory,
            params: params,
            return_type: frame_and_variable_info.return_type,
            name: in_data.assigned_name.clone(),
            ip_range: InstructionRange {
                start: start_ip,
                count: InstructionPositionOffset(0),
            },
        };

        // debug!(name=?in_data.assigned_name, "code generating function");

        let _complete_function_info = self.codegen_state.add_function(
            function_info.clone(),
            &in_data.function_name_node,
            "function",
        );

        let mut instruction_builder = InstructionBuilder::new(&mut self.builder_state);

        let temp_pool = HwmTempRegisterPool::new(128, 32);

        let ctx = Context::new();

        let mut function_code_builder = CodeBuilder::new(
            &mut self.codegen_state,
            &mut instruction_builder,
            frame_and_variable_info.parameter_and_variable_offsets,
            //frame_and_variable_info.frame_registers,
            temp_pool,
            frame_and_variable_info.local_frame_allocator,
            self.code_builder_options,
            source_map_wrapper,
        );

        let (maybe_spilled_registers, enter_patch_position) = Self::function_prologue(
            &mut function_code_builder,
            &function_info,
            &in_data.function_name_node,
        );
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
            if let FunctionInfoKind::Constant(found_constant) = &in_data.kind {
                function_code_builder.emit_initialize_memory_for_any_type(
                    &memory_location,
                    &in_data.expression.node,
                    "prepare r0 memory for constant",
                );
            }

            Destination::Memory(memory_location)
        };

        function_code_builder.emit_expression(&destination, &in_data.expression, &ctx);

        function_code_builder.patch_enter(enter_patch_position);

        Self::function_epilogue(
            &mut function_code_builder,
            maybe_spilled_registers,
            &in_data.expression.node,
            "epilogue",
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
