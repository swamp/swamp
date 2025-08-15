/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilderOptions;
use crate::state::CodeGenState;
use crate::{ConstantInfo, FunctionInData, FunctionIps, GenFunctionInfo};
use seq_map::SeqMap;
use source_map_cache::SourceMapWrapper;
use swamp_semantic::{ConstantId, ConstantRef, InternalFunctionId};

use swamp_attributes::Attributes;
use swamp_vm_debug_info::{DebugInfo, FunctionDebugInfo};
use swamp_vm_instr_build::{InstructionBuilderState, PatchPosition};
use swamp_vm_isa::{BinaryInstruction, InstructionPosition};
use swamp_vm_types::types::FunctionInfoKind;
use swamp_vm_types::{InstructionPositionOffset, InstructionRange, Meta};
use tracing::error;

/// Top-level container that owns both states
pub struct TopLevelGenState {
    pub builder_state: InstructionBuilderState,
    pub codegen_state: CodeGenState,
    pub code_builder_options: CodeBuilderOptions,
}

impl TopLevelGenState {
    #[must_use]
    pub fn new(code_builder_options: CodeBuilderOptions) -> Self {
        Self {
            builder_state: InstructionBuilderState::new(),
            codegen_state: CodeGenState::new(),
            code_builder_options,
        }
    }

    #[must_use]
    pub const fn debug_info(&self) -> &DebugInfo {
        &self.codegen_state.debug_info
    }
    #[must_use]
    pub const fn function_ips(&self) -> &FunctionIps {
        &self.codegen_state.function_ips
    }

    pub fn reserve_space_for_constants(&mut self, constants: &[ConstantRef]) {
        self.codegen_state.reserve_space_for_constants(constants);
    }

    #[must_use]
    pub fn is_host_call(attributes: &Attributes) -> bool {
        !attributes.get_attributes("host_call").is_empty()
    }
    #[must_use]
    pub fn is_test_call(attributes: &Attributes) -> bool {
        !attributes.get_attributes("test").is_empty()
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
                panic!(
                    "couldn't fixup function {}",
                    function_fixup.internal_function_definition.assigned_name
                );
            }
        }

        self.codegen_state.debug_info.info_for_each_instruction = self.builder_state.meta.clone();

        self.codegen_state.function_fixups.clear();
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
                return_type: constant.resolved_type.clone(),
                expression: constant.expr.clone(),
            };

            let (start_ip, end_ip, function_info) =
                self.emit_function_preamble(&in_data, source_map_wrapper, true);

            let return_type_basic_type = self.codegen_state.layout_cache.layout(&in_data.return_type);

            let constant_info = ConstantInfo {
                ip_range: InstructionRange {
                    count: InstructionPositionOffset(end_ip.0 - start_ip.0),
                    start: start_ip,
                },
                return_type: return_type_basic_type,
                target_constant_memory: target_region,
                constant_ref: constant.clone(),
            };

            self.codegen_state
                .constant_functions_in_order
                .insert(constant.id, constant_info)
                .unwrap();

            self.codegen_state
                .debug_info
                .function_table
                .entries
                .push(FunctionDebugInfo {
                    start_pc: start_ip.0,
                    function_id: constant.id as u16,
                });

            self.codegen_state
                .debug_info
                .function_infos
                .insert(constant.id as u16, function_info)
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
        DebugInfo,
    ) {
        (
            self.builder_state.instructions,
            self.codegen_state.constant_functions_in_order,
            self.codegen_state.function_infos,
            self.codegen_state.constants_manager.take_data(),
            self.codegen_state.debug_info,
        )
    }

    #[must_use]
    pub fn instructions(&self) -> &[BinaryInstruction] {
        &self.builder_state.instructions
    }

    #[must_use]
    pub const fn ip(&self) -> InstructionPosition {
        InstructionPosition(self.builder_state.instructions.len() as u32)
    }
    #[must_use]
    pub fn meta(&self) -> &[Meta] {
        &self.builder_state.meta
    }
}
