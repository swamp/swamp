use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::{layout_type, layout_variables};
use crate::reg_pool::TempRegisterPool;
use crate::state::{CodeGenState, GenOptions};
use crate::{
    ConstantInfo, FunctionInData, FunctionIp, FunctionIpKind, FunctionIps, GenFunctionInfo,
};
use seq_map::SeqMap;
use source_map_cache::SourceMapWrapper;
use source_map_node::Node;
use swamp_semantic::{
    ConstantId, ConstantRef, InternalFunctionDefinitionRef, InternalFunctionId,
    InternalMainExpression,
};
use swamp_types::Attributes;
use swamp_vm_instr_build::{InstructionBuilder, InstructionBuilderState, PatchPosition};
use swamp_vm_types::types::{
    CompleteFunctionInfo, FunctionInfo, FunctionInfoKind, TypedRegister, VmType, unknown_type,
};
use swamp_vm_types::{
    BinaryInstruction, InstructionPosition, InstructionPositionOffset, InstructionRange, Meta,
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
    }

    pub fn emit_function_def(
        &mut self,
        internal_fn_def: &InternalFunctionDefinitionRef,
        source_map_wrapper: &SourceMapWrapper,
    ) {
        //info!(internal_fn_def.assigned_name, "gen_function");
        assert_ne!(internal_fn_def.program_unique_id, 0);

        let in_data = FunctionInData {
            function_name_node: internal_fn_def.name.0.clone(),
            kind: FunctionInfoKind::Normal(internal_fn_def.program_unique_id as usize),
            assigned_name: internal_fn_def.assigned_name.clone(),
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

    pub fn emit_function_preamble(
        &mut self,
        in_data: &FunctionInData,
        source_map_wrapper: &SourceMapWrapper,
        is_called_by_host: bool,
    ) -> (InstructionPosition, InstructionPosition, FunctionInfo) {
        let start_ip = self.ip();

        eprintln!("function name {}", in_data.assigned_name);

        let frame_and_variable_info = layout_variables(
            &in_data.function_name_node,
            &in_data.parameter_variables,
            &in_data.function_variables,
            &in_data.return_type,
        );

        let frame_size = frame_and_variable_info.frame_memory.size();

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

        instruction_builder.enter(
            function_info.frame_memory.size(),
            &Node::default(),
            "prologue",
        );

        let temp_pool = TempRegisterPool::new(128, 32);

        let mut function_code_builder = CodeBuilder::new(
            &mut self.codegen_state,
            &mut instruction_builder,
            frame_and_variable_info.parameter_and_variable_offsets,
            frame_and_variable_info.frame_registers,
            temp_pool,
            frame_and_variable_info.rest_of_frame_allocator,
            source_map_wrapper,
        );

        let ctx = Context::default();
        //info!(?in_data, "generate");

        let return_basic_type = layout_type(&in_data.return_type);
        let return_register =
            TypedRegister::new_vm_type(0, VmType::new_unknown_placement(return_basic_type));

        function_code_builder.emit_expression_materialize(
            &return_register,
            &in_data.expression,
            &ctx,
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
        InstructionPosition(self.builder_state.instructions.len() as u16)
    }
    #[must_use]
    pub fn meta(&self) -> &[Meta] {
        &self.builder_state.meta
    }
}
