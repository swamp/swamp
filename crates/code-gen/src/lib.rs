/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

pub mod alloc;
pub mod alloc_util;
pub mod constants;
pub mod ctx;
pub mod layout;
mod location;

use crate::alloc::ScopeAllocator;
use crate::alloc_util::reserve_space_for_type;
use crate::constants::ConstantsManager;
use crate::ctx::Context;
use crate::layout::layout_variables;
use crate::layout::{layout_enum_into_tagged_union, layout_tuple_items};
use crate::layout::{layout_struct_type, layout_type};
use seq_map::SeqMap;
use source_map_cache::{FileLineInfo, SourceMapLookup, SourceMapWrapper};
use source_map_node::{FileId, Node};
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{
    AnonymousStructLiteral, BinaryOperator, BinaryOperatorKind, BooleanExpression,
    CompoundOperatorKind, ConstantId, ConstantRef, EnumLiteralData, Expression, ExpressionKind,
    ExternalFunctionDefinitionRef, ForPattern, Function, Guard, InternalFunctionDefinitionRef,
    InternalFunctionId, InternalMainExpression, Iterable, Literal, Match,
    MutRefOrImmutableExpression, NormalPattern, Pattern, Postfix, PostfixKind,
    SingleLocationExpression, StartOfChain, StartOfChainKind, TargetAssignmentLocation,
    UnaryOperator, UnaryOperatorKind, VariableRef, WhenBinding,
};
use swamp_types::{AnonymousStructType, Attribute, Attributes, EnumVariantType, Signature, Type};
use swamp_vm_disasm::{SourceFileLineInfo, disasm_instructions_color};
use swamp_vm_instr_build::{InstructionBuilder, InstructionBuilderState, PatchPosition};
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, FrameMemoryInfo, FramePlacedType, FunctionInfo, FunctionInfoKind,
    HeapPlacedType, heap_ptr_size, int_type, show_frame_memory,
};
use swamp_vm_types::{
    BinaryInstruction, CountU16, FrameMemoryAddress, FrameMemoryRegion, FrameMemorySize,
    GRID_HEADER_ALIGNMENT, GRID_HEADER_SIZE, HeapMemoryAddress, HeapMemoryOffset, HeapMemorySize,
    InstructionPosition, InstructionPositionOffset, InstructionRange, MAP_HEADER_ALIGNMENT,
    MAP_HEADER_COUNT_OFFSET, MAP_HEADER_SIZE, MAP_ITERATOR_ALIGNMENT, MAP_ITERATOR_SIZE,
    MemoryAlignment, MemoryOffset, MemorySize, Meta, RANGE_HEADER_ALIGNMENT, RANGE_HEADER_SIZE,
    RANGE_ITERATOR_ALIGNMENT, RANGE_ITERATOR_SIZE, SLICE_COUNT_OFFSET, SLICE_HEADER_SIZE,
    SLICE_PAIR_HEADER_SIZE, SLICE_PTR_OFFSET, STRING_HEADER_ALIGNMENT, STRING_HEADER_COUNT_OFFSET,
    STRING_HEADER_SIZE, STRING_PTR_SIZE, StringHeader, TempFrameMemoryAddress,
    VEC_HEADER_ALIGNMENT, VEC_HEADER_COUNT_OFFSET, VEC_HEADER_SIZE, VEC_ITERATOR_ALIGNMENT,
    VEC_ITERATOR_SIZE, VEC_PTR_SIZE, ZFlagPolarity,
};
use tracing::{error, info};

#[derive(Copy, Clone)]
pub enum Transformer {
    For,
    Filter,
    Find,
    Map,
    Any,
    All,
    FilterMap,
}

pub enum TransformerResult {
    Unit,
    Bool,
    VecWithLambdaResult,
    VecFromSourceCollection,
    WrappedValueFromSourceCollection,
}

impl Transformer {
    pub(crate) const fn return_type(self) -> TransformerResult {
        match self {
            Self::Filter => TransformerResult::VecFromSourceCollection,
            Self::FilterMap | Self::Map => TransformerResult::VecWithLambdaResult,
            Self::All | Self::Any => TransformerResult::Bool,
            Self::Find => TransformerResult::WrappedValueFromSourceCollection,
            Self::For => TransformerResult::Unit,
        }
    }

    pub(crate) const fn needs_tag_removed(self) -> bool {
        matches!(self, Self::FilterMap)
    }
}

#[derive(Copy, Clone)]
pub enum Collection {
    Vec,
    Map,
    Grid,
    String,
    Range,
}

impl Collection {
    pub fn size_and_alignment(&self) -> (MemorySize, MemoryAlignment) {
        match self {
            Self::Vec => (VEC_HEADER_SIZE, VEC_HEADER_ALIGNMENT),
            Self::Map => (MAP_HEADER_SIZE, MAP_HEADER_ALIGNMENT),
            Self::Grid => (GRID_HEADER_SIZE, GRID_HEADER_ALIGNMENT),
            Self::String => (STRING_HEADER_SIZE, STRING_HEADER_ALIGNMENT),
            Self::Range => (RANGE_HEADER_SIZE, RANGE_HEADER_ALIGNMENT),
        }
    }

    pub fn iterator_size_and_alignment(&self) -> (MemorySize, MemoryAlignment) {
        match self {
            Self::Vec => (VEC_ITERATOR_SIZE, VEC_ITERATOR_ALIGNMENT),
            Self::Map => (MAP_ITERATOR_SIZE, MAP_ITERATOR_ALIGNMENT),
            Self::Grid => todo!(),
            Self::String => todo!(),
            Self::Range => (RANGE_ITERATOR_SIZE, RANGE_ITERATOR_ALIGNMENT),
            _ => todo!(),
        }
    }

    pub fn iterator_gen_type(&self) -> BasicType {
        let kind = match self {
            Self::Vec => BasicTypeKind::InternalVecIterator,
            Self::Map => BasicTypeKind::InternalMapIterator,
            //Self::Grid => BasicTypeKind::InternalGridIterator,
            //Self::String => BasicTypeKind::InternalStringIterator,
            Self::Range => BasicTypeKind::InternalRangeIterator,
            _ => todo!(),
        };
        let (size, alignment) = self.iterator_size_and_alignment();
        BasicType {
            kind,
            total_size: size,
            max_alignment: alignment,
        }
    }
}

#[derive(Clone)]
pub enum FunctionIpKind {
    Normal(InternalFunctionId),
    Constant(ConstantId),
}

#[derive(Clone)]
pub struct FunctionIp {
    ip_range: InstructionRange,
    pub kind: FunctionIpKind,
}

#[derive(PartialEq, Debug)]
pub enum GeneratedExpressionResultKind {
    ZFlagUnmodified,
    ZFlagIsTrue,
    ZFlagIsInversion,
}

pub struct GeneratedExpressionResult {
    pub kind: GeneratedExpressionResultKind,
}

impl GeneratedExpressionResult {
    pub(crate) fn invert_polarity(&self) -> Self {
        Self {
            kind: self.kind.invert_polarity(),
        }
    }
}

impl GeneratedExpressionResultKind {
    pub(crate) fn invert_polarity(&self) -> Self {
        match self {
            Self::ZFlagUnmodified => {
                panic!("can not invert polarity. status is unknown")
            }
            Self::ZFlagIsTrue => Self::ZFlagIsInversion,
            Self::ZFlagIsInversion => Self::ZFlagIsTrue,
        }
    }

    pub(crate) fn polarity(&self) -> ZFlagPolarity {
        match self {
            Self::ZFlagUnmodified => panic!("polarity is undefined"),
            Self::ZFlagIsTrue => ZFlagPolarity::Normal,
            Self::ZFlagIsInversion => ZFlagPolarity::Inverted,
        }
    }
}

impl GeneratedExpressionResult {
    pub(crate) fn polarity(&self) -> ZFlagPolarity {
        self.kind.polarity()
    }
}

impl Default for GeneratedExpressionResult {
    fn default() -> Self {
        Self {
            kind: GeneratedExpressionResultKind::ZFlagUnmodified,
        }
    }
}

pub struct SlicePairInfo {
    pub addr: TempFrameMemoryAddress,
    pub key_size: MemorySize,
    pub value_size: MemorySize,
    pub element_count: CountU16,
    pub element_size: MemorySize,
}

pub struct GenFunctionInfo {
    pub ip_range: InstructionRange,
    pub internal_function_definition: InternalFunctionDefinitionRef,
}

pub struct FunctionFixup {
    pub patch_position: PatchPosition,
    pub fn_id: InternalFunctionId,
    pub internal_function_definition: InternalFunctionDefinitionRef,
}

pub struct ConstantInfo {
    pub ip_range: InstructionRange,
    pub constant_ref: ConstantRef,
    pub target_constant_memory: HeapPlacedType,
}

pub struct FunctionIps {
    ranges: Vec<FunctionIp>,
}

impl Default for FunctionIps {
    fn default() -> Self {
        Self::new()
    }
}

impl FunctionIps {
    #[must_use]
    pub const fn new() -> Self {
        Self { ranges: Vec::new() }
    }

    #[must_use]
    pub fn get_function_end_from_start(&self, ip: InstructionPosition) -> Option<FunctionIp> {
        for range in &self.ranges {
            if range.ip_range.start.0 == ip.0 {
                return Some(range.clone());
            }
        }

        None
    }
}

pub struct CodeGenState {
    constants: ConstantsManager,
    constant_offsets: SeqMap<ConstantId, HeapPlacedType>,
    constant_functions_in_order: SeqMap<ConstantId, ConstantInfo>,
    pub function_infos: SeqMap<InternalFunctionId, GenFunctionInfo>,
    function_fixups: Vec<FunctionFixup>,

    pub function_ips: FunctionIps,
    pub function_debug_infos: SeqMap<InstructionPosition, FunctionInfo>,
}

pub struct GenOptions {
    pub is_halt_function: bool,
}

fn different_file_info(span_a: &FileLineInfo, span_b: &FileLineInfo) -> bool {
    span_a.line != span_b.line
}

pub fn is_valid_file_id(file_id: FileId) -> bool {
    file_id != 0 && file_id != 0xffff
}

pub fn disasm_function(
    frame_relative_infos: &FrameMemoryInfo,
    instructions: &[BinaryInstruction],
    meta: &[Meta],
    ip_offset: InstructionPositionOffset,
    source_map_wrapper: &SourceMapWrapper,
) -> String {
    let mut header_output = String::new();

    show_frame_memory(frame_relative_infos, &mut header_output).unwrap();

    let mut ip_infos = SeqMap::new();

    let mut previous_node: Option<FileLineInfo> = None;

    for (offset, _inst) in instructions.iter().enumerate() {
        let absolute_ip = ip_offset.0 + offset as u16;
        let meta = &meta[offset];
        let file_line_info = if is_valid_file_id(meta.node.span.file_id) {
            Some(source_map_wrapper.get_line(&meta.node.span))
        } else {
            None
        };

        if let Some(line_info) = file_line_info {
            let is_different_line = previous_node
                .as_ref()
                .is_none_or(|previous| different_file_info(&line_info, previous));

            // TODO: Add clone to FileLineInfo
            previous_node = Some(FileLineInfo {
                row: line_info.row,
                col: line_info.col,
                line: line_info.line.clone(),
                relative_file_name: line_info.relative_file_name.clone(),
            });

            if is_different_line {
                assert_ne!(
                    line_info.row, 0,
                    "file_info: {}",
                    line_info.relative_file_name
                );
                assert_ne!(
                    line_info.row, 0,
                    "file_info: {}",
                    line_info.relative_file_name
                );
                let mapped = SourceFileLineInfo {
                    row: line_info.row,
                    file_id: meta.node.span.file_id as usize,
                };
                ip_infos
                    .insert(InstructionPosition(absolute_ip as u16), mapped)
                    .unwrap();
            }
        }
    }

    format!(
        "{}\n{}",
        header_output,
        disasm_instructions_color(
            instructions,
            &ip_offset,
            meta,
            frame_relative_infos,
            &ip_infos,
            source_map_wrapper,
        )
    )
}

pub fn disasm_whole_program(
    function_debug_infos: &SeqMap<InstructionPosition, FunctionInfo>,
    source_map_wrapper: &SourceMapWrapper,
    instructions: &[BinaryInstruction],
    meta: &[Meta],
) {
    let mut current_ip: u16 = 0;

    let instruction_count = instructions.len();
    while current_ip < (instructions.len() - 1) as u16 {
        if let Some(function_debug_info) =
            function_debug_infos.get(&InstructionPosition(current_ip))
        {
            eprintln!(
                "{} ==========================================================================",
                function_debug_info.name
            );
            let end_ip = current_ip + function_debug_info.ip_range.count.0;
            let instructions_slice = &instructions[current_ip as usize..end_ip as usize];
            let meta_slice = &meta[current_ip as usize..end_ip as usize];

            let output_string = disasm_function(
                &function_debug_info.frame_memory,
                instructions_slice,
                meta_slice,
                InstructionPositionOffset(current_ip),
                source_map_wrapper,
            );
            eprintln!("{output_string}");
            current_ip = end_ip;
        } else {
            panic!("instruction pointer that is not covered")
        }
    }
}
impl CodeGenState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            constants: ConstantsManager::default(),
            constant_offsets: SeqMap::default(),
            function_infos: SeqMap::default(),
            constant_functions_in_order: SeqMap::default(),
            function_fixups: vec![],
            function_ips: FunctionIps::default(),
            function_debug_infos: SeqMap::default(),
        }
    }

    #[must_use]
    pub fn constant_functions(&self) -> &SeqMap<ConstantId, ConstantInfo> {
        &self.constant_functions_in_order
    }

    #[must_use]
    pub fn create_function_sections(&self) -> SeqMap<InstructionPosition, String> {
        let mut lookups = SeqMap::new();
        for (_func_id, function_info) in &self.function_infos {
            let description = function_info
                .internal_function_definition
                .assigned_name
                .clone();
            lookups
                .insert(function_info.ip_range.start.clone(), description)
                .unwrap();
        }

        for (_func_id, function_info) in &self.constant_functions_in_order {
            let description = format!("constant {}", function_info.constant_ref.assigned_name);
            lookups
                .insert(function_info.ip_range.start.clone(), description)
                .unwrap();
        }

        lookups
    }
    /*
    #[must_use]
    pub fn builder(&self) -> &InstructionBuilder {
        &self.builder_state
    }

     */

    pub fn reserve_space_for_constants(&mut self, constants: &[ConstantRef]) {
        for constant in constants {
            let heap_placed_type = self.constants.allocator.allocate(&constant.resolved_type);

            self.constant_offsets
                .insert(constant.id, heap_placed_type)
                .unwrap();
        }
    }
}

pub fn reserve(ty: &Type, allocator: &mut ScopeAllocator) -> FramePlacedType {
    allocator.reserve(ty)
}

pub struct FrameAndVariableInfo {
    pub frame_memory: FrameMemoryInfo,
    variable_offsets: SeqMap<usize, FramePlacedType>,
    temp_allocator_region: FrameMemoryRegion,
    return_placement: FramePlacedType,
}

#[derive(Debug)]
pub struct FunctionInData {
    pub function_name_node: Node,
    pub kind: FunctionInfoKind,
    pub assigned_name: String,
    pub all_variables_parameters_first: Vec<VariableRef>,
    pub return_type: Type,
    pub expression: Expression,
}

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
            all_variables_parameters_first: internal_fn_def.parameter_and_variables.clone(),
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
            &main.function_scope_state,
            &main.expression.ty,
        );

        let in_data = FunctionInData {
            function_name_node: main.expression.node.clone(),
            kind: FunctionInfoKind::Normal(main.program_unique_id as usize),
            assigned_name: "main_expr".to_string(),
            all_variables_parameters_first: main.function_scope_state.clone(),
            return_type: main.expression.ty.clone(),
            expression: main.expression.clone(),
        };

        let (start_ip, end_ip, function_info) =
            self.emit_function_preamble(&in_data, source_map_lookup, true);

        let function_info = FunctionInfo {
            kind: FunctionInfoKind::Normal(main.program_unique_id as usize),
            frame_memory: variable_and_frame_memory.frame_memory,
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

        let frame_and_variable_info = layout_variables(
            &in_data.function_name_node,
            &in_data.all_variables_parameters_first,
            &in_data.return_type,
        );

        let frame_size = frame_and_variable_info.frame_memory.size();

        let mut function_info = FunctionInfo {
            kind: in_data.kind.clone(),
            frame_memory: frame_and_variable_info.frame_memory,
            name: in_data.assigned_name.clone(),
            ip_range: InstructionRange {
                start: start_ip.clone(),
                count: InstructionPositionOffset(0),
            },
        };

        let mut func_builder = self.builder_state.add_function(
            function_info.clone(),
            &in_data.function_name_node,
            "function",
        );

        let mut function_generator = FunctionCodeGen::new(
            &mut self.codegen_state,
            &mut func_builder,
            frame_and_variable_info.variable_offsets,
            frame_size,
            frame_and_variable_info.temp_allocator_region,
            &in_data.assigned_name,
            source_map_wrapper,
        );

        /*
        let ExpressionKind::Block(block_expressions) = &in_data.expression.kind else {
            panic!("function body should be a block")
        };

         */

        let ctx = Context::new(frame_and_variable_info.return_placement);
        //info!(?in_data, "generate");
        function_generator.emit_expression_materialize(&in_data.expression, &ctx);

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
                all_variables_parameters_first: constant.function_scope_state.clone(),
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
            self.codegen_state.constants.take_data(),
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

pub struct FunctionCodeGen<'a> {
    state: &'a mut CodeGenState,
    builder: &'a mut InstructionBuilder<'a>, // also references things in CodeGenState
    variable_offsets: SeqMap<usize, FramePlacedType>,
    total_frame_size: FrameMemorySize,
    variable_frame_size: FrameMemorySize,
    temp_allocator: ScopeAllocator,
    argument_allocator: ScopeAllocator,
    debug_name: String,
    source_map_lookup: &'a SourceMapWrapper<'a>,
}

impl<'a> FunctionCodeGen<'a> {
    #[must_use]
    pub fn new(
        state: &'a mut CodeGenState,
        builder: &'a mut InstructionBuilder<'a>,
        variable_offsets: SeqMap<usize, FramePlacedType>,
        frame_size: FrameMemorySize,
        temp_memory_region: FrameMemoryRegion,
        debug_name: &str,
        source_map_lookup: &'a SourceMapWrapper,
    ) -> Self {
        const ARGUMENT_MAX_SIZE: u16 = 2 * 1024;

        Self {
            state,
            variable_offsets,
            total_frame_size: frame_size,
            variable_frame_size: temp_memory_region.addr.as_size(),
            temp_allocator: ScopeAllocator::new(temp_memory_region),
            argument_allocator: ScopeAllocator::new(FrameMemoryRegion::new(
                FrameMemoryAddress(frame_size.0),
                MemorySize(ARGUMENT_MAX_SIZE),
            )),
            builder,
            debug_name: debug_name.to_string(),
            source_map_lookup,
        }
    }
}

impl FunctionCodeGen<'_> {
    #[allow(clippy::too_many_lines)]
    #[allow(clippy::single_match_else)]
    pub fn emit_single_intrinsic_call(
        &mut self,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        match intrinsic_fn {
            IntrinsicFunction::VecFromSlice => {
                let MutRefOrImmutableExpression::Expression(expr) = &arguments[0] else {
                    panic!("problem");
                };

                let slice_region = self.emit_expression_location(expr);

                let slice_type = arguments[0].ty();

                let Type::Slice(element_type) = slice_type else {
                    panic!("problem");
                };

                assert!(element_type.is_concrete());

                self.builder.add_vec_from_slice(
                    ctx.target(),
                    &slice_region,
                    node,
                    "vec_from_slice",
                );
                GeneratedExpressionResult::default()
            }

            IntrinsicFunction::MapFromSlicePair => {
                let MutRefOrImmutableExpression::Expression(expr) = &arguments[0] else {
                    panic!("problem");
                };

                let slice_region = self.emit_expression_location(expr);

                let slice_type = arguments[0].ty();

                let Type::SlicePair(key_type, value_type) = slice_type else {
                    panic!("problem");
                };

                assert!(key_type.is_concrete_or_unit()); // is unit when it is empty
                assert!(value_type.is_concrete_or_unit());

                self.builder.add_map_new_from_slice(
                    ctx.target(),
                    &slice_region,
                    node,
                    "create map from temporary slice pair",
                );

                GeneratedExpressionResult::default()
            }

            _ => {
                let (self_arg, maybe_self_type) = if arguments.is_empty() {
                    (None, None)
                } else {
                    let self_region =
                        self.emit_expression_location_mut_ref_or_immutable(&arguments[0]);
                    (Some(self_region), Some(arguments[0].ty().clone()))
                };
                let rest_args = if arguments.len() > 1 {
                    &arguments[1..]
                } else {
                    &vec![]
                };
                self.emit_single_intrinsic_call_with_self(
                    node,
                    intrinsic_fn,
                    maybe_self_type,
                    self_arg,
                    rest_args,
                    ctx,
                )
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn emit_single_intrinsic_call_with_self(
        &mut self,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        self_type: Option<Type>,
        self_addr: Option<FramePlacedType>,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let mut z_flag_result = GeneratedExpressionResult::default();
        match intrinsic_fn {
            IntrinsicFunction::RuntimePanic => {
                self.builder
                    .add_panic(&self_addr.unwrap(), node, "intrinsic panic");
            }

            // Bool
            IntrinsicFunction::BoolToString => self.builder.bool_to_string(
                ctx.target(),
                &self_addr.unwrap(),
                node,
                "bool_to_string",
            ),

            // Fixed
            IntrinsicFunction::FloatRound => {
                self.builder
                    .add_float_round(ctx.target(), &self_addr.unwrap(), node, "float round")
            }
            IntrinsicFunction::FloatFloor => {
                self.builder
                    .add_float_floor(ctx.target(), &self_addr.unwrap(), node, "float floor")
            }
            IntrinsicFunction::FloatSqrt => {
                self.builder
                    .add_float_sqrt(ctx.target(), &self_addr.unwrap(), node, "float sqr");
            }
            IntrinsicFunction::FloatSign => {
                self.builder
                    .add_float_sign(ctx.target(), &self_addr.unwrap(), node, "float sign");
            }
            IntrinsicFunction::FloatAbs => {
                self.builder
                    .add_float_abs(ctx.target(), &self_addr.unwrap(), node, "float abs");
            }
            IntrinsicFunction::FloatRnd => {
                self.builder.add_float_prnd(
                    ctx.target(),
                    &self_addr.unwrap(),
                    node,
                    "float pseudo random",
                );
            }
            IntrinsicFunction::FloatCos => {
                self.builder
                    .add_float_cos(ctx.target(), &self_addr.unwrap(), node, "float cos");
            }
            IntrinsicFunction::FloatSin => {
                self.builder
                    .add_float_sin(ctx.target(), &self_addr.unwrap(), node, "float sin");
            }
            IntrinsicFunction::FloatAcos => {
                self.builder
                    .add_float_acos(ctx.target(), &self_addr.unwrap(), node, "float acos")
            }
            IntrinsicFunction::FloatAsin => {
                self.builder
                    .add_float_asin(ctx.target(), &self_addr.unwrap(), node, "float asin")
            }
            IntrinsicFunction::FloatAtan2 => {
                self.builder
                    .add_float_atan2(ctx.target(), &self_addr.unwrap(), node, "float atan2")
            }
            IntrinsicFunction::FloatMin => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.emit_expression_location(float_arg_expr);
                self.builder.add_float_min(
                    ctx.target(),
                    &self_addr.unwrap(),
                    &float_region,
                    node,
                    "float min",
                );
            }
            IntrinsicFunction::FloatMax => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.emit_expression_location(float_arg_expr);
                self.builder.add_float_max(
                    ctx.target(),
                    &self_addr.unwrap(),
                    &float_region,
                    node,
                    "float max",
                );
            }
            IntrinsicFunction::FloatClamp => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.emit_expression_location(float_arg_expr);

                let float_b = &arguments[1];
                let MutRefOrImmutableExpression::Expression(float_b_expr) = float_b else {
                    panic!();
                };
                let float_b_region = self.emit_expression_location(float_b_expr);

                self.builder.add_float_clamp(
                    ctx.target(),
                    &float_region,
                    &self_addr.unwrap(),
                    &float_b_region,
                    node,
                    "float round",
                );
            }
            IntrinsicFunction::FloatToString => self.builder.float_to_string(
                ctx.target(),
                &self_addr.unwrap(),
                node,
                "float_to_string",
            ),

            // Int
            IntrinsicFunction::IntAbs => {
                self.builder
                    .add_int_abs(ctx.target(), &self_addr.unwrap(), node, "int abs");
            }

            IntrinsicFunction::IntRnd => {
                self.builder.add_int_rnd(
                    ctx.target(),
                    &self_addr.unwrap(),
                    node,
                    "int pseudo random",
                );
            }
            IntrinsicFunction::IntMax => {
                self.builder
                    .add_int_max(ctx.target(), &self_addr.unwrap(), node, "int max");
            }
            IntrinsicFunction::IntMin => {
                self.builder
                    .add_int_min(ctx.target(), &self_addr.unwrap(), node, "int min");
            }
            IntrinsicFunction::IntClamp => {
                self.builder
                    .add_int_clamp(ctx.target(), &self_addr.unwrap(), node, "int clamp");
            }
            IntrinsicFunction::IntToFloat => self.builder.add_int_to_float(
                ctx.target(),
                &self_addr.unwrap(),
                node,
                "int to float",
            ),
            IntrinsicFunction::IntToString => self.builder.add_int_to_string(
                ctx.target(),
                &self_addr.unwrap(),
                node,
                "int_to_string",
            ),

            // String
            IntrinsicFunction::StringLen => {
                self.builder.add_mov32(
                    ctx.target(),
                    &self_addr
                        .unwrap()
                        .move_with_offset(STRING_HEADER_COUNT_OFFSET, int_type()),
                    node,
                    "get the length",
                );
            }

            // Vec
            IntrinsicFunction::VecFromSlice => {
                panic!("no self in vec from slice")
            }
            IntrinsicFunction::MapFromSlicePair => {
                panic!("no self in mac from slice")
            }
            IntrinsicFunction::VecPush => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.emit_expression_location(key_expr);
                self.builder.add_vec_push(
                    &self_addr.unwrap(), // mut self
                    &key_region,
                    node,
                    "vec push",
                );
            }
            IntrinsicFunction::VecPop => {
                self.builder.add_vec_pop(
                    ctx.target(),
                    &self_addr.unwrap(), // mut self
                    node,
                    "vec pop",
                );
            }
            IntrinsicFunction::VecRemoveIndex => {
                let maybe_index_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.emit_expression_location(index_expr);
                self.builder.add_vec_remove_index(
                    &self_addr.unwrap(),
                    &index_region,
                    node,
                    "remove index",
                );
            }
            IntrinsicFunction::VecRemoveIndexGetValue => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.emit_expression_location(key_expr);
                self.builder.add_vec_remove_index_get_value(
                    ctx.target(),
                    &self_addr.unwrap(), // mut self
                    &key_region,
                    node,
                    "vec remove index get value",
                );
            }
            IntrinsicFunction::VecClear => {
                self.builder.add_vec_clear(
                    &self_addr.unwrap(), // mut self
                    node,
                    "vec clear",
                );
            }
            IntrinsicFunction::VecGet => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.emit_expression_location(key_expr);
                self.builder.add_vec_get(
                    ctx.target(),
                    &self_addr.unwrap(), // mut self
                    &key_region,
                    node,
                    "vec get",
                );
            }
            IntrinsicFunction::VecCreate => {
                self.builder
                    .add_vec_create(ctx.target(), MemorySize(0), node, "vec create"); // TODO: Fix to have proper element memory size
            }
            IntrinsicFunction::VecSubscript => {
                let maybe_index_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.emit_expression_location(index_expr);
                self.builder.add_vec_subscript(
                    ctx.target(),
                    &self_addr.unwrap(),
                    &index_region,
                    node,
                    "vec get element at index",
                );
            }
            IntrinsicFunction::VecSubscriptMut => {
                let maybe_index_argument = &arguments[0];

                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.emit_expression_location(index_expr);
                // TODO:

                /*
                let source_argument = &arguments[1];
                let MutRefOrImmutableExpression::Expression(value_expr) = source_argument else {
                    panic!();
                };

                let value_region = self.emit_expression_for_access(value_expr)?;
                 */
                let value_region = &index_region;

                self.builder.add_vec_set(
                    &self_addr.unwrap(),
                    &index_region,
                    value_region,
                    node,
                    "set the vec subscript",
                );
            }
            IntrinsicFunction::VecSubscriptRange => {
                let maybe_range_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(range_expr) = maybe_range_argument
                else {
                    panic!();
                };
                let range_header_region = self.emit_expression_location(range_expr);
                assert_eq!(range_header_region.size(), RANGE_HEADER_SIZE);
                self.builder.add_vec_get_range(
                    ctx.target(),
                    &self_addr.unwrap(),  // mut self (string header)
                    &range_header_region, // range x..=y
                    node,
                    "vec subscript range",
                );
            }
            IntrinsicFunction::VecIter => {
                // TODO:
                // Intentionally empty, since it should never be called
            }
            IntrinsicFunction::VecIterMut => {
                // TODO:
                // Intentionally empty, since it should never be called
            }
            IntrinsicFunction::VecFor => todo!(),   // Low prio
            IntrinsicFunction::VecWhile => todo!(), // Low prio
            IntrinsicFunction::VecFindMap => todo!(), // Low prio

            IntrinsicFunction::VecLen => self.builder.add_vec_len(
                ctx.target(),
                &self_addr.unwrap(),
                node,
                "get the vec length",
            ),
            IntrinsicFunction::VecAny => todo!(), // Low prio
            IntrinsicFunction::VecAll => todo!(), // Low prio
            IntrinsicFunction::VecMap => todo!(), // Low prio
            IntrinsicFunction::VecFilterMap => todo!(), // Low prio
            IntrinsicFunction::VecSwap => {
                let index_a = self.emit_for_access_or_location(&arguments[0]);
                let index_b = self.emit_for_access_or_location(&arguments[1]);
                self.builder.add_vec_swap(
                    &self_addr.unwrap(),
                    &index_a,
                    &index_b,
                    node,
                    "vec swap",
                );
            }

            IntrinsicFunction::VecInsert => { // Low prio
            }
            IntrinsicFunction::VecFirst => { // Low prio
            }
            IntrinsicFunction::VecLast => { // Low prio
            }
            IntrinsicFunction::VecFold => { // Low prio
            }
            IntrinsicFunction::VecFilter => {
                self.iterate_over_collection_with_lambda(
                    node,
                    Collection::Vec,
                    Transformer::Filter,
                    &self_addr.unwrap(),
                    &self_type.unwrap(),
                    &arguments[0],
                    ctx,
                );
            }

            IntrinsicFunction::VecFind => {
                self.iterate_over_collection_with_lambda(
                    node,
                    Collection::Vec,
                    Transformer::Find,
                    &self_addr.unwrap(),
                    &self_type.unwrap(),
                    &arguments[0],
                    ctx,
                );
            }

            // Map
            IntrinsicFunction::MapCreate => {
                // TODO:
            }
            IntrinsicFunction::MapHas => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.emit_expression_location(key_argument);
                self.builder
                    .add_map_has(&self_addr.unwrap(), &key, node, "map_has");
                z_flag_result.kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            IntrinsicFunction::MapRemove => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                self.emit_intrinsic_map_remove(&self_addr.unwrap(), key_argument);
            }
            IntrinsicFunction::MapIter => {
                // Never called directly
            }
            IntrinsicFunction::MapIterMut => {
                // Never called directly
            }
            IntrinsicFunction::MapLen => {
                self.builder
                    .add_map_len(ctx.target(), &self_addr.unwrap(), node, "map len");
            }
            IntrinsicFunction::MapSubscript => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.emit_expression_location(key_argument);
                self.builder.add_map_fetch(
                    ctx.target(),
                    &self_addr.unwrap(),
                    &key,
                    node,
                    "map_subscript",
                );
            }
            IntrinsicFunction::MapSubscriptMut => {}
            IntrinsicFunction::MapSubscriptMutCreateIfNeeded => {}

            // Grid
            IntrinsicFunction::GridCreate => {
                /*
                self.state.builder.add_sparse_create(
                    ctx.target(),
                    "map_subscript_mut_create (set)",
                );

                 */
            }

            IntrinsicFunction::GridSet => {
                /*
                let MutRefOrImmutableExpression::Expression(x_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let x_region = self.emit_expression_for_access(x_argument)?;
                let MutRefOrImmutableExpression::Expression(y_argument) = &arguments[1] else {
                    panic!("must be expression for key");
                };
                let y_region = self.emit_expression_for_access(y_argument)?;

                let MutRefOrImmutableExpression::Expression(value) = &arguments[2] else {
                    panic!("must be expression for key");
                };
                let value_region = self.emit_expression_for_access(value)?;

                self.state.builder.add_grid_set(
                    ctx.target(),
                    &self_addr.unwrap(),
                    x_region.addr(),
                    y_region.addr(),
                    value_region.addr(),
                    "grid_get",
                );

                 */
            }
            IntrinsicFunction::GridGet => {
                /*
                let MutRefOrImmutableExpression::Expression(x_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let x_region = self.emit_expression_for_access(x_argument)?;
                let MutRefOrImmutableExpression::Expression(y_argument) = &arguments[1] else {
                    panic!("must be expression for key");
                };
                let y_region = self.emit_expression_for_access(y_argument)?;
                self.state.builder.add_grid_get(
                    ctx.target(),
                    &self_addr.unwrap(),
                    x_region.addr(),
                    y_region.addr(),
                    "grid_get",
                );

                 */
            }

            IntrinsicFunction::GridGetColumn => {}
            IntrinsicFunction::GridFromSlice => {}

            // Map2
            IntrinsicFunction::Map2Remove => {}
            IntrinsicFunction::Map2Insert => {}
            IntrinsicFunction::Map2GetColumn => {}
            IntrinsicFunction::Map2GetRow => {}
            IntrinsicFunction::Map2Get => {}
            IntrinsicFunction::Map2Has => {}
            IntrinsicFunction::Map2Create => {}

            // Low prio ========
            // Sparse
            IntrinsicFunction::SparseCreate => {
                /*
                self.state.builder.add_sparse_create(
                    ctx.target(),
                    "map_subscript_mut_create (set)",
                );

                 */
            }
            IntrinsicFunction::SparseAdd => {}
            IntrinsicFunction::SparseFromSlice => {}
            IntrinsicFunction::SparseIter => {}
            IntrinsicFunction::SparseIterMut => {}
            IntrinsicFunction::SparseSubscript => {}
            IntrinsicFunction::SparseSubscriptMut => {}
            IntrinsicFunction::SparseHas => {}
            IntrinsicFunction::SparseRemove => {}

            // Other
            IntrinsicFunction::Float2Magnitude => {}
        }

        z_flag_result
    }

    fn emit_intrinsic_map_remove(&mut self, map_region: &FramePlacedType, key_expr: &Expression) {
        let key_region = self.emit_expression_location(key_expr);

        self.builder
            .add_map_remove(map_region, &key_region, &key_expr.node, "");
    }
    pub(crate) fn add_call(
        &mut self,
        node: &Node,
        internal_fn: &InternalFunctionDefinitionRef,
        comment: &str,
    ) {
        let function_name = internal_fn.associated_with_type.as_ref().map_or_else(
            || {
                format!(
                    "{:?}::{}",
                    internal_fn.defined_in_module_path, internal_fn.assigned_name
                )
            },
            |associated_with_type| {
                format!(
                    "{:?}::{}:{}",
                    internal_fn.defined_in_module_path,
                    associated_with_type,
                    internal_fn.assigned_name
                )
            },
        );
        let call_comment = &format!("calling {function_name} ({comment})",);

        if let Some(found) = self
            .state
            .function_infos
            .get(&internal_fn.program_unique_id)
        {
            self.builder.add_call(
                &InstructionPosition(found.ip_range.start.0.saturating_sub(1)),
                node,
                call_comment,
            );
        } else {
            let patch_position = self.builder.add_call_placeholder(node, call_comment);
            self.state.function_fixups.push(FunctionFixup {
                patch_position,
                fn_id: internal_fn.program_unique_id,
                internal_function_definition: internal_fn.clone(),
            });
        }
    }

    pub fn temp_memory_region_for_type(&mut self, ty: &Type, comment: &str) -> FramePlacedType {
        reserve_space_for_type(ty, &mut self.temp_allocator)
    }

    pub fn temp_space_for_type(&mut self, ty: &Type, comment: &str) -> Context {
        Context::new(self.temp_memory_region_for_type(ty, comment))
    }

    /// # Panics
    ///
    #[allow(clippy::single_match_else)]
    pub fn emit_expression_location(&mut self, expr: &Expression) -> FramePlacedType {
        self.emit_expression_location_internal(expr)
    }

    pub fn emit_expression_location_leave_z_flag(
        &mut self,
        expr: &Expression,
    ) -> (FramePlacedType, GeneratedExpressionResult) {
        match &expr.kind {
            ExpressionKind::VariableAccess(var_ref) => {
                let frame_address = self
                    .variable_offsets
                    .get(&var_ref.unique_id_within_function)
                    .unwrap();

                return (frame_address.clone(), GeneratedExpressionResult::default());
            }

            ExpressionKind::PostfixChain(start_of_chain, chain) => {
                return self.emit_postfix_chain(start_of_chain, chain, None);
            }

            _ => {}
        }

        let temp_ctx = self.temp_space_for_type(&expr.ty, "expression");

        let z_flag_state = self.emit_expression(expr, &temp_ctx);

        (temp_ctx.target().clone(), z_flag_state)
    }

    /// # Panics
    ///
    pub fn emit_expression_location_internal(&mut self, expr: &Expression) -> FramePlacedType {
        let (target_region, z_flag_state) = self.emit_expression_location_leave_z_flag(expr);

        self.materialize_z_flag_to_bool_if_needed(&target_region, z_flag_state, &expr.node);

        target_region
    }

    fn debug_node(&self, node: &Node) {
        let line_info = self.source_map_lookup.get_line(&node.span);
        let span_text = self.source_map_lookup.get_text_span(&node.span);
        eprintln!(
            "{}:{}:{}> {}",
            line_info.relative_file_name, line_info.row, line_info.col, span_text,
        );
        //info!(?source_code_line, "generating");
    }

    fn debug_instructions(&mut self) {
        /*
        let end_ip = self.state.builder.instructions.len() - 1;
        let instructions_to_disasm =
            &self.state.builder.instructions[self.state.debug_last_ip..=end_ip];
        let mut descriptions = Vec::new();
        for x in instructions_to_disasm {
            descriptions.push(String::new());
        }
        let output = disasm_instructions_color(
            instructions_to_disasm,
            &InstructionPosition(self.state.debug_last_ip as u16),
            &descriptions,
            &SeqMap::default(),
        );
        eprintln!("{output}");
        self.state.debug_last_ip = end_ip + 1;

         */
    }

    pub fn emit_expression_materialize(&mut self, expr: &Expression, ctx: &Context) {
        let result = self.emit_expression(expr, ctx);

        self.materialize_z_flag_to_bool_if_needed(ctx.target(), result, &expr.node);
    }

    pub fn emit_expression(
        &mut self,
        expr: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        //self.debug_node(&expr.node);

        match &expr.kind {
            ExpressionKind::ConstantAccess(constant_ref) => {
                self.emit_constant_access(&expr.node, constant_ref, ctx)
            }
            ExpressionKind::TupleDestructuring(variables, tuple_types, tuple_expression) => {
                self.emit_tuple_destructuring(variables, tuple_types, tuple_expression)
            }
            ExpressionKind::Assignment(target_mut_location_expr, source_expr) => {
                self.emit_assignment(&expr.node, target_mut_location_expr, source_expr)
            }
            ExpressionKind::VariableAccess(variable_ref) => {
                self.emit_variable_access(&expr.node, variable_ref, ctx)
            }
            ExpressionKind::BinaryOp(operator) => self.emit_binary_operator(operator, ctx),
            ExpressionKind::UnaryOp(operator) => self.emit_unary_operator(operator, ctx),
            ExpressionKind::PostfixChain(start, chain) => {
                let (chain_placed_type, expression_result) =
                    self.emit_postfix_chain(start, chain, Some(ctx));
                if chain_placed_type.addr() != ctx.target().addr()
                    && chain_placed_type.size().0 != 0
                {
                    self.builder.add_mov_for_assignment(
                        ctx.target(),
                        &chain_placed_type,
                        &expr.node,
                        "forced mov after chain",
                    );
                }
                expression_result
            }
            ExpressionKind::VariableDefinition(variable, expression) => {
                self.emit_variable_definition(variable, expression, ctx)
            }
            ExpressionKind::VariableReassignment(variable, expression) => {
                self.emit_variable_reassignment(variable, expression, ctx)
            }
            ExpressionKind::AnonymousStructLiteral(anon_struct) => {
                self.emit_anonymous_struct_literal(anon_struct, &expr.ty, ctx)
            }
            ExpressionKind::Literal(basic_literal) => {
                self.emit_literal(&expr.node, basic_literal, ctx)
            }
            ExpressionKind::Option(maybe_option) => {
                self.emit_option_expression(&expr.node, maybe_option.as_deref(), ctx)
            }
            ExpressionKind::ForLoop(for_pattern, collection, lambda_expr) => {
                self.emit_for_loop(&expr.node, for_pattern, collection, lambda_expr, ctx)
            }
            ExpressionKind::WhileLoop(condition, expression) => {
                self.emit_while_loop(condition, expression, ctx)
            }
            ExpressionKind::Block(expressions) => self.emit_block(expressions, ctx),
            ExpressionKind::Match(match_expr) => self.emit_match(match_expr, ctx),
            ExpressionKind::Guard(guards) => self.emit_guard(guards, ctx),
            ExpressionKind::If(conditional, true_expr, false_expr) => {
                self.emit_if(conditional, true_expr, false_expr.as_deref(), ctx)
            }
            ExpressionKind::When(bindings, true_expr, false_expr) => {
                self.emit_when(bindings, true_expr, false_expr.as_deref(), ctx)
            }
            ExpressionKind::CompoundAssignment(target_location, operator_kind, source_expr) => {
                self.compound_assignment(target_location, operator_kind, source_expr)
            }
            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                self.emit_single_intrinsic_call(&expr.node, intrinsic_fn, arguments, ctx)
            }
            ExpressionKind::CoerceOptionToBool(a) => self.emit_coerce_option_to_bool(a, ctx),

            ExpressionKind::InternalCall(internal, arguments) => {
                self.emit_internal_call(&expr.node, internal, arguments, ctx)
            }
            ExpressionKind::HostCall(host_fn, arguments) => {
                self.emit_host_call(&expr.node, host_fn, arguments, ctx)
            }

            // Low priority
            ExpressionKind::VariableBinding(_, _) => todo!(), // only used for `when` expressions

            // Illegal
            ExpressionKind::Lambda(_vec, _x) => {
                panic!("something went wrong. non-capturing lambdas can not be evaluated")
            }
        }
    }

    fn emit_unary_operator(
        &mut self,
        unary_operator: &UnaryOperator,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let node = &unary_operator.node;
        let result = match &unary_operator.kind {
            UnaryOperatorKind::Not => match &unary_operator.left.ty {
                Type::Bool => {
                    let bool_result = self.emit_expression_to_z_flag(&unary_operator.left);
                    bool_result.invert_polarity()
                }
                _ => panic!("unknown not"),
            },
            UnaryOperatorKind::Negate => match &unary_operator.left.ty {
                Type::Int => {
                    let left_source = self.emit_expression_location(&unary_operator.left);
                    self.builder
                        .add_neg_i32(ctx.target(), &left_source, node, "negate i32");
                    GeneratedExpressionResult::default()
                }

                Type::Float => {
                    let left_source = self.emit_expression_location(&unary_operator.left);
                    self.builder
                        .add_neg_f32(ctx.target(), &left_source, node, "negate f32");
                    GeneratedExpressionResult::default()
                }
                _ => panic!("negate should only be possible on Int and Float"),
            },
        };

        result
    }

    fn emit_binary_operator(
        &mut self,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        //info!(left=?binary_operator.left.ty, right=?binary_operator.right.ty, "binary_op");

        match &binary_operator.kind {
            BinaryOperatorKind::LogicalOr | BinaryOperatorKind::LogicalAnd => {
                self.emit_binary_operator_logical(binary_operator)
            }
            _ => self.emit_binary_operator_normal(binary_operator, ctx),
        }
    }

    fn emit_binary_operator_normal(
        &mut self,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let left_source = self.emit_expression_location(&binary_operator.left);
        let right_source = self.emit_expression_location(&binary_operator.right);

        match &binary_operator.kind {
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                match (&binary_operator.left.ty, &binary_operator.right.ty) {
                    (Type::Bool, Type::Bool) => self.emit_binary_operator_cmp8(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    (Type::Int, Type::Int) => self.emit_binary_operator_cmp32(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    (Type::Float, Type::Float) => self.emit_binary_operator_cmp32(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    (Type::String, Type::String) => self.emit_binary_operator_string_cmp(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    (Type::Enum(a), Type::Enum(b)) => self.emit_binary_operator_bytes_cmp(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    _ => todo!(),
                }
            }
            _ => match (&binary_operator.left.ty, &binary_operator.right.ty) {
                //(Type::Bool, Type::Bool) => self.emit_binary_operator_logical(binary_operator),
                (Type::Int, Type::Int) => self.emit_binary_operator_i32(
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                (Type::Float, Type::Float) => self.emit_binary_operator_f32(
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                (Type::String, Type::String) => self.emit_binary_operator_string(
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                _ => todo!(),
            },
        }
    }

    fn emit_binary_operator_i32(
        &mut self,
        left_source: &FramePlacedType,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: &FramePlacedType,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let mut kind = GeneratedExpressionResultKind::ZFlagUnmodified;
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder
                    .add_add_i32(ctx.target(), left_source, right_source, node, "i32 add");
            }
            BinaryOperatorKind::Subtract => {
                self.builder
                    .add_sub_i32(ctx.target(), left_source, right_source, node, "i32 sub")
            }
            BinaryOperatorKind::Multiply => {
                self.builder
                    .add_mul_i32(ctx.target(), left_source, right_source, node, "i32 mul");
            }
            BinaryOperatorKind::Divide => {
                self.builder
                    .add_div_i32(ctx.target(), left_source, right_source, node, "i32 div")
            }
            BinaryOperatorKind::Modulo => {
                self.builder
                    .add_mod_i32(ctx.target(), left_source, right_source, node, "i32 mod")
            }
            BinaryOperatorKind::LogicalOr => todo!(),
            BinaryOperatorKind::LogicalAnd => todo!(),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => todo!(),
            /*
            {
                self.builder
                    .add_cmp32(left_source, right_source, node, "i32 cmp");
                if let BinaryOperatorKind::Equal = binary_operator_kind {
                    kind = GeneratedExpressionResultKind::ZFlagIsTrue;
                } else {
                    kind = GeneratedExpressionResultKind::ZFlagIsInversion;
                }
            }
             */
            BinaryOperatorKind::LessThan => {
                self.builder
                    .add_lt_i32(left_source, right_source, node, "i32 lt");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_i32(left_source, right_source, node, "i32 le");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_i32(left_source, right_source, node, "i32 gt");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_i32(left_source, right_source, node, "i32 ge");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
        }

        GeneratedExpressionResult { kind }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn emit_binary_operator_f32(
        &mut self,
        left_source: &FramePlacedType,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: &FramePlacedType,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let mut kind = GeneratedExpressionResultKind::ZFlagUnmodified;
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder
                    .add_add_f32(ctx.target(), left_source, right_source, node, "f32 add");
            }

            BinaryOperatorKind::Subtract => {
                self.builder
                    .add_sub_f32(ctx.target(), left_source, right_source, node, "f32 sub")
            }
            BinaryOperatorKind::Multiply => {
                self.builder
                    .add_mul_f32(ctx.target(), left_source, right_source, node, "f32 add");
            }
            BinaryOperatorKind::Divide => {
                self.builder
                    .add_div_f32(ctx.target(), left_source, right_source, node, "f32 div");
            }
            BinaryOperatorKind::Modulo => {
                self.builder
                    .add_mod_f32(ctx.target(), left_source, right_source, node, "f32 mod")
            }
            BinaryOperatorKind::LogicalOr => panic!("not supported"),
            BinaryOperatorKind::LogicalAnd => panic!("not supported"),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => panic!("handled elsewhere"),
            /*{
                self.builder
                    .add_cmp32(left_source, right_source, node, "f32 eq");
                if let BinaryOperatorKind::Equal = binary_operator_kind {
                    kind = GeneratedExpressionResultKind::ZFlagIsTrue;
                } else {
                    kind = GeneratedExpressionResultKind::ZFlagIsInversion;
                }
            }*/
            BinaryOperatorKind::LessThan => {
                self.builder
                    .add_lt_f32(left_source, right_source, node, "f32 lt");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_f32(left_source, right_source, node, "f32 le");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_f32(left_source, right_source, node, "f32 gt");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_f32(left_source, right_source, node, "f32 ge");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
        }

        GeneratedExpressionResult { kind }
    }

    fn emit_binary_operator_string(
        &mut self,
        left_source: &FramePlacedType,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: &FramePlacedType,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder.add_string_append(
                    ctx.target(),
                    left_source,
                    right_source,
                    node,
                    "string add",
                );
            }

            BinaryOperatorKind::Equal => todo!(),
            BinaryOperatorKind::NotEqual => todo!(),
            _ => panic!("illegal string operator"),
        }

        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagUnmodified,
        }
    }

    fn emit_binary_operator_bytes_cmp(
        &mut self,
        left_source: &FramePlacedType,
        node: &Node,
        right_source: &FramePlacedType,
    ) -> GeneratedExpressionResult {
        self.builder.add_cmp(
            left_source,
            right_source,
            //            left_source.size(),
            &node,
            "compare enum",
        );

        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagIsTrue,
        }
    }

    fn emit_binary_operator_cmp8(
        &mut self,
        left_source: &FramePlacedType,
        node: &Node,
        right_source: &FramePlacedType,
    ) -> GeneratedExpressionResult {
        assert_eq!(left_source.size().0, 1);
        assert_eq!(right_source.size().0, 1);
        self.builder
            .add_cmp8(left_source, right_source, &node, "compare bool");

        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagIsTrue,
        }
    }

    fn emit_binary_operator_cmp32(
        &mut self,
        left_source: &FramePlacedType,
        node: &Node,
        right_source: &FramePlacedType,
    ) -> GeneratedExpressionResult {
        self.builder
            .add_cmp32(left_source, right_source, &node, "compare to z flag");

        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagIsTrue,
        }
    }

    fn emit_binary_operator_string_cmp(
        &mut self,
        left_source: &FramePlacedType,
        node: &Node,
        right_source: &FramePlacedType,
    ) -> GeneratedExpressionResult {
        self.builder
            .add_cmp8(left_source, right_source, &node, "compare bool");

        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagIsTrue,
        }
    }

    fn emit_binary_operator_logical(
        &mut self,
        binary_operator: &BinaryOperator,
    ) -> GeneratedExpressionResult {
        let node = &binary_operator.node;

        // the logical is always normalized
        let kind = GeneratedExpressionResultKind::ZFlagIsTrue;

        match binary_operator.kind {
            BinaryOperatorKind::LogicalOr => {
                self.emit_expression_to_normalized_z_flag(&binary_operator.left);

                let jump_after_patch = self
                    .builder
                    .add_jmp_if_equal_placeholder(node, "skip rhs `or` expression");

                self.emit_expression_to_normalized_z_flag(&binary_operator.right);

                self.builder.patch_jump_here(jump_after_patch);
            }
            BinaryOperatorKind::LogicalAnd => {
                self.emit_expression_to_normalized_z_flag(&binary_operator.left);

                let jump_after_patch = self
                    .builder
                    .add_jmp_if_not_equal_placeholder(node, "skip rhs `and` expression");

                self.emit_expression_to_normalized_z_flag(&binary_operator.right);

                self.builder.patch_jump_here(jump_after_patch);
            }

            _ => {
                panic!("unknown operator {binary_operator:?}");
            }
        }

        GeneratedExpressionResult { kind }
    }

    fn emit_condition_context(&mut self, condition: &BooleanExpression) -> PatchPosition {
        let result = self.emit_expression_to_z_flag(&condition.expression);

        let jump_on_false_condition = self.builder.add_jmp_if_not_equal_polarity_placeholder(
            &result.polarity(),
            &condition.expression.node,
            "jump boolean condition false",
        );

        jump_on_false_condition
    }

    fn emit_expression_to_z_flag(&mut self, condition: &Expression) -> GeneratedExpressionResult {
        match &condition.kind {
            ExpressionKind::CoerceOptionToBool(option_union_expr) => {
                let region = self.emit_expression_location(option_union_expr);
                // We can shortcut this, since we know that the tag location is basically a bool value
                self.builder.add_tst8(
                    &region,
                    &option_union_expr.node,
                    "shortcut directly to z-flag",
                );
                return GeneratedExpressionResult {
                    kind: GeneratedExpressionResultKind::ZFlagIsTrue,
                };
            }
            /*
            ExpressionKind::ConstantAccess(_) => {}
            ExpressionKind::VariableAccess(_) => {}
            ExpressionKind::BinaryOp(_) => {}
            ExpressionKind::UnaryOp(_) => {}
            ExpressionKind::PostfixChain(_, _) => {}
            ExpressionKind::IntrinsicCallEx(_, _) => {}
            ExpressionKind::InternalCall(_, _) => {}
            ExpressionKind::HostCall(_, _) => {}
            ExpressionKind::VariableDefinition(_, _) => {}
            ExpressionKind::VariableReassignment(_, _) => {}
            ExpressionKind::VariableBinding(_, _) => {}
            ExpressionKind::Assignment(_, _) => {}
            ExpressionKind::CompoundAssignment(_, _, _) => {}
            ExpressionKind::AnonymousStructLiteral(_) => {}
            ExpressionKind::Literal(_) => {}
            ExpressionKind::Option(_) => {}
            ExpressionKind::ForLoop(_, _, _) => {}
            ExpressionKind::WhileLoop(_, _) => {}
            ExpressionKind::Block(_) => {}
            ExpressionKind::Match(_) => {}
            ExpressionKind::Guard(_) => {}
            ExpressionKind::If(_, _, _) => {}
            ExpressionKind::When(_, _, _) => {}
            ExpressionKind::TupleDestructuring(_, _, _) => {}
            ExpressionKind::Lambda(_, _) => {}

             */
            _ => {}
        }

        let (frame_memory_region, mut gen_result) =
            self.emit_expression_location_leave_z_flag(condition);

        if gen_result.kind == GeneratedExpressionResultKind::ZFlagUnmodified {
            self.builder.add_tst8(
                &frame_memory_region,
                &condition.node,
                "convert to boolean expression (update z flag)",
            );
            gen_result.kind = GeneratedExpressionResultKind::ZFlagIsTrue;
        }

        gen_result
    }

    fn emit_expression_to_normalized_z_flag(&mut self, condition: &Expression) {
        let result = self.emit_expression_to_z_flag(condition);
        assert_ne!(result.kind, GeneratedExpressionResultKind::ZFlagUnmodified);

        if result.kind == GeneratedExpressionResultKind::ZFlagIsInversion {
            self.builder
                .add_not_z(&condition.node, "normalized z is required");
        }
    }

    fn emit_boolean_expression_z_flag(
        &mut self,
        condition: &BooleanExpression,
    ) -> GeneratedExpressionResult {
        self.emit_expression_to_z_flag(&condition.expression)
    }

    fn emit_if(
        &mut self,
        condition: &BooleanExpression,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let jump_on_false_condition = self.emit_condition_context(condition);

        // True expression just takes over our target
        // Both to reuse the current target, and for the fact when there is no else
        self.emit_expression_materialize(true_expr, ctx);

        if let Some(false_expr) = maybe_false_expr {
            // we need to help the true expression to jump over false
            let skip_false_if_true = self.builder.add_jump_placeholder(
                &condition.expression.node,
                "since it was true, skip over false section",
            );

            // If the expression was false, it should continue here
            self.builder.patch_jump_here(jump_on_false_condition);

            // Else expression also can just take over our if target
            self.emit_expression_materialize(false_expr, ctx);

            self.builder.patch_jump_here(skip_false_if_true);
        } else {
            self.builder.patch_jump_here(jump_on_false_condition);
        }

        GeneratedExpressionResult::default()
    }

    fn emit_while_loop(
        &mut self,
        condition: &BooleanExpression,
        expression: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        // `while` loops are only for side effects, make sure that the target size is zero (Unit)
        assert_eq!(ctx.target_size().0, 0);

        let ip_for_condition = self.builder.position();

        let jump_on_false_condition = self.emit_condition_context(condition);

        // Expression is only for side effects
        let unit_ctx = self.temp_space_for_type(&Type::Unit, "while body expression");
        self.emit_expression_materialize(expression, &unit_ctx);

        // Always jump to the condition again to see if it is true
        self.builder
            .add_jmp(ip_for_condition, &expression.node, "jmp to while condition");

        self.builder.patch_jump_here(jump_on_false_condition);

        GeneratedExpressionResult::default()
    }

    fn emit_location_argument(
        &mut self,
        argument: &SingleLocationExpression,
        ctx: &Context,
        comment: &str,
    ) {
        let region = self.emit_lvalue_chain(argument, None);

        if region.size().0 != 0 {
            self.builder
                .add_mov_for_assignment(ctx.target(), &region, &argument.node, comment);
        }
    }

    fn emit_variable_assignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let target_relative_frame_pointer = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| {
                panic!(
                    "could not find id {} {}",
                    variable.unique_id_within_function, variable.assigned_name
                )
            });

        let init_ctx = ctx.with_target(
            target_relative_frame_pointer.clone(),
            "variable assignment target",
        );

        self.emit_expression_materialize(expression, &init_ctx);

        GeneratedExpressionResult::default()
    }

    fn emit_variable_binding(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) {
        let target_relative_frame_pointer = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| panic!("{}", variable.assigned_name));

        let init_ctx = ctx.with_target(
            target_relative_frame_pointer.clone(),
            "variable assignment target",
        );

        self.emit_mut_or_immute(mut_or_immutable_expression, &init_ctx)
    }

    fn emit_assignment(
        &mut self,
        node: &Node,
        lhs: &TargetAssignmentLocation,
        rhs: &Expression,
    ) -> GeneratedExpressionResult {
        let frame_relative_source = self.emit_expression_location(rhs);
        self.emit_lvalue_chain(&lhs.0, Some(frame_relative_source));

        GeneratedExpressionResult::default()
    }

    fn emit_variable_definition(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        self.emit_variable_assignment(variable, expression, ctx)
    }

    fn emit_variable_reassignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        self.emit_variable_assignment(variable, expression, ctx)
    }

    fn copy_back_mutable_arguments(
        &mut self,
        node: &Node,
        signature: &Signature,
        maybe_self: Option<FramePlacedType>,
        arguments: &Vec<MutRefOrImmutableExpression>,
    ) {
        let arguments_memory_region = self.infinite_above_frame_size();
        let mut arguments_allocator = ScopeAllocator::new(arguments_memory_region);

        let _argument_addr = reserve(&signature.return_type, &mut arguments_allocator);

        let mut parameters = signature.parameters.clone();
        if let Some(found_self) = maybe_self {
            assert_eq!(signature.parameters[0].name, "self");
            if signature.parameters[0].is_mutable && found_self.size().0 > 0 {
                let source_region = reserve(&parameters[0].resolved_type, &mut arguments_allocator);
                self.builder.add_mov_for_assignment(
                    &found_self,
                    &source_region,
                    node,
                    "copy back to <self>",
                );
            }

            parameters.remove(0);
        }
        for (parameter, argument) in parameters.iter().zip(arguments) {
            if !parameter.is_mutable {
                continue;
            }

            let source_region = reserve(&parameter.resolved_type, &mut arguments_allocator);
            if source_region.size().0 == 0 {
                continue;
            }

            if let MutRefOrImmutableExpression::Location(found_location) = argument {
                let argument_target = self.emit_lvalue_chain(found_location, None);
                self.builder.add_mov_for_assignment(
                    &argument_target,
                    &source_region,
                    node,
                    &format!(
                        "copy back mutable argument {}",
                        found_location.starting_variable.assigned_name
                    ),
                );
            } else {
                panic!("internal error. argument is mut but not a location")
            }
        }
    }
    fn emit_arguments(
        &mut self,
        node: &Node,
        signature: &Signature,
        self_region: Option<FramePlacedType>,
        arguments: &Vec<MutRefOrImmutableExpression>,
    ) -> FrameMemoryRegion {
        self.argument_allocator.reset();
        // Layout return and arguments, must be continuous space
        let argument_addr = reserve(&signature.return_type, &mut self.argument_allocator);
        //assert_eq!(argument_addr.addr.0, self.frame_size.0);

        let mut argument_final_targets = Vec::new();
        let mut argument_comments = Vec::new();

        // TODO: Temporary targets can be skipped, and the final argument targets be used instead
        // in the case that there are no nested calls. This will make the code slightly more complicated
        // so I opted to put that on the todo list for now.

        let mut argument_temp_targets = Vec::new();
        // Layout arguments, must be continuous space
        for (index, type_for_parameter) in signature.parameters.iter().enumerate() {
            let argument_final_target = reserve(
                &type_for_parameter.resolved_type,
                &mut self.argument_allocator,
            );
            let temporary = self.temp_space_for_type(&type_for_parameter.resolved_type, "argument");
            assert_eq!(temporary.target().size(), argument_final_target.size());

            argument_final_targets.push(argument_final_target);
            argument_temp_targets.push(temporary);

            argument_comments.push(format!("argument {}", type_for_parameter.name));
        }

        let skip_count = if let Some(push_self) = self_region {
            if push_self.size().0 != 0 {
                self.builder.add_mov_for_assignment(
                    argument_temp_targets[0].target(),
                    &push_self,
                    node,
                    "<self>",
                );
            }
            1
        } else {
            0
        };

        for ((argument_temp_target, argument_expr_or_loc), argument_comment) in
            argument_temp_targets
                .iter()
                .skip(skip_count)
                .zip(arguments)
                .zip(argument_comments)
        {
            self.emit_argument(
                argument_expr_or_loc,
                argument_temp_target,
                &argument_comment,
            );
        }

        for (final_target, temp_target) in argument_final_targets.iter().zip(argument_temp_targets)
        {
            if temp_target.target().size().0 != 0 {
                self.builder.add_mov_for_assignment(
                    final_target,
                    temp_target.target(),
                    node,
                    "copy in to final argument target",
                );
            }
        }

        let memory_size = argument_final_targets
            .last()
            .map_or(MemorySize(0), |last_target| {
                MemorySize(
                    last_target.addr().add(last_target.size()).0
                        - argument_final_targets[0].addr().0,
                )
            });

        let start_addr = argument_final_targets
            .first()
            .map_or(FrameMemoryAddress(0), FramePlacedType::addr);

        FrameMemoryRegion {
            addr: start_addr,
            size: memory_size,
        }
    }

    #[allow(clippy::too_many_lines)]
    fn emit_postfix_chain(
        &mut self,
        start_expression: &StartOfChain,
        chain: &[Postfix],
        final_target: Option<&Context>,
    ) -> (FramePlacedType, GeneratedExpressionResult) {
        let mut self_source_frame_placed = self.emit_start_of_chain(start_expression);
        let mut z_flag_result = GeneratedExpressionResult::default();
        for (index, element) in chain.iter().enumerate() {
            let is_last = index == chain.len() - 1;
            match &element.kind {
                PostfixKind::StructField(anonymous_struct, field_index) => {
                    let field_placed_type = self_source_frame_placed.move_to_field(*field_index);

                    self_source_frame_placed = field_placed_type;
                }
                PostfixKind::MemberCall(function_to_call, arguments) => {
                    let target_ctx = if is_last && final_target.is_some() {
                        final_target.unwrap()
                    } else {
                        &self.temp_space_for_type(&function_to_call.signature().return_type, "")
                    };

                    match &**function_to_call {
                        Function::Internal(internal_fn) => {
                            if let Some((intrinsic_fn, intrinsic_arguments)) =
                                single_intrinsic_fn(&internal_fn.body)
                            {
                                let merged_arguments = Self::merge_arguments_keep_literals(
                                    arguments,
                                    intrinsic_arguments,
                                );

                                let z_result = self.emit_single_intrinsic_call_with_self(
                                    &start_expression.node,
                                    intrinsic_fn,
                                    Some(element.ty.clone()),
                                    Some(self_source_frame_placed.clone()),
                                    &merged_arguments,
                                    target_ctx,
                                );
                                if is_last {
                                    z_flag_result = z_result;
                                }
                            } else {
                                self.emit_arguments(
                                    &start_expression.node,
                                    &internal_fn.signature.signature,
                                    Some(self_source_frame_placed.clone()),
                                    arguments,
                                );
                                self.add_call(
                                    &element.node,
                                    internal_fn,
                                    &format!("frame size: {}", self.total_frame_size),
                                ); // will be fixed up later

                                self.call_post_helper(
                                    &element.node,
                                    &internal_fn.signature.signature,
                                    Some(self_source_frame_placed.clone()),
                                    arguments,
                                    target_ctx,
                                );
                            }
                        }
                        Function::External(external_function_def) => {
                            self.emit_host_self_call(
                                &start_expression.node,
                                external_function_def,
                                &self_source_frame_placed,
                                arguments,
                                target_ctx,
                            );
                        }
                        Function::Intrinsic(intr) => {}
                        _ => panic!(
                            "{}",
                            &format!("not supported as a member call {function_to_call:?}")
                        ),
                    }
                    self_source_frame_placed = target_ctx.target().clone();
                }
                PostfixKind::OptionalChainingOperator => {
                    todo!()
                }
                PostfixKind::NoneCoalescingOperator(expression) => {
                    let target_ctx = &self.temp_space_for_type(&expression.ty, "");

                    let patch = self.builder.add_unwrap_jmp_some_placeholder(
                        target_ctx.target(),
                        &self_source_frame_placed,
                        &expression.node,
                        "none coalesce",
                    );

                    self.emit_expression_materialize(expression, target_ctx);

                    self.builder.patch_jump_here(patch);

                    self_source_frame_placed = target_ctx.target().clone();
                }
            }
        }

        (self_source_frame_placed, z_flag_result)
    }

    fn call_post_helper(
        &mut self,
        node: &Node,
        signature: &Signature,
        maybe_self: Option<FramePlacedType>,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        self.copy_back_mutable_arguments(node, signature, maybe_self, arguments);
        let return_placed_type = self.return_frame_address(&signature.return_type);
        if return_placed_type.size().0 != 0 {
            self.builder.add_mov_for_assignment(
                ctx.target(),
                &return_placed_type,
                node,
                "copy the return value to the caller",
            );
        }
        GeneratedExpressionResult::default()
    }

    fn emit_tuple(&mut self, types: &[Type], expressions: &[Expression], ctx: &Context) {
        let gen_tuple_type = layout_tuple_items(types);
        let gen_tuple_placed = BasicType {
            total_size: gen_tuple_type.total_size,
            max_alignment: gen_tuple_type.max_alignment,
            kind: BasicTypeKind::Tuple(gen_tuple_type.clone()),
        };

        assert_eq!(gen_tuple_placed.total_size, ctx.target_size());
        assert_eq!(gen_tuple_type.fields.len(), expressions.len());

        for (offset_item, expr) in gen_tuple_type.fields.iter().zip(expressions) {
            let target_addr = ctx.addr() + offset_item.offset;
            let placed_target = FramePlacedType::new(target_addr, offset_item.ty.clone());
            let element_ctx = Context::new(placed_target);
            self.emit_expression_materialize(expr, &element_ctx);
        }
    }

    fn emit_anonymous_struct(
        &mut self,
        anon_struct_type: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Expression)>,
        base_context: &Context,
    ) {
        for (field_index, expression) in source_order_expressions {
            let field_placed_type = base_context.target().move_to_field(*field_index);
            let field_ctx = Context::new(field_placed_type);
            self.emit_expression_materialize(expression, &field_ctx);
        }
    }

    fn emit_literal(
        &mut self,
        node: &Node,
        literal: &Literal,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        match literal {
            Literal::IntLiteral(int) => {
                self.builder
                    .add_ld32(ctx.target(), *int, node, "int literal");
            }
            Literal::FloatLiteral(fixed_point) => {
                self.builder
                    .add_ld32(ctx.target(), fixed_point.inner(), node, "float literal");
            }
            Literal::NoneLiteral => {
                self.builder.add_ld8(ctx.target(), 0, node, "none literal");
            }
            Literal::BoolLiteral(truthy) => {
                self.builder
                    .add_ld8(ctx.target(), u8::from(*truthy), node, "bool literal");
            }
            Literal::EnumVariantLiteral(_enum_type, a, b) => {
                self.builder.add_ld8(
                    ctx.target(),
                    a.common().container_index,
                    node,
                    &format!("enum variant {} tag", a.common().assigned_name),
                );

                let inner_addr = ctx
                    .target()
                    .union_payload(a.common().container_index as usize);
                let inner_ctx = Context::new(inner_addr);

                match b {
                    EnumLiteralData::Nothing => {}
                    EnumLiteralData::Tuple(expressions) => {
                        let EnumVariantType::Tuple(tuple_type) = a else {
                            panic!();
                        };
                        self.emit_tuple(&tuple_type.fields_in_order, expressions, &inner_ctx);
                    }
                    EnumLiteralData::Struct(sorted_expressions) => {
                        let EnumVariantType::Struct(variant_struct_type) = a else {
                            panic!()
                        };

                        self.emit_anonymous_struct(
                            &variant_struct_type.anon_struct,
                            sorted_expressions,
                            &inner_ctx,
                        );
                    }
                }
            }
            Literal::TupleLiteral(tuple_type, expressions) => {
                self.emit_tuple(tuple_type, expressions, ctx)
            }
            Literal::StringLiteral(str) => {
                self.emit_string_literal(node, str, ctx);
            }
            Literal::Slice(slice_type, expressions) => {
                self.emit_slice_literal(node, slice_type, expressions, ctx);
            }
            Literal::SlicePair(slice_pair_type, pairs) => {
                self.emit_slice_pair_literal(slice_pair_type, pairs, node, ctx);
            }
        }

        GeneratedExpressionResult::default()
    }

    fn emit_string_literal(&mut self, node: &Node, string: &str, ctx: &Context) {
        let string_bytes = string.as_bytes();
        let string_byte_count = string_bytes.len();

        let data_ptr = self
            .state
            .constants
            .allocate_byte_array(string_bytes, string_byte_count as u32);

        let string_header = StringHeader {
            heap_offset: data_ptr.addr().0,
            byte_count: string_byte_count as u32,
            capacity: string_byte_count as u32,
        };

        // Convert string header to bytes (little-endian)
        let mut header_bytes = [0u8; 12];
        header_bytes[0..4].copy_from_slice(&string_header.heap_offset.to_le_bytes());
        header_bytes[4..8].copy_from_slice(&string_header.byte_count.to_le_bytes());
        header_bytes[8..12].copy_from_slice(&string_header.capacity.to_le_bytes());

        let string_header_in_heap_ptr = HeapMemoryAddress(
            self.state
                .constants
                .allocate_byte_array(&header_bytes, header_bytes.len() as u32)
                .addr()
                .0,
        );
        let mem_size = STRING_PTR_SIZE;

        assert_eq!(ctx.target_size(), STRING_PTR_SIZE);
        self.builder.add_ld32(
            ctx.target(),
            string_header_in_heap_ptr.0 as i32,
            node,
            "constant string",
        );
    }

    fn emit_option_expression(
        &mut self,
        node: &Node,
        maybe_option: Option<&Expression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        if let Some(found_value) = maybe_option {
            let target_tag = ctx.target().move_to_optional_tag();
            self.builder
                .add_ld8(&target_tag, 1, node, "option Some tag"); // 1 signals `Some`

            let some_payload_ctx = ctx.move_to_optional_some_payload();
            self.emit_expression_materialize(found_value, &some_payload_ctx); // Fills in more of the union
        } else {
            self.builder
                .add_ld8(ctx.target(), 0, node, "option None tag"); // 0 signals `None`
            // No real need to clear the rest of the memory
        }

        GeneratedExpressionResult::default()
    }

    fn emit_for_loop(
        &mut self,
        node: &Node,
        for_pattern: &ForPattern,
        iterable: &Iterable,
        lambda_non_capturing_expr: &Box<Expression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        // Add check if the collection is empty, to skip everything

        // get some kind of iteration pointer

        // check if it has reached its end

        let collection_type = &iterable.resolved_expression.ty();

        let gen_collection =
            self.emit_expression_location_mut_ref_or_immutable(&iterable.resolved_expression);
        match collection_type {
            Type::String => {
                todo!();
            }
            Type::NamedStruct(named_type) => {
                if named_type.is_vec() {
                    self.emit_for_loop_lambda(
                        node,
                        Collection::Vec,
                        &gen_collection,
                        collection_type,
                        for_pattern,
                        &lambda_non_capturing_expr,
                        ctx,
                    )
                } else if named_type.is_map() {
                    self.emit_for_loop_lambda(
                        node,
                        Collection::Map,
                        &gen_collection,
                        collection_type,
                        for_pattern,
                        &lambda_non_capturing_expr,
                        ctx,
                    )
                } else if named_type.is_range() {
                    self.emit_for_loop_lambda(
                        node,
                        Collection::Range,
                        &gen_collection,
                        collection_type,
                        for_pattern,
                        &lambda_non_capturing_expr,
                        ctx,
                    )
                } else if named_type.is_stack() {
                    /*
                    self.emit_for_loop_lambda(
                        node,
                        Collection::Stack,
                        &gen_collection,
                        collection_type,
                        for_pattern,
                        &lambda_non_capturing_expr,
                        ctx,
                    )?

                     */
                } else if named_type.is_grid() {
                    /*
                    self.emit_for_loop_lambda(
                        node,
                        Collection::Grid,
                        &gen_collection,
                        collection_type,
                        for_pattern,
                        &lambda_non_capturing_expr,
                        ctx,
                    )?

                     */
                } else {
                    panic!("can not iterate this collection");
                }
            }
            _ => {
                panic!("can not iterate this collection");
            }
        };

        GeneratedExpressionResult::default()
    }

    fn emit_block(
        &mut self,
        expressions: &[Expression],
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        if let Some((last, others)) = expressions.split_last() {
            for expr in others {
                let temp_context = self.temp_space_for_type(&Type::Unit, "block target");
                self.emit_expression_materialize(expr, &temp_context);
            }
            self.emit_expression_materialize(last, ctx);
        }

        GeneratedExpressionResult::default()
    }

    fn get_variable_region(&self, variable: &VariableRef) -> &FramePlacedType {
        let frame_address = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap();

        frame_address
    }

    fn emit_variable_access(
        &mut self,
        node: &Node, // Variable access node
        variable: &VariableRef,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let frame_placed_variable = self.get_variable_region(variable).clone();
        self.builder.add_mov_for_assignment(
            ctx.target(),
            &frame_placed_variable,
            node,
            &format!("variable access {}", ctx.comment()),
        );

        GeneratedExpressionResult::default()
    }

    fn referenced_or_not_type(ty: &Type) -> Type {
        if let Type::MutableReference(inner_type) = ty {
            *inner_type.clone()
        } else {
            ty.clone()
        }
    }

    fn compound_assignment(
        &mut self,
        target_location: &TargetAssignmentLocation,
        op: &CompoundOperatorKind,
        source: &Expression,
    ) -> GeneratedExpressionResult {
        let target_location = self.emit_lvalue_chain(&target_location.0, None);

        let source_info = self.emit_expression_location(source);

        let type_to_consider = Self::referenced_or_not_type(&source.ty);

        match &type_to_consider {
            Type::Int => {
                self.emit_compound_assignment_i32(&source.node, &target_location, op, &source_info);
            }
            Type::Float => {
                self.emit_compound_assignment_f32(&source.node, &target_location, op, &source_info);
            }
            Type::String => todo!(),
            _ => panic!("not allowed as a compound assignment"),
        }

        GeneratedExpressionResult::default()
    }

    fn emit_compound_assignment_i32(
        &mut self,
        node: &Node,
        target: &FramePlacedType,
        op: &CompoundOperatorKind,
        source_ctx: &FramePlacedType,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.builder
                    .add_add_i32(target, target, source_ctx, node, "+=  (i32)");
            }
            CompoundOperatorKind::Sub => {
                self.builder
                    .add_sub_i32(target, target, source_ctx, node, "-=  (i32)")
            }
            CompoundOperatorKind::Mul => {
                self.builder
                    .add_mul_i32(target, target, source_ctx, node, "*=  (i32)")
            }
            CompoundOperatorKind::Div => {
                self.builder
                    .add_div_i32(target, target, source_ctx, node, "/=  (i32)")
            }
            CompoundOperatorKind::Modulo => {
                self.builder
                    .add_mod_i32(target, target, source_ctx, node, "%=  (i32)")
            }
        }
    }

    fn emit_compound_assignment_f32(
        &mut self,
        node: &Node,
        target: &FramePlacedType,
        op: &CompoundOperatorKind,
        source_ctx: &FramePlacedType,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.builder
                    .add_add_f32(target, target, source_ctx, node, "+=  (f32)");
            }
            CompoundOperatorKind::Sub => {
                self.builder
                    .add_sub_f32(target, target, source_ctx, node, "-=  (f32)")
            }
            CompoundOperatorKind::Mul => {
                self.builder
                    .add_mul_f32(target, target, source_ctx, node, "*=  (f32)")
            }
            CompoundOperatorKind::Div => {
                self.builder
                    .add_div_f32(target, target, source_ctx, node, "/=  (f32)")
            }
            CompoundOperatorKind::Modulo => {
                self.builder
                    .add_mod_f32(target, target, source_ctx, node, "%=  (f32)")
            }
        }
    }

    fn infinite_above_frame_size(&self) -> FrameMemoryRegion {
        FrameMemoryRegion::new(
            FrameMemoryAddress(self.total_frame_size.0),
            MemorySize(1024),
        )
    }

    fn return_frame_address(&self, ty: &Type) -> FramePlacedType {
        let return_layout = layout_type(ty, "");
        let addr = FrameMemoryAddress(self.total_frame_size.0);

        FramePlacedType::new(addr, return_layout)
    }

    fn emit_anonymous_struct_literal(
        &mut self,
        anon_struct_literal: &AnonymousStructLiteral,
        ty: &Type,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let anon_struct_type = match ty {
            Type::NamedStruct(named_struct) => named_struct.anon_struct_type.clone(),
            Type::AnonymousStruct(anon_struct_type) => anon_struct_type.clone(),
            _ => panic!("internal error with struct literal"),
        };

        self.emit_struct_literal_helper(
            &anon_struct_type,
            &anon_struct_literal.source_order_expressions,
            ctx,
        )
    }

    fn emit_struct_literal_helper(
        &mut self,
        struct_type_ref: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Option<Node>, Expression)>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        //let struct_type = Type::AnonymousStruct(struct_type_ref.clone());
        let gen_source_struct_type = layout_struct_type(struct_type_ref, "");

        if ctx.target_size() != gen_source_struct_type.total_size {
            info!("problem");
        }

        assert_eq!(ctx.target_size().0, gen_source_struct_type.total_size.0);
        assert_eq!(
            source_order_expressions.len(),
            gen_source_struct_type.fields.len()
        );

        for (_offset_item, (field_index, _node, expression)) in gen_source_struct_type
            .fields
            .iter()
            .zip(source_order_expressions)
        {
            let field_ctx = ctx.move_to_field_index(*field_index);
            self.emit_expression_materialize(expression, &field_ctx);
        }

        GeneratedExpressionResult::default()
    }

    fn emit_slice_literal(
        &mut self,
        node: &Node,
        slice_type: &Type,
        expressions: &[Expression],
        ctx: &Context,
    ) {
        let Type::Slice(element_type) = slice_type else {
            panic!("incorrect slice type")
        };

        let element_gen_type = layout_type(element_type, "");
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(element_gen_type.total_size.0 * element_count);
        assert_eq!(ctx.target_size(), SLICE_HEADER_SIZE);

        let heap_ptr_header_addr = ctx
            .target()
            .move_with_offset(SLICE_PTR_OFFSET, heap_ptr_size());

        self.builder.add_alloc(
            &heap_ptr_header_addr,
            total_slice_size,
            node,
            "allocate slice",
        );

        let temp_element_ctx = self.temp_allocator.reserve_ctx(element_type);

        for (index, expr) in expressions.iter().enumerate() {
            self.emit_expression_materialize(expr, &temp_element_ctx);

            let heap_offset =
                HeapMemoryOffset((index as u32) * element_gen_type.total_size.0 as u32);

            self.builder.add_stx_for_assignment(
                &heap_ptr_header_addr,
                heap_offset,
                temp_element_ctx.target(),
                node,
                "copy slice element",
            );
        }

        self.builder.add_slice_from_heap(
            ctx.target(),
            heap_ptr_header_addr.addr(),
            &element_gen_type,
            element_count,
            node,
            "slice literal",
        );
    }

    fn emit_slice_pair_literal(
        &mut self,
        slice_type: &Type,
        expressions: &[(Expression, Expression)],
        node: &Node,
        ctx: &Context,
    ) {
        let Type::SlicePair(key_type, value_type) = slice_type else {
            panic!("should have been slice pair type")
        };

        assert!(key_type.is_concrete());
        assert!(value_type.is_concrete());

        assert_eq!(ctx.target_size(), SLICE_PAIR_HEADER_SIZE);

        //let constructed_tuple = Type::Tuple(vec![*key_type.clone(), *value_type.clone()]);

        let key_layout = layout_type(key_type, "");
        let value_layout = layout_type(value_type, "");

        //info!(?key_layout, ?value_layout, "layouts");

        let pair_size = key_layout.total_size.0 + value_layout.total_size.0; // Alignment is not relevant, since we will only access them using byte chunks.
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(pair_size * element_count);

        let heap_ptr_header_addr = ctx
            .target()
            .move_with_offset(SLICE_PTR_OFFSET, heap_ptr_size());

        self.builder.add_alloc(
            &heap_ptr_header_addr,
            total_slice_size,
            node,
            "allocate slice pair",
        );

        let temp_key_ctx = self.temp_allocator.reserve_ctx(key_type);
        let temp_value_ctx = self.temp_allocator.reserve_ctx(value_type);

        for (index, (key_expr, value_expr)) in expressions.iter().enumerate() {
            self.emit_expression_materialize(key_expr, &temp_key_ctx);
            let key_offset = HeapMemoryOffset((index as u32) * pair_size as u32);
            self.builder.add_stx_for_assignment(
                &heap_ptr_header_addr,
                key_offset,
                temp_key_ctx.target(),
                node,
                "copy slice pair key element",
            );

            self.emit_expression_materialize(value_expr, &temp_value_ctx);
            let value_offset = HeapMemoryOffset((index as u32) * pair_size as u32).add(
                HeapMemorySize(key_layout.total_size.0 as u32),
                MemoryAlignment::U8,
            );
            self.builder.add_stx_for_assignment(
                &heap_ptr_header_addr,
                value_offset,
                temp_value_ctx.target(),
                node,
                "copy slice pair value element",
            );
        }

        self.builder.add_slice_pair_from_heap(
            ctx.target(),
            heap_ptr_header_addr.addr(),
            &key_layout,
            &value_layout,
            element_count,
            node,
            "creating slice pair",
        );
        /*

        SlicePairInfo {
            addr: TempFrameMemoryAddress(start_frame_address_to_transfer),
            key_size: key_layout.size,
            value_size: value_layout.size,
            element_count: CountU16(element_count),
            element_size,
        }

         */
    }

    /*
    fn emit_intrinsic_call_ex(
        &mut self,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    )  {
        //        info!(?intrinsic_fn, "generating intrinsic call");

        match intrinsic_fn {
            // Fixed
            IntrinsicFunction::FloatRound => todo!(),
            IntrinsicFunction::FloatFloor => todo!(),
            IntrinsicFunction::FloatSqrt => todo!(),
            IntrinsicFunction::FloatSign => todo!(),
            IntrinsicFunction::FloatAbs => todo!(),
            IntrinsicFunction::FloatRnd => todo!(),
            IntrinsicFunction::FloatCos => todo!(),
            IntrinsicFunction::FloatSin => todo!(),
            IntrinsicFunction::FloatAcos => todo!(),
            IntrinsicFunction::FloatAsin => todo!(),
            IntrinsicFunction::FloatAtan2 => todo!(),
            IntrinsicFunction::FloatMin => todo!(),
            IntrinsicFunction::FloatMax => todo!(),
            IntrinsicFunction::FloatClamp => todo!(),

            // i32
            IntrinsicFunction::IntAbs => todo!(),
            IntrinsicFunction::IntRnd => todo!(),
            IntrinsicFunction::IntMax => todo!(),
            IntrinsicFunction::IntMin => todo!(),
            IntrinsicFunction::IntClamp => todo!(),
            IntrinsicFunction::IntToFloat => todo!(),

            // String
            IntrinsicFunction::StringLen => todo!(),

            // Vector
            IntrinsicFunction::VecFromSlice => self.emit_intrinsic_vec_from_slice(arguments, ctx),
            IntrinsicFunction::VecPush => todo!(),
            IntrinsicFunction::VecPop => todo!(),
            IntrinsicFunction::VecFor => todo!(),
            IntrinsicFunction::VecWhile => todo!(),
            IntrinsicFunction::VecFindMap => todo!(),
            IntrinsicFunction::VecRemoveIndex => todo!(),
            IntrinsicFunction::VecRemoveIndexGetValue => todo!(),
            IntrinsicFunction::VecClear => todo!(),
            IntrinsicFunction::VecCreate => {
                self.emit_intrinsic_vec_create(arguments);
                Ok(())
            }
            IntrinsicFunction::VecSubscript => todo!(),
            IntrinsicFunction::VecSubscriptMut => todo!(),
            IntrinsicFunction::VecSubscriptRange => todo!(),
            IntrinsicFunction::VecIter => todo!(), // intentionally disregard, since it is never called
            IntrinsicFunction::VecIterMut => todo!(), // intentionally disregard, since it is never called
            IntrinsicFunction::VecLen => todo!(),
            IntrinsicFunction::VecIsEmpty => todo!(),
            IntrinsicFunction::VecSelfPush => todo!(),
            IntrinsicFunction::VecSelfExtend => todo!(),
            IntrinsicFunction::VecFold => todo!(),
            IntrinsicFunction::VecGet => todo!(),

            // Map
            IntrinsicFunction::MapCreate => todo!(),
            IntrinsicFunction::MapFromSlicePair => todo!(),
            IntrinsicFunction::MapHas => todo!(),
            IntrinsicFunction::MapRemove => todo!(),
            IntrinsicFunction::MapIter => todo!(),
            IntrinsicFunction::MapIterMut => todo!(),
            IntrinsicFunction::MapLen => todo!(),
            IntrinsicFunction::MapIsEmpty => todo!(),
            IntrinsicFunction::MapSubscript => todo!(),
            IntrinsicFunction::MapSubscriptSet => todo!(),
            IntrinsicFunction::MapSubscriptMut => todo!(),
            IntrinsicFunction::MapSubscriptMutCreateIfNeeded => todo!(),

            IntrinsicFunction::Map2GetColumn => todo!(),
            IntrinsicFunction::Map2GetRow => todo!(),
            IntrinsicFunction::Map2Remove => todo!(),
            IntrinsicFunction::Map2Has => todo!(),
            IntrinsicFunction::Map2Get => todo!(),
            IntrinsicFunction::Map2Insert => todo!(),
            IntrinsicFunction::Map2Create => todo!(),

            // Sparse
            IntrinsicFunction::SparseAdd => todo!(),
            IntrinsicFunction::SparseNew => todo!(),
            IntrinsicFunction::SparseCreate => todo!(),
            IntrinsicFunction::SparseFromSlice => todo!(),
            IntrinsicFunction::SparseIter => todo!(),
            IntrinsicFunction::SparseIterMut => todo!(),
            IntrinsicFunction::SparseSubscript => todo!(),
            IntrinsicFunction::SparseSubscriptMut => todo!(),
            IntrinsicFunction::SparseHas => todo!(),
            IntrinsicFunction::SparseRemove => todo!(),

            // Grid
            IntrinsicFunction::GridCreate => todo!(),
            IntrinsicFunction::GridFromSlice => todo!(),
            IntrinsicFunction::GridSet => todo!(),
            IntrinsicFunction::GridGet => todo!(),
            IntrinsicFunction::GridGetColumn => todo!(),

            // Other
            IntrinsicFunction::Float2Magnitude => todo!(),
            IntrinsicFunction::VecAny => todo!(),
            IntrinsicFunction::VecAll => todo!(),
            IntrinsicFunction::VecMap => todo!(),
            IntrinsicFunction::VecFilter => todo!(),
            IntrinsicFunction::VecFilterMap => todo!(),
            IntrinsicFunction::VecFind => todo!(),
            IntrinsicFunction::VecSwap => todo!(),
            IntrinsicFunction::VecInsert => todo!(),
            IntrinsicFunction::VecFirst => todo!(),
            IntrinsicFunction::VecLast => todo!(),
            IntrinsicFunction::RuntimePanic => todo!(),
            IntrinsicFunction::BoolToString => todo!(),
            IntrinsicFunction::FloatToString => todo!(),
            IntrinsicFunction::IntToString => todo!(),
        };

        Ok(())
    }

     */

    fn emit_intrinsic_vec_create(&self, arguments: &Vec<MutRefOrImmutableExpression>) {
        for arg in arguments {
            info!(?arg, "argument");
        }
    }

    fn emit_intrinsic_vec_from_slice(
        &mut self,
        node: &Node,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) {
        if let MutRefOrImmutableExpression::Expression(found_expr) = &arguments[0] {
            let memory = self.emit_expression_location(found_expr);
            self.builder
                .add_vec_from_slice(ctx.target(), &memory, node, "create vec");
        } else {
            panic!("vec_from_slice");
        }
    }

    fn emit_match(&mut self, match_expr: &Match, ctx: &Context) -> GeneratedExpressionResult {
        let region_to_match = self.emit_for_access_or_location(&match_expr.expression);

        let mut jump_to_exit_placeholders = Vec::new();

        let arm_len_to_consider = if match_expr.contains_wildcard() {
            match_expr.arms.len()
        } else {
            match_expr.arms.len()
        };
        for (index, arm) in match_expr.arms.iter().enumerate() {
            let is_last = index == arm_len_to_consider - 1;

            //  Each arm must set the CPU zero flag
            let maybe_guard = match &arm.pattern {
                Pattern::Normal(normal_pattern, maybe_guard) => match normal_pattern {
                    NormalPattern::PatternList(_) => None,
                    NormalPattern::EnumPattern(enum_variant, maybe_patterns) => {
                        self.builder.add_eq_u8_immediate(
                            &region_to_match,
                            enum_variant.common().container_index,
                            &arm.expression.node,
                            "check for enum variant",
                        );
                        maybe_guard.as_ref()
                    }
                    NormalPattern::Literal(_) => {
                        todo!()
                    }
                },
                Pattern::Wildcard(_) => {
                    // Wildcard is always true, so no comparison code is needed here at all
                    None
                }
            };

            let did_add_comparison = !matches!(arm.pattern, Pattern::Wildcard(_));

            let maybe_skip_added = if did_add_comparison {
                Some(self.builder.add_jmp_if_not_equal_placeholder(
                    &arm.expression.node,
                    "placeholder for enum match",
                ))
            } else {
                None
            };

            let maybe_guard_skip = if let Some(guard) = maybe_guard {
                Some(self.emit_condition_context(guard))

            //                Some(self.builder.add_jmp_if_not_equal_polarity_placeholder(
            //                  &polarity.polarity(),
            //                match_expr.expression.node(),
            //              "placeholder for skip guard",
            //        ))
            } else {
                None
            };

            self.emit_expression_materialize(&arm.expression, ctx);

            if !is_last {
                let jump_to_exit_placeholder = self.builder.add_jump_placeholder(
                    &arm.expression.debug_last_expression().node,
                    "jump to exit",
                );
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
            }

            if let Some(skip) = maybe_skip_added {
                self.builder.patch_jump_here(skip);
            }
            if let Some(guard_skip) = maybe_guard_skip {
                self.builder.patch_jump_here(guard_skip);
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.builder.patch_jump_here(placeholder);
        }

        GeneratedExpressionResult::default()
    }

    fn emit_guard(&mut self, guards: &Vec<Guard>, ctx: &Context) -> GeneratedExpressionResult {
        let mut jump_to_exit_placeholders = Vec::new();
        for guard in guards {
            if let Some(condition) = &guard.condition {
                //                let result = self.emit_boolean_expression_z_flag(condition)?;
                let skip_expression_patch = self.emit_condition_context(condition);
                //&result.polarity(),
                //&guard.result.node,
                //"guard condition",
                //);
                self.emit_expression_materialize(&guard.result, ctx);
                let jump_to_exit_placeholder = self.builder.add_jump_placeholder(
                    &guard.result.debug_last_expression().node,
                    "jump to exit",
                );
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
                self.builder.patch_jump_here(skip_expression_patch);
            } else {
                // _ -> wildcard
                self.emit_expression_materialize(&guard.result, ctx);
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.builder.patch_jump_here(placeholder);
        }

        GeneratedExpressionResult::default()
    }

    fn emit_when(
        &mut self,
        bindings: &Vec<WhenBinding>,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let mut all_false_jumps = Vec::new();

        for binding in bindings {
            //            let placed_binding_variable = self.get_variable_region(&binding.variable);
            let old_variable_region = self.emit_for_access_or_location(&binding.expr);

            self.builder
                .add_tst8(&old_variable_region, binding.expr.node(), "check binding");
            let patch = self
                .builder
                .add_jmp_if_not_equal_placeholder(binding.expr.node(), "jump if none");
            all_false_jumps.push(patch);
        }

        // if we are here all bindings are `Some`, so it is safe to get the payload
        for binding in bindings {
            let placed_variable = self.get_variable_region(&binding.variable).clone();

            if binding.has_expression() {
                let var_ctx = Context::new(placed_variable);
                self.emit_mut_or_immute(&binding.expr, &var_ctx);
            } else {
                let MutRefOrImmutableExpression::Expression(variable_access_expression) =
                    &binding.expr
                else {
                    panic!("must be expression");
                };
                let old_variable_region = self.emit_expression_location(variable_access_expression);

                let some_payload = old_variable_region.move_to_optional_some_payload();

                self.builder.add_movlp_for_assignment(
                    &placed_variable,
                    &some_payload,
                    binding.expr.node(),
                    "move from Some to value",
                );
            }
        }

        self.emit_expression_materialize(true_expr, ctx);
        let maybe_jump_over_false = if let Some(else_expr) = maybe_false_expr {
            Some(
                self.builder
                    .add_jump_placeholder(&else_expr.node, "jump over false section"),
            )
        } else {
            None
        };

        for false_jump_patch in all_false_jumps {
            self.builder.patch_jump_here(false_jump_patch);
        }

        if let Some(else_expr) = maybe_false_expr {
            self.emit_expression_materialize(else_expr, ctx);
            self.builder.patch_jump_here(maybe_jump_over_false.unwrap());
        }

        GeneratedExpressionResult::default()
    }

    fn emit_tuple_destructuring(
        &mut self,
        target_variables: &[VariableRef],
        tuple_type: &[Type],
        source_tuple_expression: &Expression,
    ) -> GeneratedExpressionResult {
        let source_region = self.emit_expression_location(source_tuple_expression);

        let tuple_type = layout_tuple_items(tuple_type);
        assert_eq!(tuple_type.total_size.0, source_region.size().0);

        for (tuple_index, target_variable) in target_variables.iter().enumerate() {
            if target_variable.is_unused {
            } else {
                let frame_placed_target_variable =
                    self.get_variable_region(target_variable).clone();

                //                assert_eq!(frame_placed_target_variable.size().0, offset_item.size.0);

                let source_part = source_region.move_to_field(tuple_index);

                self.builder.add_mov_for_assignment(
                    &frame_placed_target_variable,
                    &source_part,
                    &target_variable.name,
                    &format!(
                        "destructuring to variable {}",
                        target_variable.assigned_name
                    ),
                );
            }
        }

        GeneratedExpressionResult::default()
    }

    fn emit_constant_access(
        &mut self,
        node: &Node,
        constant_reference: &ConstantRef,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        //info!(?constant_reference, "looking up constant");
        let constant_region = self
            .state
            .constant_offsets
            .get(&constant_reference.id)
            .unwrap();
        assert_eq!(ctx.target_size(), constant_region.size());

        self.builder.add_mov_mem_for_assignment(
            ctx.target(),
            constant_region,
            node,
            &format!("load constant '{}'", constant_reference.assigned_name),
        );

        GeneratedExpressionResult::default()
    }

    fn emit_coerce_option_to_bool(
        &mut self,
        expr: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let region = self.emit_expression_location(expr);
        self.builder.add_mov(
            ctx.target(),
            &region,
            MemorySize(1),
            &expr.node,
            "move option tag to bool",
        );

        GeneratedExpressionResult::default()
    }

    fn emit_start_of_chain(&mut self, start: &StartOfChain) -> FramePlacedType {
        match &start.kind {
            StartOfChainKind::Expression(expr) => self.emit_expression_location(expr),
            StartOfChainKind::Variable(variable) => {
                let frame_placed_type = self.get_variable_region(variable);
                frame_placed_type.clone()
            }
        }
    }

    fn emit_internal_call(
        &mut self,
        node: &Node,
        internal_fn: &InternalFunctionDefinitionRef,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        self.emit_arguments(node, &internal_fn.signature.signature, None, arguments);

        self.add_call(
            node,
            internal_fn,
            &format!("frame size: {}", self.total_frame_size),
        ); // will be fixed up later

        self.call_post_helper(node, &internal_fn.signature.signature, None, arguments, ctx)
    }

    fn emit_host_call(
        &mut self,
        node: &Node,
        host_fn: &ExternalFunctionDefinitionRef,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let memory_region = self.emit_arguments(node, &host_fn.signature, None, arguments);

        self.builder.add_host_call(
            host_fn.id as u16,
            memory_region.size,
            node,
            &format!(
                "host: {} arguments_size:{}",
                host_fn.assigned_name, memory_region.size.0
            ),
        );

        self.call_post_helper(node, &host_fn.signature, None, arguments, ctx)
    }

    fn emit_host_self_call(
        &mut self,
        node: &Node,
        host_fn: &ExternalFunctionDefinitionRef,
        self_frame_placed_type: &FramePlacedType,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let memory_region = self.emit_arguments(
            node,
            &host_fn.signature,
            Some(self_frame_placed_type.clone()),
            arguments,
        );

        self.builder.add_host_call(
            host_fn.id as u16,
            memory_region.size,
            node,
            &format!(
                "host self call: {} arguments_size:{}",
                host_fn.assigned_name, memory_region.size.0
            ),
        ); // will be fixed up later

        self.call_post_helper(
            node,
            &host_fn.signature,
            Some(self_frame_placed_type.clone()),
            arguments,
            ctx,
        )
    }

    fn merge_arguments_keep_literals(
        outer_args: &Vec<MutRefOrImmutableExpression>,
        intrinsic_args: &Vec<MutRefOrImmutableExpression>,
    ) -> Vec<MutRefOrImmutableExpression> {
        // HACK: we assume that the parameters are in the same order.
        // If one has more arguments, we assume that those extra arguments are in the end
        // We also assume that the first is self
        let mut all_args = outer_args.clone();

        if intrinsic_args.len() > outer_args.len() + 1 {
            all_args.extend_from_slice(&intrinsic_args[outer_args.len() + 1..]);
        }

        all_args
    }

    /// Generates code to iterate over a collection using a transformer (e.g., map, filter, filter_map)
    /// and a lambda expression. Handles creation of result vectors, iterator setup, lambda invocation,
    /// early exit logic, and result collection.
    ///
    /// Steps:
    /// 1. (Optional) Initialize a target vector for the result, if the transformer produces one.
    /// 2. Initialize the iterator for the collection.
    /// 3. Generate code to fetch the next element from the iterator.
    /// 4. Inline the lambda code for the current element.
    /// 5. If the transformer supports early exit (e.g., filter, find), set the Z flag based on the lambda result.
    /// 6. Conditionally skip result insertion if early exit is triggered.
    /// 7. (Optional) If applicable, insert the (possibly unwrapped) result into the target vector.
    /// 8. Loop back to fetch the next element.
    /// 9. Finalize iteration, handling any post-processing (e.g., normalizing boolean results).
    ///
    /// # Parameters
    /// - `node`: The AST node for error reporting and code location.
    /// - `collection_type`: The type of collection being iterated.
    /// - `transformer`: The transformer operation (map, filter, find, fold, etc.).
    /// - `collection_self_region`: Memory region of the collection.
    /// - `lambda_expression`: The lambda expression to apply.
    /// - `ctx`: Code generation context. Contains the result target.
    ///
    /// # Returns
    /// - `Ok(())` on success, or an error if code generation fails.
    ///
    /// # Errors
    /// // TODO:
    /// # Panics
    /// - If the lambda expression or its kind is not as expected (internal error).
    #[allow(clippy::too_many_lines)]
    #[allow(clippy::too_many_arguments)]
    pub fn iterate_over_collection_with_lambda(
        &mut self,
        node: &Node,
        source_collection_type: Collection,
        transformer: Transformer,
        source_collection_self_region: &FramePlacedType,
        source_collection_analyzed_type: &Type,
        lambda_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) {
        // Take out lambda and other lookups before generating the code
        let MutRefOrImmutableExpression::Expression(expr) = lambda_expression else {
            panic!("internal error");
        };

        let ExpressionKind::Lambda(lambda_variables, lambda_expr) = &expr.kind else {
            panic!();
        };

        let primary_element_type = source_collection_analyzed_type.primary_element_type();

        let target_variables: Vec<_> = lambda_variables
            .iter()
            .map(|x| {
                self.variable_offsets
                    .get(&x.unique_id_within_function)
                    .unwrap()
                    .clone()
            })
            .collect();

        // Primary is the right most variable
        let primary_variable = &target_variables[target_variables.len() - 1];

        let lambda_return_analyzed_type = &lambda_expr.ty;

        // 1. Optionally initialize the result vector if the transformer produces one.
        let lambda_return_gen_type = layout_type(lambda_return_analyzed_type, "for_iterator");

        if matches!(
            transformer.return_type(),
            TransformerResult::VecWithLambdaResult | TransformerResult::VecFromSourceCollection
        ) {
            let element_size_in_target_vec = match transformer.return_type() {
                TransformerResult::VecFromSourceCollection => {
                    let element_gen_type = layout_type(primary_element_type.unwrap(), "");
                    element_gen_type.total_size
                }
                TransformerResult::VecWithLambdaResult => {
                    if transformer.needs_tag_removed() {
                        let (_tag_size, _tag_offset, _payload_offset, payload_size) =
                            lambda_return_gen_type.unwrap_info().unwrap();
                        payload_size
                    } else {
                        lambda_return_gen_type.total_size
                    }
                }
                _ => panic!("should not happen"),
            };

            assert_eq!(ctx.target_size(), VEC_PTR_SIZE);
            self.builder.add_vec_create(
                ctx.target(),
                element_size_in_target_vec,
                node,
                "target result vector",
            );
        }

        // 2. Initialize the iterator and generate code to fetch the next element.
        let (continue_iteration_label, iteration_complete_patch_position) = self
            .iter_init_and_next(
                node,
                source_collection_type,
                source_collection_self_region,
                &target_variables,
            );

        // 3. Inline the lambda code for the current element(s).
        let lambda_result = self.emit_expression_location(lambda_expr);

        // 4. If the transformer supports early exit, set the Z flag based on the lambda result.
        let transformer_z_flag_state =
            self.check_if_transformer_sets_z_flag(transformer, &lambda_result, node);

        // 5. Conditionally skip result insertion if early exit is triggered.
        let maybe_skip_early = if matches!(
            transformer_z_flag_state,
            GeneratedExpressionResultKind::ZFlagIsTrue
                | GeneratedExpressionResultKind::ZFlagIsInversion
        ) {
            // The z flag is set so we can act on it
            let skip_early = self.builder.add_jmp_if_not_equal_polarity_placeholder(
                &transformer_z_flag_state.polarity(),
                node,
                "skip early",
            );

            Some(skip_early)
        } else {
            // Z flag is not set, we have to iterate through the whole collection
            None
        };

        // 6. If applicable, insert the (possibly unwrapped) result into the target vector.
        match transformer.return_type() {
            TransformerResult::Unit => {
                // Only alternative is that it is a bool return, so no need to take any action here
            }
            TransformerResult::Bool => {
                // Only alternative is that it is a bool return, so no need to take any action here
            }
            TransformerResult::WrappedValueFromSourceCollection => {
                // Handled elsewhere
            }
            TransformerResult::VecWithLambdaResult => {
                self.transformer_add_to_collection(
                    &lambda_result,
                    transformer.needs_tag_removed(),
                    source_collection_type,
                    ctx.target(),
                    node,
                );
            }
            TransformerResult::VecFromSourceCollection => {
                self.add_to_collection(
                    node,
                    source_collection_type,
                    ctx.target(),
                    primary_variable,
                );
            }
        }

        // 7. Loop back to fetch the next element.
        self.builder.add_jmp(
            continue_iteration_label,
            &lambda_expr.debug_last_expression().node,
            "jump to iter_next",
        );

        self.builder
            .patch_jump_here(iteration_complete_patch_position);

        // 8. Finalize iteration, handling any post-processing (e.g., normalizing boolean results).
        if let Some(found_skip_early) = maybe_skip_early {
            self.builder.patch_jump_here(found_skip_early);
        }

        match transformer.return_type() {
            TransformerResult::Bool => {
                // It is a transformer that returns a bool, lets store z flag as bool it
                self.builder
                    .add_stz(ctx.target(), node, "transformer sets standard bool");
            }
            TransformerResult::WrappedValueFromSourceCollection => {
                let BasicTypeKind::Optional(tagged_union) = &ctx.ty().kind else {
                    panic!("expected optional");
                };

                let tag_target = ctx.target().move_to_optional_tag();
                self.builder
                    .add_ld8(&tag_target, 1, node, "mark tag as Some");

                let some_payload_target = ctx.target().move_to_optional_some_payload();
                self.builder.add_mov_for_assignment(
                    &some_payload_target,
                    primary_variable,
                    node,
                    "copy into optional return",
                );
            }
            _ => {}
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn iter_init_and_next(
        &mut self,
        node: &Node,
        collection_type: Collection,
        collection_self_addr: &FramePlacedType,
        target_variables: &[FramePlacedType],
    ) -> (InstructionPosition, PatchPosition) {
        let iterator_gen_type = collection_type.iterator_gen_type();

        let iterator_target = self.temp_allocator.allocate_type(iterator_gen_type);

        let iter_next_position = InstructionPosition(self.builder.position().0 + 1);
        let placeholder = match collection_type {
            Collection::Vec => {
                self.builder.add_vec_iter_init(
                    &iterator_target,
                    collection_self_addr,
                    node,
                    "vec init",
                );

                if target_variables.len() == 2 {
                    self.builder.add_vec_iter_next_pair_placeholder(
                        &iterator_target,
                        &target_variables[0],
                        &target_variables[1],
                        node,
                        "vec iter next pair",
                    )
                } else {
                    self.builder.add_vec_iter_next_placeholder(
                        &iterator_target,
                        &target_variables[0],
                        node,
                        "vec iter next single",
                    )
                }
            }
            Collection::Map => {
                self.builder.add_map_iter_init(
                    &iterator_target,
                    collection_self_addr,
                    node,
                    "map init",
                );

                if target_variables.len() == 2 {
                    self.builder.add_map_iter_next_pair_placeholder(
                        &iterator_target,
                        &target_variables[0],
                        &target_variables[1],
                        node,
                        "map next_pair",
                    )
                } else {
                    self.builder.add_map_iter_next_placeholder(
                        &iterator_target,
                        &target_variables[0],
                        node,
                        "map next_single",
                    )
                }
            }
            Collection::Grid => todo!(),
            Collection::Range => {
                self.builder.add_range_iter_init(
                    &iterator_target,
                    collection_self_addr,
                    node,
                    "range init",
                );

                assert_eq!(target_variables.len(), 1);
                self.builder.add_range_iter_next_placeholder(
                    &iterator_target,
                    &target_variables[0],
                    node,
                    "range iter next single",
                )
            }

            // Low  prio
            Collection::String => todo!(),
        };

        (iter_next_position, placeholder)
    }

    fn check_if_transformer_sets_z_flag(
        &mut self,
        transformer: Transformer,
        in_value: &FramePlacedType,
        node: &Node,
    ) -> GeneratedExpressionResultKind {
        match transformer {
            Transformer::For => GeneratedExpressionResultKind::ZFlagUnmodified,
            Transformer::Filter => {
                assert_eq!(in_value.size().0, 1); // bool
                self.builder
                    .add_tst8(in_value, node, "filter bool to z flag");
                GeneratedExpressionResultKind::ZFlagIsTrue
            }
            Transformer::Find => {
                assert_eq!(in_value.size().0, 1); // bool
                self.builder
                    .add_tst8(in_value, node, "find: bool to z flag");
                GeneratedExpressionResultKind::ZFlagIsInversion
            }
            Transformer::Map => GeneratedExpressionResultKind::ZFlagUnmodified,
            Transformer::Any => {
                self.builder.add_tst8(in_value, node, "any, check tag");
                GeneratedExpressionResultKind::ZFlagIsInversion
            }
            Transformer::All => {
                self.builder.add_tst8(in_value, node, "all, check tag");
                GeneratedExpressionResultKind::ZFlagIsTrue
            }
            Transformer::FilterMap => {
                self.builder
                    .add_tst8(in_value, node, "filter map, check tag");
                GeneratedExpressionResultKind::ZFlagIsTrue
            }
        }
    }

    fn add_to_collection(
        &mut self,
        node: &Node,
        collection: Collection,
        mut_collection: &FramePlacedType,
        value: &FramePlacedType,
    ) {
        match collection {
            Collection::Vec => {
                self.builder
                    .add_vec_push(mut_collection, value, node, "push");
            }
            Collection::Map => todo!(),
            Collection::Grid => todo!(),
            Collection::String => todo!(),
            Collection::Range => todo!(),
        }
    }

    fn transformer_add_to_collection(
        &mut self,
        in_value: &FramePlacedType,
        should_unwrap_value: bool,
        collection_type: Collection,
        mut_collection: &FramePlacedType,
        node: &Node,
    ) {
        let adjusted_region = if should_unwrap_value {
            let option_payload_source = in_value.move_to_optional_some_payload();
            &option_payload_source.clone()
        } else {
            in_value
        };

        self.add_to_collection(node, collection_type, mut_collection, adjusted_region);
    }
    fn materialize_z_flag_to_bool_if_needed(
        &mut self,
        target: &FramePlacedType,
        z_flag_state: GeneratedExpressionResult,
        node: &Node,
    ) {
        match z_flag_state.kind {
            GeneratedExpressionResultKind::ZFlagUnmodified => {
                // intentionally do nothing
            }
            GeneratedExpressionResultKind::ZFlagIsTrue => {
                self.builder
                    .add_stz(target, node, "materialize positive z flag");
            }
            GeneratedExpressionResultKind::ZFlagIsInversion => {
                self.builder
                    .add_stnz(target, node, "materialize inverse z flag");
            }
        }
    }

    fn emit_for_loop_lambda(
        &mut self,
        node: &Node,
        collection: Collection,
        source_collection: &FramePlacedType,
        source_collection_type: &Type,
        for_pattern: &ForPattern,
        lambda_expr: &Expression,

        ctx: &Context,
    ) {
        let variables = match for_pattern {
            ForPattern::Single(a) => vec![a.clone()],
            ForPattern::Pair(a, b) => vec![a.clone(), b.clone()],
        };

        let fake_lambda_kind = ExpressionKind::Lambda(variables, Box::from(lambda_expr.clone()));
        let fake_lambda_expr = MutRefOrImmutableExpression::Expression(Expression {
            ty: lambda_expr.ty.clone(),
            node: node.clone(),
            kind: fake_lambda_kind,
        });

        self.iterate_over_collection_with_lambda(
            node,
            collection,
            Transformer::For,
            source_collection,
            source_collection_type,
            &fake_lambda_expr,
            ctx,
        )
    }
}

fn single_intrinsic_fn(
    body: &Expression,
) -> Option<(&IntrinsicFunction, &Vec<MutRefOrImmutableExpression>)> {
    let ExpressionKind::Block(block_expressions) = &body.kind else {
        panic!("function body should be a block")
    };

    if let ExpressionKind::IntrinsicCallEx(found_intrinsic_fn, arguments) =
        &block_expressions[0].kind
    {
        Some((found_intrinsic_fn, arguments))
    } else {
        None
    }
}
