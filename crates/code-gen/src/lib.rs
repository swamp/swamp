/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

pub mod alloc;
pub mod alloc_util;
pub mod constants;
pub mod ctx;
mod layout;
mod location;
mod vec;

use crate::alloc::ScopeAllocator;
use crate::alloc_util::reserve_space_for_type;
use crate::constants::ConstantsManager;
use crate::ctx::Context;
use crate::layout::layout_type;
use crate::layout::layout_variables;
use crate::layout::type_size_and_alignment;
use crate::layout::{layout_enum_into_tagged_union, layout_tuple_items};
use crate::vec::{MAP_LENGTH_OFFSET, VECTOR_LENGTH_OFFSET};
use seq_map::SeqMap;
use source_map_cache::{FileLineInfo, SourceMapLookup, SourceMapWrapper};
use source_map_node::Node;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{
    AnonymousStructLiteral, BinaryOperator, BinaryOperatorKind, BooleanExpression,
    CompoundOperatorKind, ConstantId, ConstantRef, EnumLiteralData, Expression, ExpressionKind,
    ExternalFunctionDefinitionRef, ForPattern, Function, Guard, InternalFunctionDefinitionRef,
    InternalFunctionId, InternalMainExpression, Iterable, Literal, Match,
    MutRefOrImmutableExpression, NormalPattern, Pattern, Postfix, PostfixKind,
    SingleLocationExpression, StartOfChain, StartOfChainKind, TargetAssignmentLocation,
    UnaryOperator, UnaryOperatorKind, Variable, VariableRef, WhenBinding,
};
use swamp_types::{
    AnonymousStructType, EnumVariantType, NamedStructType, Signature, StructTypeField, Type,
};
use swamp_vm_debug_types::{
    BasicType, FrameMemoryInfo, FunctionInfo, FunctionInfoKind, show_frame_memory,
};
use swamp_vm_disasm::{SourceFileLineInfo, disasm_instructions_color};
use swamp_vm_instr_build::{InstructionBuilder, InstructionBuilderState, PatchPosition};
use swamp_vm_types::{
    BinaryInstruction, CountU16, FrameMemoryAddress, FrameMemoryRegion, FrameMemorySize,
    GRID_HEADER_ALIGNMENT, GRID_HEADER_SIZE, HeapMemoryOffset, HeapMemoryRegion,
    InstructionPosition, InstructionPositionOffset, InstructionRange, MAP_HEADER_ALIGNMENT,
    MAP_HEADER_SIZE, MemoryAlignment, MemoryOffset, MemorySize, Meta, RANGE_HEADER_ALIGNMENT,
    RANGE_HEADER_SIZE, SLICE_COUNT_OFFSET, SLICE_HEADER_SIZE, SLICE_PTR_OFFSET,
    STRING_HEADER_ALIGNMENT, STRING_HEADER_SIZE, TempFrameMemoryAddress, VEC_HEADER_ALIGNMENT,
    VEC_HEADER_SIZE, VEC_ITERATOR_ALIGNMENT, VEC_ITERATOR_SIZE, ZFlagPolarity,
};
use tracing::{error, info};

#[derive(Copy, Clone)]
pub enum Transformer {
    Filter,
    Map,
    Any,
    All,
    FilterMap,
}

pub enum TransformerResult {
    Bool,
    VecWithLambdaResult,
    VecFromSourceCollection,
}

impl Transformer {
    pub(crate) const fn return_type(self) -> TransformerResult {
        match self {
            Self::Filter => TransformerResult::VecFromSourceCollection,
            Self::FilterMap | Self::Map => TransformerResult::VecWithLambdaResult,
            Self::All | Self::Any => TransformerResult::Bool,
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
            //     Self::Map => (MAP_HEADER_SIZE, MAP_HEADER_ALIGNMENT),
            //   Self::Grid => (GRID_HEADER_SIZE, GRID_HEADER_ALIGNMENT),
            // Self::String => (STRING_HEADER_SIZE, STRING_HEADER_ALIGNMENT),
            //Self::Range => (RANGE_HEADER_SIZE, RANGE_HEADER_ALIGNMENT),
            _ => todo!(),
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

#[derive(Debug)]
pub enum ErrorKind {
    IllegalCompoundAssignment,
    VariableNotUnique,
    IllegalCollection,
    NotAnIterableCollection,
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub node: Node,
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
    pub target_constant_memory: HeapMemoryRegion,
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
    constant_offsets: SeqMap<ConstantId, HeapMemoryRegion>,
    constant_functions: SeqMap<ConstantId, ConstantInfo>,
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
        let file_line_info = if meta.node.span.file_id == 0 {
            FileLineInfo {
                row: 0,
                col: 0,
                line: "".to_string(),
                relative_file_name: "".to_string(),
            }
        } else {
            //let text = source_map_wrapper.get_text_span(&meta.node.span);
            source_map_wrapper.get_line(&meta.node.span)
        };
        let is_different_line = if let Some(previous) = &previous_node {
            different_file_info(&file_line_info, previous)
        } else {
            true
        };
        previous_node = Some(FileLineInfo {
            row: file_line_info.row,
            col: file_line_info.col,
            line: file_line_info.line.clone(),
            relative_file_name: file_line_info.relative_file_name.clone(),
        });

        if is_different_line {
            let mapped = SourceFileLineInfo {
                row: file_line_info.row,
                col: file_line_info.col,
                line: file_line_info.line,
                relative_file_name: file_line_info.relative_file_name,
            };
            ip_infos
                .insert(InstructionPosition(absolute_ip as u16), mapped)
                .unwrap();
        }
    }

    /*
    for frame_relative_info in frame_relative_infos {
        let converted_kind = match &frame_relative_info.kind {
            FrameRelativeInfoKind::Variable(var) => FrameAddressInfoKind::Variable(VariableInfo {
                is_mutable: var.is_mutable(),
                name: var.assigned_name.clone(),
                ty: ComplexType::BasicType(BasicType::S32),
            }),
        };

        memory_infos.push(FrameAddressInfo {
            addr: frame_relative_info.frame_memory_region.addr,
            size: FrameMemorySize(frame_relative_info.frame_memory_region.size.0),
            kind: converted_kind,
        })
    }

    let mem_info = FrameMemoryInfo {
        infos: memory_infos,
        size: FrameMemorySize(),
    };

     */

    format!(
        "{}\n{}",
        header_output,
        disasm_instructions_color(
            instructions,
            &ip_offset,
            meta,
            frame_relative_infos,
            &ip_infos,
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
            constant_functions: SeqMap::default(),
            function_fixups: vec![],
            function_ips: FunctionIps::default(),
            function_debug_infos: SeqMap::default(),
        }
    }

    #[must_use]
    pub fn constant_functions(&self) -> &SeqMap<ConstantId, ConstantInfo> {
        &self.constant_functions
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

        for (_func_id, function_info) in &self.constant_functions {
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

    pub fn reserve_space_for_constants(&mut self, constants: &[ConstantRef]) -> Result<(), Error> {
        for constant in constants {
            let (size, alignment) = type_size_and_alignment(&constant.resolved_type);

            let heap_memory_address = self.constants.allocator.allocate(size, alignment);

            let constant_memory_region = HeapMemoryRegion {
                addr: heap_memory_address,
                size,
            };

            self.constant_offsets
                .insert(constant.id, constant_memory_region)
                .unwrap();
        }

        Ok(())
    }
}

pub fn reserve(ty: &Type, allocator: &mut ScopeAllocator) -> FrameMemoryRegion {
    let (size, alignment) = type_size_and_alignment(ty);
    allocator.reserve(size, alignment)
}

pub struct FrameAndVariableInfo {
    pub frame_memory: FrameMemoryInfo,
    variable_offsets: SeqMap<usize, FrameMemoryRegion>,
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

    pub fn reserve_space_for_constants(&mut self, constants: &[ConstantRef]) -> Result<(), Error> {
        self.codegen_state.reserve_space_for_constants(constants)
    }

    pub fn gen_function_def(
        &mut self,
        internal_fn_def: &InternalFunctionDefinitionRef,
        options: &GenOptions,
        source_map_wrapper: &SourceMapWrapper,
    ) -> Result<(), Error> {
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

        let (start_ip, end_ip, function_info) =
            self.gen_function_preamble(&in_data, source_map_wrapper)?;

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

        Ok(())
    }

    /// # Errors
    ///
    pub fn gen_main_function(
        &mut self,
        main: &InternalMainExpression,
        options: &GenOptions,
        source_map_lookup: &SourceMapWrapper,
    ) -> Result<(), Error> {
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
            self.gen_function_preamble(&in_data, source_map_lookup)?;

        let function_info = FunctionInfo {
            kind: FunctionInfoKind::Normal(main.program_unique_id as usize),
            frame_memory: variable_and_frame_memory.frame_memory,
            name: "main".to_string(),
            ip_range: InstructionRange {
                start: start_ip.clone(),
                count: InstructionPositionOffset(end_ip.0 - start_ip.0),
            },
        };

        Ok(())
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

    pub fn gen_function_preamble(
        &mut self,
        in_data: &FunctionInData,
        source_map_wrapper: &SourceMapWrapper,
    ) -> Result<(InstructionPosition, InstructionPosition, FunctionInfo), Error> {
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
            source_map_wrapper,
        );

        /*
        let ExpressionKind::Block(block_expressions) = &in_data.expression.kind else {
            panic!("function body should be a block")
        };

         */

        let (return_type_size, _return_alignment) = type_size_and_alignment(&in_data.return_type);
        let ctx = Context::new(FrameMemoryRegion::new(
            FrameMemoryAddress(0),
            return_type_size,
        ));
        //        info!(?in_data, "generate");
        function_generator.gen_expression_materialize(&in_data.expression, &ctx)?;

        self.finalize_function(&GenOptions {
            is_halt_function: true,
        });

        let end_ip = self.ip();

        function_info.ip_range.count = InstructionPositionOffset(end_ip.0 - start_ip.0);

        Ok((start_ip, end_ip, function_info))
    }

    pub fn gen_constants_expression_functions_in_order(
        &mut self,
        constants: &[ConstantRef],
        source_map_wrapper: &SourceMapWrapper,
    ) -> Result<(), Error> {
        for constant in constants {
            let target_region = *self
                .codegen_state
                .constant_offsets
                .get(&constant.id)
                .unwrap();

            let in_data = FunctionInData {
                function_name_node: constant.name.clone(),
                kind: FunctionInfoKind::Constant(constant.id as usize),
                assigned_name: constant.assigned_name.clone(),
                all_variables_parameters_first: constant.function_scope_state.clone(),
                return_type: constant.resolved_type.clone(),
                expression: constant.expr.clone(),
            };

            let (start_ip, end_ip, function_info) =
                self.gen_function_preamble(&in_data, source_map_wrapper)?;

            let constant_info = ConstantInfo {
                ip_range: InstructionRange {
                    count: InstructionPositionOffset(end_ip.0 - start_ip.0),
                    start: start_ip.clone(),
                },
                target_constant_memory: target_region,
                constant_ref: constant.clone(),
            };

            self.codegen_state
                .constant_functions
                .insert(constant.id, constant_info)
                .unwrap();

            self.codegen_state
                .function_debug_infos
                .insert(start_ip, function_info)
                .unwrap();
        }

        Ok(())
    }

    #[must_use]
    pub fn take_instructions_and_constants(
        self,
    ) -> (Vec<BinaryInstruction>, SeqMap<ConstantId, ConstantInfo>) {
        (
            self.builder_state.instructions,
            self.codegen_state.constant_functions,
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
    variable_offsets: SeqMap<usize, FrameMemoryRegion>,
    frame_size: FrameMemorySize,
    temp_allocator: ScopeAllocator,
    argument_allocator: ScopeAllocator,
    source_map_lookup: &'a SourceMapWrapper<'a>,
}

impl<'a> FunctionCodeGen<'a> {
    #[must_use]
    pub fn new(
        state: &'a mut CodeGenState,
        builder: &'a mut InstructionBuilder<'a>,
        variable_offsets: SeqMap<usize, FrameMemoryRegion>,
        frame_size: FrameMemorySize,
        source_map_lookup: &'a SourceMapWrapper,
    ) -> Self {
        const ARGUMENT_MAX_SIZE: u16 = 2 * 1024;

        Self {
            state,
            variable_offsets,
            frame_size,
            temp_allocator: ScopeAllocator::new(FrameMemoryRegion::new(
                FrameMemoryAddress(frame_size.0 + ARGUMENT_MAX_SIZE),
                MemorySize(32 * 1024),
            )),
            argument_allocator: ScopeAllocator::new(FrameMemoryRegion::new(
                FrameMemoryAddress(frame_size.0),
                MemorySize(ARGUMENT_MAX_SIZE),
            )),
            builder,
            source_map_lookup,
        }
    }
}

impl FunctionCodeGen<'_> {
    #[allow(clippy::too_many_lines)]
    #[allow(clippy::single_match_else)]
    pub fn gen_single_intrinsic_call(
        &mut self,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        match intrinsic_fn {
            IntrinsicFunction::VecFromSlice => {
                let MutRefOrImmutableExpression::Expression(expr) = &arguments[0] else {
                    panic!("problem");
                };

                let slice_region = self.gen_expression_location(expr)?;

                let slice_type = arguments[0].ty();

                let Type::Slice(element_type) = slice_type else {
                    panic!("problem");
                };

                assert!(element_type.is_concrete());

                let (element_size, _element_alignment) = type_size_and_alignment(&element_type);

                self.builder.add_vec_from_slice(
                    ctx.addr(),
                    slice_region.addr,
                    element_size,
                    node,
                    "vec_from_slice",
                );
                Ok(GeneratedExpressionResult::default())
            }

            IntrinsicFunction::MapFromSlicePair => {
                let MutRefOrImmutableExpression::Expression(expr) = &arguments[0] else {
                    panic!("problem");
                };

                let slice_region = self.gen_expression_location(expr)?;

                let slice_type = arguments[0].ty();

                let Type::SlicePair(key_type, value_type) = slice_type else {
                    panic!("problem");
                };

                assert!(key_type.is_concrete_or_unit()); // is unit when it is empty
                assert!(value_type.is_concrete_or_unit());

                let element_pair_layout = layout_tuple_items(&[*key_type, *value_type]);
                let key_layout = &element_pair_layout.fields[0];
                let value_layout = &element_pair_layout.fields[1];

                self.builder.add_map_new_from_slice(
                    ctx.addr(),
                    slice_region.addr,
                    key_layout.size,
                    value_layout.size,
                    element_pair_layout.total_size,
                    node,
                    "create map from temporary slice pair",
                );

                Ok(GeneratedExpressionResult::default())
            }

            _ => {
                let (self_arg, maybe_self_type) = if arguments.is_empty() {
                    (None, None)
                } else {
                    let self_region =
                        self.gen_expression_location_mut_ref_or_immutable(&arguments[0])?;
                    (Some(self_region), Some(arguments[0].ty().clone()))
                };
                let rest_args = if arguments.len() > 1 {
                    &arguments[1..]
                } else {
                    &vec![]
                };
                self.gen_single_intrinsic_call_with_self(
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
    pub fn gen_single_intrinsic_call_with_self(
        &mut self,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        self_type: Option<Type>,
        self_addr: Option<FrameMemoryRegion>,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        match intrinsic_fn {
            IntrinsicFunction::RuntimePanic => {
                self.builder
                    .add_panic(self_addr.unwrap().addr(), node, "intrinsic panic");
            }

            // Bool
            IntrinsicFunction::BoolToString => self.builder.bool_to_string(
                ctx.addr(),
                self_addr.unwrap().addr(),
                node,
                "bool_to_string",
            ),

            // Fixed
            IntrinsicFunction::FloatRound => self.builder.add_float_round(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float round",
            ),
            IntrinsicFunction::FloatFloor => self.builder.add_float_floor(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float floor",
            ),
            IntrinsicFunction::FloatSqrt => {
                self.builder
                    .add_float_sqrt(ctx.addr(), self_addr.unwrap().addr, node, "float sqr");
            }
            IntrinsicFunction::FloatSign => {
                self.builder.add_float_sign(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    node,
                    "float sign",
                );
            }
            IntrinsicFunction::FloatAbs => {
                self.builder
                    .add_float_abs(ctx.addr(), self_addr.unwrap().addr, node, "float abs");
            }
            IntrinsicFunction::FloatRnd => {
                self.builder.add_float_prnd(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    node,
                    "float pseudo random",
                );
            }
            IntrinsicFunction::FloatCos => {
                self.builder
                    .add_float_cos(ctx.addr(), self_addr.unwrap().addr, node, "float cos");
            }
            IntrinsicFunction::FloatSin => {
                self.builder
                    .add_float_sin(ctx.addr(), self_addr.unwrap().addr, node, "float sin");
            }
            IntrinsicFunction::FloatAcos => {
                self.builder
                    .add_float_acos(ctx.addr(), self_addr.unwrap().addr, node, "float acos")
            }
            IntrinsicFunction::FloatAsin => {
                self.builder
                    .add_float_asin(ctx.addr(), self_addr.unwrap().addr, node, "float asin")
            }
            IntrinsicFunction::FloatAtan2 => self.builder.add_float_atan2(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float atan2",
            ),
            IntrinsicFunction::FloatMin => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.gen_expression_location(float_arg_expr)?;
                self.builder.add_float_min(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    float_region.addr,
                    node,
                    "float min",
                );
            }
            IntrinsicFunction::FloatMax => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.gen_expression_location(float_arg_expr)?;
                self.builder.add_float_max(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    float_region.addr,
                    node,
                    "float max",
                );
            }
            IntrinsicFunction::FloatClamp => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.gen_expression_location(float_arg_expr)?;

                let float_b = &arguments[1];
                let MutRefOrImmutableExpression::Expression(float_b_expr) = float_b else {
                    panic!();
                };
                let float_b_region = self.gen_expression_location(float_b_expr)?;

                self.builder.add_float_clamp(
                    ctx.addr(),
                    float_region.addr,
                    self_addr.unwrap().addr,
                    float_b_region.addr,
                    node,
                    "float round",
                );
            }
            IntrinsicFunction::FloatToString => self.builder.float_to_string(
                ctx.addr(),
                self_addr.unwrap().addr(),
                node,
                "float_to_string",
            ),

            // Int
            IntrinsicFunction::IntAbs => {
                self.builder
                    .add_int_abs(ctx.addr(), self_addr.unwrap().addr, node, "int abs");
            }

            IntrinsicFunction::IntRnd => {
                self.builder.add_int_rnd(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    node,
                    "int pseudo random",
                );
            }
            IntrinsicFunction::IntMax => {
                self.builder
                    .add_int_max(ctx.addr(), self_addr.unwrap().addr, node, "int max");
            }
            IntrinsicFunction::IntMin => {
                self.builder
                    .add_int_min(ctx.addr(), self_addr.unwrap().addr, node, "int min");
            }
            IntrinsicFunction::IntClamp => {
                self.builder
                    .add_int_clamp(ctx.addr(), self_addr.unwrap().addr, node, "int clamp");
            }
            IntrinsicFunction::IntToFloat => self.builder.add_int_to_float(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "int to float",
            ),
            IntrinsicFunction::IntToString => self.builder.add_int_to_string(
                ctx.addr(),
                self_addr.unwrap().addr(),
                node,
                "int_to_string",
            ),

            // String
            IntrinsicFunction::StringLen => {
                self.builder.add_mov32(
                    ctx.addr(),
                    self_addr.unwrap().addr + MemoryOffset(VECTOR_LENGTH_OFFSET),
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
                let key_region = self.gen_expression_location(key_expr)?;
                self.builder.add_vec_push(
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    node,
                    "vec push",
                );
            }
            IntrinsicFunction::VecPop => {
                self.builder.add_vec_pop(
                    ctx.addr(),
                    self_addr.unwrap().addr, // mut self
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
                let index_region = self.gen_expression_location(index_expr)?;
                self.builder.add_vec_remove_index(
                    self_addr.unwrap().addr,
                    index_region.addr,
                    node,
                    "remove index",
                );
            }
            IntrinsicFunction::VecRemoveIndexGetValue => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.gen_expression_location(key_expr)?;
                self.builder.add_vec_remove_index_get_value(
                    ctx.addr(),
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    node,
                    "vec remove index get value",
                );
            }
            IntrinsicFunction::VecClear => {
                self.builder.add_vec_clear(
                    self_addr.unwrap().addr, // mut self
                    node,
                    "vec clear",
                );
            }
            IntrinsicFunction::VecGet => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.gen_expression_location(key_expr)?;
                self.builder.add_vec_get(
                    ctx.addr(),
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    node,
                    "vec get",
                );
            }
            IntrinsicFunction::VecCreate => {
                self.builder
                    .add_vec_create(ctx.addr(), MemorySize(0), node, "vec create"); // TODO: Fix to have proper element memory size
            }
            IntrinsicFunction::VecSubscript => {
                let maybe_index_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.gen_expression_location(index_expr)?;
                self.builder.add_vec_subscript(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    index_region.addr,
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
                let index_region = self.gen_expression_location(index_expr)?;
                // TODO:

                /*
                let source_argument = &arguments[1];
                let MutRefOrImmutableExpression::Expression(value_expr) = source_argument else {
                    panic!();
                };

                let value_region = self.gen_expression_for_access(value_expr)?;
                 */
                let value_region = index_region;

                self.builder.add_vec_subscript_mut(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    index_region.addr,
                    value_region.addr,
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
                let range_header_region = self.gen_expression_location(range_expr)?;
                assert_eq!(ctx.target_size(), RANGE_HEADER_SIZE);
                self.builder.add_vec_get_range(
                    ctx.addr(),
                    self_addr.unwrap().addr,  // mut self (string header)
                    range_header_region.addr, // range x..=y
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

            IntrinsicFunction::VecLen => self.builder.add_mov32(
                ctx.addr(),
                self_addr.unwrap().addr + MemoryOffset(VECTOR_LENGTH_OFFSET),
                node,
                "get the vec length",
            ),
            IntrinsicFunction::VecAny => todo!(), // Low prio
            IntrinsicFunction::VecAll => todo!(), // Low prio
            IntrinsicFunction::VecMap => todo!(), // Low prio
            IntrinsicFunction::VecFilterMap => todo!(), // Low prio
            IntrinsicFunction::VecSwap => {
                let index_a = self.gen_for_access_or_location(&arguments[0])?;
                let index_b = self.gen_for_access_or_location(&arguments[1])?;
                self.builder.add_vec_swamp(
                    self_addr.unwrap().addr,
                    index_a.addr,
                    index_b.addr,
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
                    self_addr.unwrap(),
                    &self_type.unwrap(),
                    &arguments[0],
                    ctx,
                )?;
                //self.builder.patch_jump_here(transformer_patch_position);
            }

            IntrinsicFunction::VecFind => {
                //TODO:
            }

            // Map
            IntrinsicFunction::MapCreate => {
                // TODO:
            }
            IntrinsicFunction::MapHas => {
                self.builder
                    .add_map_has(self_addr.unwrap().addr, node, "map_has");
            }
            IntrinsicFunction::MapRemove => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                self.gen_intrinsic_map_remove(self_addr.unwrap(), key_argument, ctx)?;
            }
            IntrinsicFunction::MapIter => {
                // Never called directly
            }
            IntrinsicFunction::MapIterMut => {
                // Never called directly
            }
            IntrinsicFunction::MapLen => {
                self.builder.add_mov32(
                    ctx.addr(),
                    self_addr.unwrap().addr + MAP_LENGTH_OFFSET,
                    node,
                    "map len",
                );
            }
            IntrinsicFunction::MapSubscript => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.gen_expression_location(key_argument)?;
                self.builder.add_map_subscript(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    key.addr,
                    node,
                    "map_subscript",
                );
            }
            IntrinsicFunction::MapSubscriptMut => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.gen_expression_location(key_argument)?;
                self.builder.add_map_subscript_mut(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    key.addr,
                    node,
                    "map_subscript",
                );
            }
            IntrinsicFunction::MapSubscriptMutCreateIfNeeded => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.gen_expression_location(key_argument)?;
                self.builder.add_map_subscript_mut_create(
                    self_addr.unwrap().addr,
                    key.addr,
                    node,
                    "map_subscript_mut_create (set)",
                );
            }

            // Grid
            IntrinsicFunction::GridCreate => {
                /*
                self.state.builder.add_sparse_create(
                    ctx.addr(),
                    "map_subscript_mut_create (set)",
                );

                 */
            }

            IntrinsicFunction::GridSet => {
                /*
                let MutRefOrImmutableExpression::Expression(x_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let x_region = self.gen_expression_for_access(x_argument)?;
                let MutRefOrImmutableExpression::Expression(y_argument) = &arguments[1] else {
                    panic!("must be expression for key");
                };
                let y_region = self.gen_expression_for_access(y_argument)?;

                let MutRefOrImmutableExpression::Expression(value) = &arguments[2] else {
                    panic!("must be expression for key");
                };
                let value_region = self.gen_expression_for_access(value)?;

                self.state.builder.add_grid_set(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    x_region.addr,
                    y_region.addr,
                    value_region.addr,
                    "grid_get",
                );

                 */
            }
            IntrinsicFunction::GridGet => {
                /*
                let MutRefOrImmutableExpression::Expression(x_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let x_region = self.gen_expression_for_access(x_argument)?;
                let MutRefOrImmutableExpression::Expression(y_argument) = &arguments[1] else {
                    panic!("must be expression for key");
                };
                let y_region = self.gen_expression_for_access(y_argument)?;
                self.state.builder.add_grid_get(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    x_region.addr,
                    y_region.addr,
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
                    ctx.addr(),
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

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_intrinsic_map_remove(
        &mut self,
        map_region: FrameMemoryRegion,
        key_expr: &Expression,
        ctx: &Context,
    ) -> Result<(), Error> {
        let key_region = self.gen_expression_location(key_expr)?;

        self.builder
            .add_map_remove(map_region.addr, key_region.addr, &key_expr.node, "");

        Ok(())
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
            self.builder
                .add_call(&found.ip_range.start, node, call_comment);
        } else {
            let patch_position = self.builder.add_call_placeholder(node, call_comment);
            self.state.function_fixups.push(FunctionFixup {
                patch_position,
                fn_id: internal_fn.program_unique_id,
                internal_function_definition: internal_fn.clone(),
            });
        }
    }

    pub fn temp_memory_region_for_type(&mut self, ty: &Type, comment: &str) -> FrameMemoryRegion {
        let new_target_info = reserve_space_for_type(ty, &mut self.temp_allocator);
        //        info!(?new_target_info, ?self.temp_allocator, ?ty, "RESERVING SPACE");
        new_target_info
    }

    pub fn temp_space_for_type(&mut self, ty: &Type, comment: &str) -> Context {
        Context::new(self.temp_memory_region_for_type(ty, comment))
    }

    /// # Panics
    ///
    #[allow(clippy::single_match_else)]
    pub fn gen_expression_location(
        &mut self,
        expr: &Expression,
    ) -> Result<FrameMemoryRegion, Error> {
        let (region, _gen_result) = self.gen_expression_location_internal(expr)?;

        Ok(region)
    }

    /// # Panics
    ///
    #[allow(clippy::single_match_else)]
    pub fn gen_expression_location_internal(
        &mut self,
        expr: &Expression,
    ) -> Result<(FrameMemoryRegion, GeneratedExpressionResult), Error> {
        match &expr.kind {
            ExpressionKind::VariableAccess(var_ref) => {
                let frame_address = self
                    .variable_offsets
                    .get(&var_ref.unique_id_within_function)
                    .unwrap();

                return Ok((*frame_address, GeneratedExpressionResult::default()));
            }

            _ => {}
        }

        let temp_ctx = self.temp_space_for_type(&expr.ty, "expression");

        let expression_result = self.gen_expression_materialize(expr, &temp_ctx)?;

        Ok((temp_ctx.target(), expression_result))
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

    pub fn gen_expression_materialize(
        &mut self,
        expr: &Expression,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        //self.debug_node(&expr.node);

        match &expr.kind {
            ExpressionKind::ConstantAccess(constant_ref) => {
                self.gen_constant_access(&expr.node, constant_ref, ctx)
            }
            ExpressionKind::TupleDestructuring(variables, tuple_types, tuple_expression) => {
                self.gen_tuple_destructuring(variables, tuple_types, tuple_expression)
            }
            ExpressionKind::Assignment(target_mut_location_expr, source_expr) => {
                self.gen_assignment(&expr.node, target_mut_location_expr, source_expr)
            }
            ExpressionKind::VariableAccess(variable_ref) => {
                self.gen_variable_access(&expr.node, variable_ref, ctx)
            }
            ExpressionKind::BinaryOp(operator) => self.gen_binary_operator(operator, ctx),
            ExpressionKind::UnaryOp(operator) => self.gen_unary_operator(operator, ctx),
            ExpressionKind::PostfixChain(start, chain) => self.gen_postfix_chain(start, chain, ctx),
            ExpressionKind::VariableDefinition(variable, expression) => {
                self.gen_variable_definition(variable, expression, ctx)
            }
            ExpressionKind::VariableReassignment(variable, expression) => {
                self.gen_variable_reassignment(variable, expression, ctx)
            }
            ExpressionKind::AnonymousStructLiteral(anon_struct) => {
                self.gen_anonymous_struct_literal(anon_struct, &expr.ty, ctx)
            }
            ExpressionKind::Literal(basic_literal) => {
                self.gen_literal(&expr.node, basic_literal, ctx)
            }
            ExpressionKind::Option(maybe_option) => {
                self.gen_option_expression(&expr.node, maybe_option.as_deref(), ctx)
            }
            ExpressionKind::ForLoop(a, b, c) => self.gen_for_loop(&expr.node, a, b, c),
            ExpressionKind::WhileLoop(condition, expression) => {
                self.gen_while_loop(condition, expression, ctx)
            }
            ExpressionKind::Block(expressions) => self.gen_block(expressions, ctx),
            ExpressionKind::Match(match_expr) => self.gen_match(match_expr, ctx),
            ExpressionKind::Guard(guards) => self.gen_guard(guards, ctx),
            ExpressionKind::If(conditional, true_expr, false_expr) => {
                self.gen_if(conditional, true_expr, false_expr.as_deref(), ctx)
            }
            ExpressionKind::When(bindings, true_expr, false_expr) => {
                self.gen_when(bindings, true_expr, false_expr.as_deref(), ctx)
            }
            ExpressionKind::CompoundAssignment(target_location, operator_kind, source_expr) => {
                self.compound_assignment(target_location, operator_kind, source_expr)
            }
            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                self.gen_single_intrinsic_call(&expr.node, intrinsic_fn, arguments, ctx)
            }
            ExpressionKind::CoerceOptionToBool(a) => self.gen_coerce_option_to_bool(a, ctx),

            ExpressionKind::InternalCall(internal, arguments) => {
                self.gen_internal_call(&expr.node, internal, arguments, ctx)
            }
            ExpressionKind::HostCall(host_fn, arguments) => {
                self.gen_host_call(&expr.node, host_fn, arguments, ctx)
            }

            // Low priority
            ExpressionKind::VariableBinding(_, _) => todo!(), // only used for `when` expressions

            // Illegal
            ExpressionKind::Lambda(_vec, _x) => {
                panic!("something went wrong. non-capturing lambdas can not be evaluated")
            }
        }
    }

    fn gen_unary_operator(
        &mut self,
        unary_operator: &UnaryOperator,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let node = &unary_operator.node;
        let result = match &unary_operator.kind {
            UnaryOperatorKind::Not => match &unary_operator.left.ty {
                Type::Bool => {
                    let bool_result = self.gen_expression_to_z_flag(&unary_operator.left)?;
                    bool_result.invert_polarity()
                }
                _ => panic!("unknown not"),
            },
            UnaryOperatorKind::Negate => match &unary_operator.left.ty {
                Type::Int => {
                    let left_source = self.gen_expression_location(&unary_operator.left)?;
                    self.builder
                        .add_neg_i32(ctx.addr(), left_source.addr, node, "negate i32");
                    GeneratedExpressionResult::default()
                }

                Type::Float => {
                    let left_source = self.gen_expression_location(&unary_operator.left)?;
                    self.builder
                        .add_neg_f32(ctx.addr(), left_source.addr, node, "negate f32");
                    GeneratedExpressionResult::default()
                }
                _ => panic!("negate should only be possible on Int and Float"),
            },
        };

        Ok(result)
    }

    fn gen_binary_operator(
        &mut self,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        //info!(left=?binary_operator.left.ty, right=?binary_operator.right.ty, "binary_op");

        let left_source = self.gen_expression_location(&binary_operator.left)?;
        let right_source = self.gen_expression_location(&binary_operator.right)?;

        match &binary_operator.kind {
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                match (&binary_operator.left.ty, &binary_operator.right.ty) {
                    (Type::Bool, Type::Bool) => self.gen_binary_operator_cmp8(
                        left_source,
                        &binary_operator.node,
                        right_source,
                    ),
                    (Type::Int, Type::Int) => self.gen_binary_operator_cmp32(
                        left_source,
                        &binary_operator.node,
                        right_source,
                    ),
                    (Type::Float, Type::Float) => self.gen_binary_operator_cmp32(
                        left_source,
                        &binary_operator.node,
                        right_source,
                    ),
                    (Type::String, Type::String) => self.gen_binary_operator_string_cmp(
                        left_source,
                        &binary_operator.node,
                        right_source,
                    ),
                    (Type::Enum(a), Type::Enum(b)) => self.gen_binary_operator_bytes_cmp(
                        left_source,
                        &binary_operator.node,
                        right_source,
                    ),
                    _ => todo!(),
                }
            }
            _ => match (&binary_operator.left.ty, &binary_operator.right.ty) {
                (Type::Bool, Type::Bool) => self.gen_binary_operator_bool(binary_operator),
                (Type::Int, Type::Int) => self.gen_binary_operator_i32(
                    left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    right_source,
                    ctx,
                ),
                (Type::Float, Type::Float) => self.gen_binary_operator_f32(
                    left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    right_source,
                    ctx,
                ),
                (Type::String, Type::String) => self.gen_binary_operator_string(
                    left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    right_source,
                    ctx,
                ),
                _ => todo!(),
            },
        }
    }

    fn gen_binary_operator_i32(
        &mut self,
        left_source: FrameMemoryRegion,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: FrameMemoryRegion,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let mut kind = GeneratedExpressionResultKind::ZFlagUnmodified;
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder.add_add_i32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "i32 add",
                );
            }

            BinaryOperatorKind::Subtract => self.builder.add_sub_i32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "i32 sub",
            ),
            BinaryOperatorKind::Multiply => {
                self.builder.add_mul_i32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "i32 add",
                );
            }
            BinaryOperatorKind::Divide => self.builder.add_div_i32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "i32 div",
            ),
            BinaryOperatorKind::Modulo => self.builder.add_mod_i32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "i32 mod",
            ),
            BinaryOperatorKind::LogicalOr => todo!(),
            BinaryOperatorKind::LogicalAnd => todo!(),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                self.builder
                    .add_cmp32(left_source.addr(), right_source.addr(), node, "i32 cmp");
                if let BinaryOperatorKind::Equal = binary_operator_kind {
                    kind = GeneratedExpressionResultKind::ZFlagIsTrue;
                } else {
                    kind = GeneratedExpressionResultKind::ZFlagIsInversion;
                }
            }
            BinaryOperatorKind::LessThan => {
                self.builder
                    .add_cmp32(left_source.addr(), right_source.addr(), node, "i32 cmp");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_i32(left_source.addr(), right_source.addr(), node, "i32 le");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_i32(left_source.addr(), right_source.addr(), node, "i32 gt");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_i32(left_source.addr(), right_source.addr(), node, "i32 ge");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
        }

        Ok(GeneratedExpressionResult { kind })
    }

    #[allow(clippy::unnecessary_wraps)]
    fn gen_binary_operator_f32(
        &mut self,
        left_source: FrameMemoryRegion,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: FrameMemoryRegion,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let mut kind = GeneratedExpressionResultKind::ZFlagUnmodified;
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder.add_add_f32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 add",
                );
            }

            BinaryOperatorKind::Subtract => self.builder.add_sub_f32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "f32 sub",
            ),
            BinaryOperatorKind::Multiply => {
                self.builder.add_mul_f32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 add",
                );
            }
            BinaryOperatorKind::Divide => {
                self.builder.add_div_f32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 div",
                );
            }
            BinaryOperatorKind::Modulo => self.builder.add_mod_f32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "f32 mod",
            ),
            BinaryOperatorKind::LogicalOr => panic!("not supported"),
            BinaryOperatorKind::LogicalAnd => panic!("not supported"),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                self.builder
                    .add_cmp32(left_source.addr(), right_source.addr(), node, "f32 eq");
                if let BinaryOperatorKind::Equal = binary_operator_kind {
                    kind = GeneratedExpressionResultKind::ZFlagIsTrue;
                } else {
                    kind = GeneratedExpressionResultKind::ZFlagIsInversion;
                }
            }
            BinaryOperatorKind::LessThan => {
                self.builder
                    .add_lt_f32(left_source.addr(), right_source.addr(), node, "f32 lt");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_f32(left_source.addr(), right_source.addr(), node, "f32 le");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_f32(left_source.addr(), right_source.addr(), node, "f32 gt");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_f32(left_source.addr(), right_source.addr(), node, "f32 ge");
                kind = GeneratedExpressionResultKind::ZFlagIsTrue;
            }
        }

        Ok(GeneratedExpressionResult { kind })
    }

    fn gen_binary_operator_string(
        &mut self,
        left_source: FrameMemoryRegion,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: FrameMemoryRegion,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder.add_string_append(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "string add",
                );
            }

            BinaryOperatorKind::Equal => todo!(),
            BinaryOperatorKind::NotEqual => todo!(),
            _ => panic!("illegal string operator"),
        }

        Ok(GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagUnmodified,
        })
    }

    fn gen_binary_operator_bytes_cmp(
        &mut self,
        left_source: FrameMemoryRegion,
        node: &Node,
        right_source: FrameMemoryRegion,
    ) -> Result<GeneratedExpressionResult, Error> {
        self.builder.add_cmp(
            left_source.addr,
            right_source.addr,
            left_source.size,
            &node,
            "compare enum",
        );

        Ok(GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagUnmodified,
        })
    }

    fn gen_binary_operator_cmp8(
        &mut self,
        left_source: FrameMemoryRegion,
        node: &Node,
        right_source: FrameMemoryRegion,
    ) -> Result<GeneratedExpressionResult, Error> {
        self.builder
            .add_cmp8(left_source.addr, right_source.addr, &node, "compare bool");

        Ok(GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagUnmodified,
        })
    }

    fn gen_binary_operator_cmp32(
        &mut self,
        left_source: FrameMemoryRegion,
        node: &Node,
        right_source: FrameMemoryRegion,
    ) -> Result<GeneratedExpressionResult, Error> {
        self.builder
            .add_cmp32(left_source.addr, right_source.addr, &node, "compare bool");

        Ok(GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagIsTrue,
        })
    }

    fn gen_binary_operator_string_cmp(
        &mut self,
        left_source: FrameMemoryRegion,
        node: &Node,
        right_source: FrameMemoryRegion,
    ) -> Result<GeneratedExpressionResult, Error> {
        self.builder
            .add_cmp8(left_source.addr, right_source.addr, &node, "compare bool");

        Ok(GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagIsTrue,
        })
    }

    fn gen_binary_operator_bool(
        &mut self,
        binary_operator: &BinaryOperator,
    ) -> Result<GeneratedExpressionResult, Error> {
        let node = &binary_operator.node;

        let mut kind = GeneratedExpressionResultKind::ZFlagIsTrue;

        match binary_operator.kind {
            BinaryOperatorKind::LogicalOr => {
                let z_flag_left =
                    self.gen_expression_to_normalized_z_flag(&binary_operator.left)?;

                let jump_after_patch = self
                    .builder
                    .add_jmp_if_equal_placeholder(node, "skip rhs `or` expression");

                let z_flag_right =
                    self.gen_expression_to_normalized_z_flag(&binary_operator.right)?;

                self.builder.patch_jump_here(jump_after_patch);
            }
            BinaryOperatorKind::LogicalAnd => {
                let z_flag_left =
                    self.gen_expression_to_normalized_z_flag(&binary_operator.left)?;

                let jump_after_patch = self
                    .builder
                    .add_jmp_if_not_equal_placeholder(node, "skip rhs `and` expression");

                let z_flag_right =
                    self.gen_expression_to_normalized_z_flag(&binary_operator.right)?;

                self.builder.patch_jump_here(jump_after_patch);
            }

            _ => {
                panic!("unknown operator {:?}", binary_operator);
            }
        }

        Ok(GeneratedExpressionResult { kind })
    }

    fn gen_condition_context(
        &mut self,
        condition: &BooleanExpression,
    ) -> Result<PatchPosition, Error> {
        let result = self.gen_expression_to_z_flag(&condition.expression)?;

        let jump_on_false_condition = self.builder.add_jmp_if_not_equal_polarity_placeholder(
            &result.polarity(),
            &condition.expression.node,
            "jump boolean condition false",
        );

        Ok(jump_on_false_condition)
    }

    fn gen_expression_to_z_flag(
        &mut self,
        condition: &Expression,
    ) -> Result<GeneratedExpressionResult, Error> {
        match &condition.kind {
            ExpressionKind::CoerceOptionToBool(option_union_expr) => {
                let region = self.gen_expression_location(option_union_expr)?;
                // We can shortcut this, since we know that the tag location is basically a bool value
                self.builder.add_tst8(
                    region.addr,
                    &option_union_expr.node,
                    "shortcut directly to z-flag",
                );
                return Ok(GeneratedExpressionResult {
                    kind: GeneratedExpressionResultKind::ZFlagIsTrue,
                });
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
            self.gen_expression_location_internal(condition)?;

        if gen_result.kind == GeneratedExpressionResultKind::ZFlagUnmodified {
            self.builder.add_tst8(
                frame_memory_region.addr,
                &condition.node,
                "convert to boolean expression (update z flag)",
            );
            gen_result.kind = GeneratedExpressionResultKind::ZFlagIsTrue;
        }

        Ok(gen_result)
    }

    fn gen_expression_to_normalized_z_flag(
        &mut self,
        condition: &Expression,
    ) -> Result<GeneratedExpressionResult, Error> {
        let result = self.gen_expression_to_z_flag(condition)?;
        assert_ne!(result.kind, GeneratedExpressionResultKind::ZFlagUnmodified);

        if result.kind == GeneratedExpressionResultKind::ZFlagIsInversion {
            self.builder
                .add_not_z(&condition.node, "normalized z is required");
        }

        Ok(GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::ZFlagIsTrue,
        })
    }

    fn gen_boolean_expression_z_flag(
        &mut self,
        condition: &BooleanExpression,
    ) -> Result<GeneratedExpressionResult, Error> {
        self.gen_expression_to_z_flag(&condition.expression)
    }

    fn gen_if(
        &mut self,
        condition: &BooleanExpression,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let jump_on_false_condition = self.gen_condition_context(condition)?;

        // True expression just takes over our target
        // Both to reuse the current target, and for the fact when there is no else
        self.gen_expression_materialize(true_expr, ctx)?;

        if let Some(false_expr) = maybe_false_expr {
            // we need to help the true expression to jump over false
            let skip_false_if_true = self
                .builder
                .add_jump_placeholder(&condition.expression.node, "condition is false skip");

            // If the expression was false, it should continue here
            self.builder.patch_jump_here(jump_on_false_condition);

            // Else expression also can just take over our if target
            self.gen_expression_materialize(false_expr, ctx)?;

            self.builder.patch_jump_here(skip_false_if_true);
        } else {
            self.builder.patch_jump_here(jump_on_false_condition);
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_while_loop(
        &mut self,
        condition: &BooleanExpression,
        expression: &Expression,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        // `while` loops are only for side effects, make sure that the target size is zero (Unit)
        assert_eq!(ctx.target_size().0, 0);

        let ip_for_condition = self.builder.position();

        let jump_on_false_condition = self.gen_condition_context(condition)?;

        // Expression is only for side effects
        let unit_ctx = self.temp_space_for_type(&Type::Unit, "while body expression");
        self.gen_expression_materialize(expression, &unit_ctx)?;

        // Always jump to the condition again to see if it is true
        self.builder
            .add_jmp(ip_for_condition, &expression.node, "jmp to while condition");

        self.builder.patch_jump_here(jump_on_false_condition);

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_location_argument(
        &mut self,
        argument: &SingleLocationExpression,
        ctx: &Context,
        comment: &str,
    ) -> Result<(), Error> {
        let region = self.gen_lvalue_address(argument)?;

        self.builder.add_mov(
            ctx.addr(),
            region.addr,
            region.size,
            &argument.node,
            comment,
        );

        Ok(())
    }

    fn gen_variable_assignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let target_relative_frame_pointer = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| {
                panic!(
                    "could not find id {} {}",
                    variable.unique_id_within_function, variable.assigned_name
                )
            });

        let init_ctx =
            ctx.with_target(*target_relative_frame_pointer, "variable assignment target");

        let _ = self.gen_expression_materialize(expression, &init_ctx)?;

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_variable_binding(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) -> Result<(), Error> {
        let target_relative_frame_pointer = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| panic!("{}", variable.assigned_name));

        let init_ctx =
            ctx.with_target(*target_relative_frame_pointer, "variable assignment target");

        self.gen_mut_or_immute(mut_or_immutable_expression, &init_ctx)
    }

    fn gen_assignment(
        &mut self,
        node: &Node,
        lhs: &TargetAssignmentLocation,
        rhs: &Expression,
    ) -> Result<GeneratedExpressionResult, Error> {
        let lhs_addr = self.gen_lvalue_address(&lhs.0)?;
        let access = self.gen_expression_location(rhs)?;

        self.builder
            .add_mov(lhs_addr.addr, access.addr, access.size, node, "assignment");

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_variable_definition(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        self.gen_variable_assignment(variable, expression, ctx)
    }

    fn gen_variable_reassignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        self.gen_variable_assignment(variable, expression, ctx)
    }

    fn copy_back_mutable_arguments(
        &mut self,
        node: &Node,
        signature: &Signature,
        maybe_self: Option<FrameMemoryRegion>,
        arguments: &Vec<MutRefOrImmutableExpression>,
    ) -> Result<(), Error> {
        let arguments_memory_region = self.infinite_above_frame_size();
        let mut arguments_allocator = ScopeAllocator::new(arguments_memory_region);

        let _argument_addr = reserve(&signature.return_type, &mut arguments_allocator);

        let mut parameters = signature.parameters.clone();
        if let Some(found_self) = maybe_self {
            assert_eq!(signature.parameters[0].name, "self");
            if signature.parameters[0].is_mutable {
                let source_region = reserve(&parameters[0].resolved_type, &mut arguments_allocator);
                self.builder.add_mov(
                    found_self.addr,
                    source_region.addr,
                    source_region.size,
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

            if let MutRefOrImmutableExpression::Location(found_location) = argument {
                let argument_target = self.gen_lvalue_address(found_location)?;
                self.builder.add_mov(
                    argument_target.addr,
                    source_region.addr,
                    source_region.size,
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
        Ok(())
    }
    fn gen_arguments(
        &mut self,
        node: &Node,
        signature: &Signature,
        self_region: Option<FrameMemoryRegion>,
        arguments: &Vec<MutRefOrImmutableExpression>,
    ) -> Result<FrameMemoryRegion, Error> {
        self.argument_allocator.reset();
        // Layout return and arguments, must be continuous space
        let argument_addr = reserve(&signature.return_type, &mut self.argument_allocator);
        //assert_eq!(argument_addr.addr.0, self.frame_size.0);

        let mut argument_targets = Vec::new();
        let mut argument_comments = Vec::new();

        // Layout arguments, must be continuous space
        for (index, type_for_parameter) in signature.parameters.iter().enumerate() {
            let argument_target = reserve(
                &type_for_parameter.resolved_type,
                &mut self.argument_allocator,
            );
            let arg_ctx = Context::new(argument_target);
            argument_targets.push(arg_ctx);
            argument_comments.push(format!("argument {}", type_for_parameter.name));
        }

        if let Some(push_self) = self_region {
            self.builder.add_mov(
                argument_targets[0].addr(),
                push_self.addr,
                push_self.size,
                node,
                "<self>",
            );
            argument_targets.remove(0);
        }

        for ((argument_target_ctx, argument_expr_or_loc), argument_comment) in argument_targets
            .iter()
            .zip(arguments)
            .zip(argument_comments)
        {
            let debug_addr = argument_target_ctx.target().addr();
            self.gen_argument(
                argument_expr_or_loc,
                &argument_target_ctx,
                &argument_comment,
            )?;
        }

        let memory_size = argument_targets
            .last()
            .map_or(MemorySize(0), |last_target| {
                MemorySize(
                    last_target.addr().add(last_target.target_size()).0
                        - argument_targets[0].addr().0,
                )
            });

        let start_addr = argument_targets
            .first()
            .map_or(FrameMemoryAddress(0), |first| first.addr());

        Ok(FrameMemoryRegion {
            addr: start_addr,
            size: memory_size,
        })
    }

    #[allow(clippy::too_many_lines)]
    fn gen_postfix_chain(
        &mut self,
        start_expression: &StartOfChain,
        chain: &[Postfix],
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        /*
        if let ExpressionKind::InternalFunctionAccess(internal_fn) = &start_expression.kind {
            if chain.len() == 1 {
                if let PostfixKind::FunctionCall(args) = &chain[0].kind {
                    if let Some(intrinsic_fn) = single_intrinsic_fn(&internal_fn.body) {
                        let maybe_self = self.gen_expression_for_access(start_expression)?;
                        self.gen_single_intrinsic_call(
                            &start_expression.node,
                            intrinsic_fn,
                            Some(maybe_self),
                            args,
                            ctx,
                        )?;
                    } else {
                        self.gen_arguments(
                            &start_expression.node,
                            &internal_fn.signature.signature,
                            None,
                            args,
                        )?;
                        self.state.add_call(
                            &chain[0].node,
                            internal_fn,
                            &format!("frame size: {}", self.frame_size),
                        ); // will be fixed up later
                        let (return_size, _alignment) =
                            type_size_and_alignment(&internal_fn.signature.signature.return_type);
                        if return_size.0 != 0 {
                            self.state.builder.add_mov(
                                ctx.addr(),
                                self.infinite_above_frame_size().addr,
                                return_size,
                                &start_expression.node,
                                "copy the ret value to destination",
                            );
                        }
                        self.copy_back_mutable_arguments(
                            &start_expression.node,
                            &internal_fn.signature.signature,
                            None,
                            args,
                        )?;
                    }

                    return Ok(());
                }
            }
        }

        if let ExpressionKind::ExternalFunctionAccess(external_fn) = &start_expression.kind {
            if chain.len() == 1 {
                if let PostfixKind::FunctionCall(args) = &chain[0].kind {
                    let total_region = self.gen_arguments(
                        &start_expression.node,
                        &external_fn.signature,
                        None,
                        args,
                    )?;
                    self.state.builder.add_host_call(
                        external_fn.id as u16,
                        total_region.size,
                        &start_expression.node,
                        &format!("call external '{}'", external_fn.assigned_name),
                    );
                    let (return_size, _alignment) =
                        type_size_and_alignment(&external_fn.signature.return_type);
                    if return_size.0 != 0 {
                        self.state.builder.add_mov(
                            ctx.addr(),
                            self.infinite_above_frame_size().addr,
                            return_size,
                            &start_expression.node,
                            "copy the ret value to destination",
                        );
                    }

                    return Ok(());
                }
            }
        }

         */

        let mut start_source = self.gen_start_of_chain(start_expression)?;

        for element in chain {
            match &element.kind {
                PostfixKind::StructField(anonymous_struct, field_index) => {
                    let (memory_offset, memory_size, _max_alignment) =
                        Self::get_struct_field_offset(
                            &anonymous_struct.field_name_sorted_fields,
                            *field_index,
                        );
                    start_source = FrameMemoryRegion::new(
                        start_source.addr.advance(memory_offset),
                        memory_size,
                    );
                }
                PostfixKind::MemberCall(function_to_call, arguments) => {
                    match &**function_to_call {
                        Function::Internal(internal_fn) => {
                            if let Some((intrinsic_fn, intrinsic_arguments)) =
                                single_intrinsic_fn(&internal_fn.body)
                            {
                                let merged_arguments = Self::merge_arguments_keep_literals(
                                    arguments,
                                    intrinsic_arguments,
                                );

                                self.gen_single_intrinsic_call_with_self(
                                    &start_expression.node,
                                    intrinsic_fn,
                                    Some(element.ty.clone()),
                                    Some(start_source),
                                    &merged_arguments,
                                    ctx,
                                )?;
                            } else {
                                self.gen_arguments(
                                    &start_expression.node,
                                    &internal_fn.signature.signature,
                                    Some(start_source),
                                    arguments,
                                )?;
                                self.add_call(
                                    &element.node,
                                    internal_fn,
                                    &format!("frame size: {}", self.frame_size),
                                ); // will be fixed up later

                                self.call_post_helper(
                                    &element.node,
                                    &internal_fn.signature.signature,
                                    Some(start_source),
                                    arguments,
                                    ctx,
                                )?;
                            }
                        }
                        Function::External(x) => {}
                        Function::Intrinsic(intr) => {}
                        _ => panic!(
                            "{}",
                            &format!("not supported as a member call {function_to_call:?}")
                        ),
                    }
                }
                PostfixKind::OptionalChainingOperator => {
                    //TODO:
                }
                PostfixKind::NoneCoalescingOperator(_) => {
                    // TODO:
                }
            }
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn call_post_helper(
        &mut self,
        node: &Node,
        signature: &Signature,
        maybe_self: Option<FrameMemoryRegion>,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let (return_size, _alignment) = type_size_and_alignment(&signature.return_type);
        if return_size.0 != 0 {
            self.builder.add_mov(
                ctx.addr(),
                self.infinite_above_frame_size().addr,
                return_size,
                node,
                "copy the return value to destination",
            );
        }

        self.copy_back_mutable_arguments(node, signature, maybe_self, arguments)?;

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_tuple(&mut self, expressions: &[Expression], ctx: &Context) -> Result<(), Error> {
        let mut scope = ScopeAllocator::new(ctx.target());

        for expr in expressions {
            let (memory_size, alignment) = type_size_and_alignment(&expr.ty);
            let start_addr = scope.allocate(memory_size, alignment);
            let element_region = FrameMemoryRegion::new(start_addr, memory_size);
            let element_ctx = Context::new(element_region);
            self.gen_expression_materialize(expr, &element_ctx)?;
        }

        Ok(())
    }

    fn get_struct_field_offset(
        fields: &SeqMap<String, StructTypeField>,
        index_to_find: usize,
    ) -> (MemoryOffset, MemorySize, MemoryAlignment) {
        let mut offset = 0;

        for (index, (_name, field)) in fields.iter().enumerate() {
            let (struct_field_size, struct_field_align) =
                type_size_and_alignment(&field.field_type);
            if index == index_to_find {
                return (MemoryOffset(offset), struct_field_size, struct_field_align);
            }

            offset += struct_field_size.0;
        }

        panic!("field not found");
    }

    fn gen_anonymous_struct(
        &mut self,
        anon_struct_type: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Expression)>,
        base_context: &Context,
    ) -> Result<(), Error> {
        for (field_index, expression) in source_order_expressions {
            let (field_memory_offset, field_size, _field_alignment) = Self::get_struct_field_offset(
                &anon_struct_type.field_name_sorted_fields,
                *field_index,
            );
            let field_ctx = base_context.with_offset(field_memory_offset, field_size);
            self.gen_expression_materialize(expression, &field_ctx)?;
        }

        Ok(())
    }

    fn gen_literal(
        &mut self,
        node: &Node,
        literal: &Literal,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        match literal {
            Literal::IntLiteral(int) => {
                self.builder.add_ld32(ctx.addr(), *int, node, "int literal");
            }
            Literal::FloatLiteral(fixed_point) => {
                self.builder
                    .add_ld32(ctx.addr(), fixed_point.inner(), node, "float literal");
            }
            Literal::NoneLiteral => {
                self.builder.add_ld8(ctx.addr(), 0, node, "none literal");
            }
            Literal::BoolLiteral(truthy) => {
                self.builder
                    .add_ld8(ctx.addr(), u8::from(*truthy), node, "bool literal");
            }

            Literal::EnumVariantLiteral(enum_type, a, b) => {
                let tagged_union = layout_enum_into_tagged_union(
                    &enum_type.assigned_name,
                    &enum_type.variants.values().cloned().collect::<Vec<_>>(),
                );

                let variant_data =
                    tagged_union.get_variant_by_index(a.common().container_index as usize);

                let payload_offset = tagged_union.payload_offset();

                self.builder.add_ld8(
                    ctx.addr(),
                    a.common().container_index,
                    node,
                    &format!("enum variant {} tag", a.common().assigned_name),
                );

                let inner_addr = ctx.addr().add(MemorySize(payload_offset.0));

                let region = FrameMemoryRegion::new(inner_addr, variant_data.ty.total_size);
                let inner_ctx = Context::new(region);

                //layout_union(a)
                match b {
                    EnumLiteralData::Nothing => {}
                    EnumLiteralData::Tuple(expressions) => {
                        self.gen_tuple(expressions, &inner_ctx)?;
                    }
                    EnumLiteralData::Struct(sorted_expressions) => {
                        if let EnumVariantType::Struct(variant_struct_type) = a {
                            self.gen_anonymous_struct(
                                &variant_struct_type.anon_struct,
                                sorted_expressions,
                                &inner_ctx,
                            )?;
                        }
                    }
                }
            }
            Literal::TupleLiteral(_tuple_type, expressions) => self.gen_tuple(expressions, ctx)?,
            Literal::StringLiteral(str) => {
                self.gen_string_literal(node, str, ctx);
            }
            Literal::Slice(slice_type, expressions) => {
                self.gen_slice_literal(node, slice_type, expressions, ctx)?;
            }
            Literal::SlicePair(slice_pair_type, pairs) => {
                self.gen_slice_pair_literal(slice_pair_type, pairs)?;
            }
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_string_literal(&mut self, node: &Node, string: &str, ctx: &Context) {
        let string_bytes = string.as_bytes();
        let string_byte_count = string_bytes.len();

        let data_ptr = self
            .state
            .constants
            .allocate(string_bytes, MemoryAlignment::U8);

        let mem_size = MemorySize(string_byte_count as u16);

        self.builder.add_string_from_constant_slice(
            ctx.addr(),
            data_ptr,
            mem_size,
            node,
            "create string",
        );
        // self.gen_vec_immediate(data_ptr, mem_size, mem_size, "string", ctx);
    }

    /*
    fn gen_vec_immediate(
        &mut self,
        data_ptr: MemoryAddress,
        len: MemorySize,
        capacity: MemorySize,
        comment_prefix: &str,
        ctx: &Context,
    ) {
        self.state
            .builder
            .add_ld_u16(ctx.addr(), len.0, &format!("{} len", comment_prefix));

        self.state.builder.add_ld_u16(
            ctx.addr().add(MemorySize(2)),
            capacity.0,
            &format!("{} capacity", comment_prefix),
        );

        self.state.builder.add_ld_u16(
            ctx.addr().add(MemorySize(4)),
            data_ptr.0,
            &format!("{} ptr", comment_prefix),
        );
    }


     */
    fn gen_option_expression(
        &mut self,
        node: &Node,
        maybe_option: Option<&Expression>,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        if let Some(found_value) = maybe_option {
            self.builder.add_ld8(ctx.addr(), 1, node, "option Some tag"); // 1 signals `Some`
            let (inner_size, inner_alignment) = type_size_and_alignment(&found_value.ty);
            let one_offset_ctx = ctx.with_offset(inner_alignment.into(), inner_size);

            self.gen_expression_materialize(found_value, &one_offset_ctx)?; // Fills in more of the union
        } else {
            self.builder.add_ld8(ctx.addr(), 0, node, "option None tag"); // 0 signals `None`
            // No real need to clear the rest of the memory
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_for_loop_vec(
        &mut self,
        node: &Node,
        for_pattern: &ForPattern,
        collection_expr: &MutRefOrImmutableExpression,
    ) -> Result<(InstructionPosition, PatchPosition), Error> {
        let collection_region = self.gen_for_access_or_location(collection_expr)?;

        let temp_iterator_region = self
            .temp_allocator
            .allocate(VEC_ITERATOR_SIZE, VEC_ITERATOR_ALIGNMENT);
        self.builder.add_vec_iter_init(
            temp_iterator_region,
            collection_region.addr,
            node,
            "initialize vec iterator",
        );

        let loop_ip = self.builder.position();

        let placeholder_position = match for_pattern {
            ForPattern::Single(variable) => {
                let target_variable = self
                    .variable_offsets
                    .get(&variable.unique_id_within_function)
                    .unwrap();
                self.builder.add_vec_iter_next_placeholder(
                    temp_iterator_region,
                    target_variable.addr,
                    node,
                    "move to next or jump over",
                )
            }
            ForPattern::Pair(variable_a, variable_b) => {
                let target_variable_a = self
                    .variable_offsets
                    .get(&variable_a.unique_id_within_function)
                    .unwrap();
                let target_variable_b = self
                    .variable_offsets
                    .get(&variable_b.unique_id_within_function)
                    .unwrap();
                self.builder.add_vec_iter_next_pair_placeholder(
                    temp_iterator_region,
                    target_variable_a.addr,
                    target_variable_b.addr,
                    node,
                    "move to next or jump over",
                )
            }
        };

        Ok((loop_ip, placeholder_position))
    }

    fn gen_for_loop_map(
        &mut self,
        node: &Node,
        for_pattern: &ForPattern,
    ) -> Result<(InstructionPosition, PatchPosition), Error> {
        self.builder.add_map_iter_init(
            FrameMemoryAddress(0x80),
            FrameMemoryAddress(0xffff),
            node,
            "initialize map iterator",
        );

        let jump_ip = self.builder.position();

        let patch_position = match for_pattern {
            ForPattern::Single(_) => self.builder.add_map_iter_next_placeholder(
                FrameMemoryAddress(0x80),
                FrameMemoryAddress(0x16),
                node,
                "move to next or jump over",
            ),
            ForPattern::Pair(_, _) => self.builder.add_map_iter_next_pair_placeholder(
                FrameMemoryAddress(0x80),
                FrameMemoryAddress(0x16),
                FrameMemoryAddress(0x16),
                node,
                "move to next or jump over",
            ),
        };

        Ok((jump_ip, patch_position))
    }

    fn gen_for_loop_range(
        &mut self,
        node: &Node,
        for_pattern: &ForPattern,
    ) -> Result<(InstructionPosition, PatchPosition), Error> {
        self.builder.add_map_iter_init(
            FrameMemoryAddress(0x80),
            FrameMemoryAddress(0xffff),
            node,
            "initialize map iterator",
        );

        let jump_ip = self.builder.position();

        let patch_position = match for_pattern {
            ForPattern::Single(_) => self.builder.add_range_iter_next_placeholder(
                FrameMemoryAddress(0x80),
                FrameMemoryAddress(0x16),
                node,
                "move to next or jump over",
            ),
            ForPattern::Pair(_, _) => self.builder.add_range_iter_next_pair_placeholder(
                FrameMemoryAddress(0x80),
                FrameMemoryAddress(0x16),
                FrameMemoryAddress(0x16),
                node,
                "move to next or jump over",
            ),
        };

        Ok((jump_ip, patch_position))
    }

    fn gen_for_loop(
        &mut self,
        node: &Node,
        for_pattern: &ForPattern,
        iterable: &Iterable,
        closure: &Box<Expression>,
    ) -> Result<GeneratedExpressionResult, Error> {
        // Add check if the collection is empty, to skip everything

        // get some kind of iteration pointer

        // check if it has reached its end

        let collection_type = &iterable.resolved_expression.ty();
        let (jump_ip, placeholder_position) = match collection_type {
            Type::String => {
                todo!();
            }
            Type::NamedStruct(named_type) => {
                //let node = &named_type.name;
                if named_type.is_vec() {
                    self.gen_for_loop_vec(node, for_pattern, &iterable.resolved_expression)?
                } else if named_type.is_map() {
                    self.gen_for_loop_map(node, for_pattern)?
                } else if named_type.is_range() {
                    self.gen_for_loop_range(node, for_pattern)?
                } else if named_type.is_stack() {
                    self.gen_for_loop_range(node, for_pattern)?
                } else if named_type.is_grid() {
                    self.gen_for_loop_range(node, for_pattern)?
                } else {
                    error!(?named_type, "can not iterate this collection");
                    return Err(self.create_err(
                        ErrorKind::NotAnIterableCollection,
                        iterable.resolved_expression.node(),
                    ));
                }
            }
            _ => {
                return Err(self.create_err(
                    ErrorKind::IllegalCollection,
                    iterable.resolved_expression.node(),
                ));
            }
        };

        match for_pattern {
            ForPattern::Single(value_variable) => {}
            ForPattern::Pair(key_variable, value_variable) => {}
        }

        let unit_expr = self.temp_space_for_type(&Type::Unit, "for loop body");
        self.gen_expression_materialize(closure, &unit_expr)?;

        self.builder
            .add_jmp(jump_ip, &Node::default(), "jump to next iteration");
        // advance iterator pointer
        // jump to check if iterator pointer has reached its end
        self.builder.patch_jump_here(placeholder_position);

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_for_loop_for_vec(
        &mut self,
        element_type: &Type,
        vector_expr: Expression,
        ctx: &mut Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        // get the vector that is referenced
        let vector_ctx = self.temp_space_for_type(&vector_expr.ty, "vector space");
        self.gen_expression_materialize(&vector_expr, &vector_ctx)

        /*
        let value_var_addr = match for_pattern {
            ForPattern::Single(value_variable) => self
                .variable_offsets
                .get(&value_variable.unique_id_within_function)
                .expect("Variable not found"),
            ForPattern::Pair(_, _) => {
                panic!("Cannot use key-value pattern with vectors");
            }
        };

         */

        /*
        let element_size = type_size(element_type);
               // Temporary for the counter
               let counter_addr = ctx.allocate_temp(MemorySize(2)); // u16 counter
               self.state
                   .builder
                   .add_ld_u16(counter_addr, 0, "temporary counter");

               let loop_start_pos = self.state.builder.position();

               // vector length
               let length_addr = ctx.allocate_temp(MemorySize(2));
               self.state.builder.add_mov(
                   length_addr,
                   vector_ctx.addr().add(MemorySize(VECTOR_LENGTH_OFFSET)),
                   MemorySize(2),
                   "vector length",
               );

               // Compare counter < length
               let compare_result_addr = ctx.allocate_temp(MemorySize(1)); // boolean result
               self.state.builder.add_lt_u16(
                   compare_result_addr,
                   counter_addr,
                   length_addr,
                   "counter < length",
               );

               // Exit loop if counter >= length
               let exit_jump = self
                   .state
                   .builder
                   .add_conditional_jump_placeholder(compare_result_addr, "counter >= length exit");

               let data_ptr_addr = ctx.allocate_temp(MemorySize(2));
               self.state.builder.add_mov(
                   data_ptr_addr,
                   vector_ctx.addr().add(MemorySize(VECTOR_DATA_PTR_OFFSET)),
                   MemorySize(PTR_SIZE),
                   "copy vector data ptr",
               );


        */
        /*
        let offset_addr = ctx.allocate_temp(2);
        self.state.builder.add_mul_u16(
            offset_addr,
            counter_addr,
            element_size
        );

        self.state.builder.add_ld_indirect(
            *value_var_addr,     // Destination: loop variable
            data_ptr_addr,       // Base: vector's data pointer
            offset_addr,         // Offset: counter * element_size
            element_size         // Size to copy
        );

        let mut body_ctx = ctx.temp_space_for_type(&Type::Unit);
        self.gen_expression(body, &mut body_ctx);

        self.state.builder.add_inc_u16(counter_addr);

        self.state.builder.add_jmp_to_position(loop_start_pos);

        let end_pos = self.state.builder.current_position();
        self.state.builder.patch_jump(exit_jump, end_pos);

         */
    }

    fn gen_block(
        &mut self,
        expressions: &[Expression],
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        if let Some((last, others)) = expressions.split_last() {
            for expr in others {
                let temp_context = self.temp_space_for_type(&Type::Unit, "block target");
                self.gen_expression_materialize(expr, &temp_context)?;
            }
            self.gen_expression_materialize(last, ctx)?;
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn get_variable_region(&self, variable: &VariableRef) -> (FrameMemoryRegion, MemoryAlignment) {
        let frame_address = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap();
        let (_size, align) = type_size_and_alignment(&variable.resolved_type);

        (*frame_address, align)
    }

    fn gen_variable_access(
        &mut self,
        node: &Node, // Variable access node
        variable: &VariableRef,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let (region, alignment) = self.get_variable_region(variable);
        self.builder.add_mov(
            ctx.addr(),
            region.addr,
            region.size,
            node,
            &format!(
                "variable access '{}' ({})",
                variable.assigned_name,
                ctx.comment()
            ),
        );

        Ok(GeneratedExpressionResult::default())
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
    ) -> Result<GeneratedExpressionResult, Error> {
        let target_location = self.gen_lvalue_address(&target_location.0)?;

        let source_info = self.gen_expression_location(source)?;

        let type_to_consider = Self::referenced_or_not_type(&source.ty);

        match &type_to_consider {
            Type::Int => {
                self.gen_compound_assignment_i32(&source.node, &target_location, op, &source_info);
            }
            Type::Float => {
                self.gen_compound_assignment_f32(&source.node, &target_location, op, &source_info);
            }
            Type::String => todo!(),
            _ => return Err(self.create_err(ErrorKind::IllegalCompoundAssignment, &source.node)),
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_compound_assignment_i32(
        &mut self,
        node: &Node,
        target: &FrameMemoryRegion,
        op: &CompoundOperatorKind,
        source_ctx: &FrameMemoryRegion,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.builder.add_add_i32(
                    target.addr(),
                    target.addr(),
                    source_ctx.addr(),
                    node,
                    "+=  (i32)",
                );
            }
            CompoundOperatorKind::Sub => self.builder.add_sub_i32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "-=  (i32)",
            ),
            CompoundOperatorKind::Mul => self.builder.add_mul_i32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "*=  (i32)",
            ),
            CompoundOperatorKind::Div => self.builder.add_div_i32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "/=  (i32)",
            ),
            CompoundOperatorKind::Modulo => self.builder.add_mod_i32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "%=  (i32)",
            ),
        }
    }

    fn gen_compound_assignment_f32(
        &mut self,
        node: &Node,
        target: &FrameMemoryRegion,
        op: &CompoundOperatorKind,
        source_ctx: &FrameMemoryRegion,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.builder.add_add_f32(
                    target.addr(),
                    target.addr(),
                    source_ctx.addr(),
                    node,
                    "+=  (f32)",
                );
            }
            CompoundOperatorKind::Sub => self.builder.add_sub_f32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "-=  (f32)",
            ),
            CompoundOperatorKind::Mul => self.builder.add_mul_f32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "*=  (f32)",
            ),
            CompoundOperatorKind::Div => self.builder.add_div_f32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "/=  (f32)",
            ),
            CompoundOperatorKind::Modulo => self.builder.add_mod_f32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "%=  (f32)",
            ),
        }
    }

    fn internal_function_access(
        &mut self,
        internal: &InternalFunctionDefinitionRef,
        ctx: &Context,
    ) -> Result<(), Error> {
        self.builder.add_ld_u16(
            ctx.addr(),
            internal.program_unique_id,
            &internal.name.0,
            &format!("function access '{}'", internal.assigned_name),
        );
        Ok(())
    }

    fn infinite_above_frame_size(&self) -> FrameMemoryRegion {
        FrameMemoryRegion::new(FrameMemoryAddress(self.frame_size.0), MemorySize(1024))
    }

    fn gen_anonymous_struct_literal(
        &mut self,
        anon_struct_literal: &AnonymousStructLiteral,
        ty: &Type,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let anon_struct_type = match ty {
            Type::NamedStruct(named_struct) => named_struct.anon_struct_type.clone(),
            Type::AnonymousStruct(anon_struct_type) => anon_struct_type.clone(),
            _ => panic!("internal error with struct literal"),
        };

        self.gen_struct_literal_helper(
            &anon_struct_type,
            &anon_struct_literal.source_order_expressions,
            ctx,
        )
    }

    fn gen_struct_literal_helper(
        &mut self,
        struct_type_ref: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Option<Node>, Expression)>,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let struct_type = Type::AnonymousStruct(struct_type_ref.clone());
        let (whole_struct_size, whole_struct_alignment) = type_size_and_alignment(&struct_type);
        if ctx.target_size().0 != whole_struct_size.0 {
            info!("problem");
        }
        assert_eq!(ctx.target_size().0, whole_struct_size.0);

        for (field_index, _node, expression) in source_order_expressions {
            let (field_offset, field_size, field_alignment) =
                struct_field_offset(*field_index, struct_type_ref);
            //info!(?field_offset, ?field_index, "field offset");
            let new_address = ctx.addr().advance(field_offset);
            let field_ctx = Context::new(FrameMemoryRegion::new(new_address, field_size));
            self.gen_expression_materialize(expression, &field_ctx)?;
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_slice_literal(
        &mut self,
        node: &Node,
        slice_type: &Type,
        expressions: &[Expression],
        ctx: &Context,
    ) -> Result<(), Error> {
        let Type::Slice(element_type) = slice_type else {
            panic!("incorrect slice type")
        };

        let (element_size, element_alignment) = type_size_and_alignment(element_type);
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(element_size.0 * element_count);
        assert_eq!(ctx.target_size(), SLICE_HEADER_SIZE);

        let heap_ptr_header_addr = ctx.addr() + SLICE_PTR_OFFSET;

        self.builder.add_alloc(
            heap_ptr_header_addr,
            total_slice_size,
            node,
            "allocate slice",
        );

        let temp_element_address = self
            .temp_allocator
            .allocate(element_size, element_alignment);
        let temp_element_region = FrameMemoryRegion::new(temp_element_address, element_size);
        let element_ctx = Context::new(temp_element_region);

        for (index, expr) in expressions.iter().enumerate() {
            let (expression_size, expression_alignment) = type_size_and_alignment(&expr.ty);
            if expression_size != element_ctx.target_size() {
                error!(?expr.ty, ?element_type, "how can this happen?");
            }
            assert_eq!(expression_size, element_ctx.target_size());

            self.gen_expression_materialize(expr, &element_ctx)?;

            let heap_offset = HeapMemoryOffset((index as u32) * element_size.0 as u32);

            self.builder.add_stx(
                heap_ptr_header_addr,
                heap_offset,
                element_ctx.addr(),
                FrameMemorySize(element_ctx.target_size().0),
                node,
                "copy slice element",
            );
        }

        let slice_header_byte_count_addr = ctx.addr() + SLICE_COUNT_OFFSET;
        self.builder.add_ld32(
            slice_header_byte_count_addr,
            element_count as i32,
            node,
            "set slice element count",
        );

        Ok(())
    }

    fn gen_slice_pair_literal(
        &mut self,
        slice_type: &Type,
        expressions: &[(Expression, Expression)],
    ) -> Result<SlicePairInfo, Error> {
        let Type::SlicePair(key_type, value_type) = slice_type else {
            panic!("should have been slice pair type")
        };

        //let constructed_tuple = Type::Tuple(vec![*key_type.clone(), *value_type.clone()]);

        let tuple_type_layout = layout_tuple_items(&[*key_type.clone(), *value_type.clone()]);

        let key_layout = &tuple_type_layout.fields[0];
        let value_layout = &tuple_type_layout.fields[1];

        //info!(?key_layout, ?value_layout, "layouts");

        let element_size = tuple_type_layout.total_size;
        let element_alignment = tuple_type_layout.max_alignment;

        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(element_size.0 * element_count);

        let start_frame_address_to_transfer = self
            .temp_allocator
            .allocate(total_slice_size, element_alignment);

        for (index, (key_expr, value_expr)) in expressions.iter().enumerate() {
            let memory_offset = MemoryOffset((index as u16) * element_size.0);
            let key_region = FrameMemoryRegion::new(
                start_frame_address_to_transfer.advance(memory_offset),
                key_layout.size,
            );
            let key_ctx = Context::new(key_region);
            self.gen_expression_materialize(key_expr, &key_ctx)?;

            //.advance(memory_offset.add(value_layout.offset.as_size(), tuple_type_layout.total_alignment)),
            let value_region = FrameMemoryRegion::new(
                start_frame_address_to_transfer + value_layout.offset,
                value_layout.size,
            );
            let value_ctx = Context::new(value_region);
            self.gen_expression_materialize(value_expr, &value_ctx)?;
        }

        Ok(SlicePairInfo {
            addr: TempFrameMemoryAddress(start_frame_address_to_transfer),
            key_size: key_layout.size,
            value_size: value_layout.size,
            element_count: CountU16(element_count),
            element_size,
        })
    }

    fn gen_slice_helper(
        &mut self,
        node: &Node,
        start_temp_frame_address_to_transfer: FrameMemoryAddress,
        element_count: u16,
        element_size: MemorySize,
        ctx: &Context,
    ) {
        let total_slice_size = MemorySize(element_size.0 * element_count);
        let vec_len_addr = ctx.addr().advance(MemoryOffset(0));
        self.builder
            .add_ld_u16(vec_len_addr, element_count, node, "slice len");

        let vec_capacity_addr = ctx.addr().advance(MemoryOffset(2));
        self.builder
            .add_ld_u16(vec_capacity_addr, element_count, node, "slice capacity");

        let vec_element_size_addr = ctx.addr().advance(MemoryOffset(4));
        self.builder.add_ld_u16(
            vec_element_size_addr,
            element_size.0,
            node,
            "slice element size",
        );

        /*
        let allocated_vec_address = ctx.addr().advance(MemoryOffset(6));
        self.state
        .builder
        add_alloc(allocated_vec_address, total_slice_size, "slice literal");

        self.state.builder.add_stx(
            allocated_vec_address,
            MemoryOffset(0),
            start_temp_frame_address_to_transfer,
            total_slice_size,
            "copy from slice continuous temporary frame memory to allocated vec ptr heap area",
        );

         */
    }

    /*
    fn gen_intrinsic_call_ex(
        &mut self,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> Result<(), Error> {
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
            IntrinsicFunction::VecFromSlice => self.gen_intrinsic_vec_from_slice(arguments, ctx),
            IntrinsicFunction::VecPush => todo!(),
            IntrinsicFunction::VecPop => todo!(),
            IntrinsicFunction::VecFor => todo!(),
            IntrinsicFunction::VecWhile => todo!(),
            IntrinsicFunction::VecFindMap => todo!(),
            IntrinsicFunction::VecRemoveIndex => todo!(),
            IntrinsicFunction::VecRemoveIndexGetValue => todo!(),
            IntrinsicFunction::VecClear => todo!(),
            IntrinsicFunction::VecCreate => {
                self.gen_intrinsic_vec_create(arguments);
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

    fn gen_intrinsic_vec_create(&self, arguments: &Vec<MutRefOrImmutableExpression>) {
        for arg in arguments {
            info!(?arg, "argument");
        }
    }

    fn gen_intrinsic_vec_from_slice(
        &mut self,
        node: &Node,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) -> Result<(), Error> {
        if let MutRefOrImmutableExpression::Expression(found_expr) = &arguments[0] {
            let memory = self.gen_expression_location(found_expr)?;
            self.builder.add_vec_from_slice(
                ctx.addr(),
                memory.addr,
                MemorySize(0),
                node,
                "create vec",
            );
        } else {
            panic!("vec_from_slice");
        }

        Ok(())
    }

    fn gen_match(
        &mut self,
        match_expr: &Match,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let region_to_match = self.gen_for_access_or_location(&match_expr.expression)?;

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
                            region_to_match.addr,
                            enum_variant.common().container_index,
                            match_expr.expression.node(),
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
                    match_expr.expression.node(),
                    "placeholder for enum match",
                ))
            } else {
                None
            };

            let maybe_guard_skip = if let Some(guard) = maybe_guard {
                Some(self.gen_condition_context(guard)?)

            //                Some(self.builder.add_jmp_if_not_equal_polarity_placeholder(
            //                  &polarity.polarity(),
            //                match_expr.expression.node(),
            //              "placeholder for skip guard",
            //        ))
            } else {
                None
            };

            self.gen_expression_materialize(&arm.expression, ctx)?;

            if !is_last {
                let jump_to_exit_placeholder = self
                    .builder
                    .add_jump_placeholder(&arm.expression.node, "jump to exit");
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

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_guard(
        &mut self,
        guards: &Vec<Guard>,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let mut jump_to_exit_placeholders = Vec::new();
        for guard in guards {
            if let Some(condition) = &guard.condition {
                //                let result = self.gen_boolean_expression_z_flag(condition)?;
                let skip_expression_patch = self.gen_condition_context(condition)?;
                //&result.polarity(),
                //&guard.result.node,
                //"guard condition",
                //);
                self.gen_expression_materialize(&guard.result, ctx)?;
                let jump_to_exit_placeholder = self
                    .builder
                    .add_jump_placeholder(&guard.result.node, "jump to exit");
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
                self.builder.patch_jump_here(skip_expression_patch);
            } else {
                // _ -> wildcard
                self.gen_expression_materialize(&guard.result, ctx)?;
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.builder.patch_jump_here(placeholder);
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_when(
        &mut self,
        bindings: &Vec<WhenBinding>,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let mut all_false_jumps = Vec::new();

        for binding in bindings {
            let (variable_region, _alignment) = self.get_variable_region(&binding.variable);

            let old_variable_region = self.gen_for_access_or_location(&binding.expr)?;

            self.builder.add_tst8(
                old_variable_region.addr,
                binding.expr.node(),
                "check binding",
            );
            let patch = self
                .builder
                .add_jmp_if_not_equal_placeholder(binding.expr.node(), "jump if none");
            all_false_jumps.push(patch);
        }

        // if we are here all bindings are `Some`
        for binding in bindings {
            let (variable_region, alignment) = self.get_variable_region(&binding.variable);

            if binding.has_expression() {
                let var_ctx = Context::new(variable_region);
                self.gen_mut_or_immute(&binding.expr, &var_ctx)?;
            } else {
                let MutRefOrImmutableExpression::Expression(variable_access_expression) =
                    &binding.expr
                else {
                    panic!("must be expression");
                };
                let old_variable_region =
                    self.gen_expression_location(variable_access_expression)?;
                let alignment_offset: MemoryOffset = alignment.into();
                let some_value_region = FrameMemoryRegion::new(
                    old_variable_region.addr.advance(alignment_offset),
                    MemorySize(variable_region.size.0),
                );
                self.builder.add_movlp(
                    variable_region.addr,
                    some_value_region.addr,
                    some_value_region.size,
                    binding.expr.node(),
                    "move from Some to value",
                );
            }
        }

        self.gen_expression_materialize(true_expr, ctx)?;
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
            self.gen_expression_materialize(else_expr, ctx)?;
            self.builder.patch_jump_here(maybe_jump_over_false.unwrap());
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn create_err(&mut self, kind: ErrorKind, node: &Node) -> Error {
        error!(?kind, "encountered error");
        Error {
            kind,
            node: node.clone(),
        }
    }

    fn gen_tuple_destructuring(
        &mut self,
        target_variables: &[VariableRef],
        tuple_type: &[Type],
        source_tuple_expression: &Expression,
    ) -> Result<GeneratedExpressionResult, Error> {
        let source_region = self.gen_expression_location(source_tuple_expression)?;

        let tuple_type = layout_tuple_items(tuple_type);
        assert_eq!(tuple_type.total_size.0, source_region.size.0);

        for (target_variable, offset_item) in target_variables.iter().zip(tuple_type.fields) {
            if target_variable.is_unused {
            } else {
                let (target_region, _variable_alignment) =
                    self.get_variable_region(target_variable);
                assert_eq!(target_region.size.0, offset_item.size.0);

                let source_element_region = FrameMemoryRegion::new(
                    source_region.addr.advance(offset_item.offset),
                    offset_item.size,
                );
                self.builder.add_mov(
                    target_region.addr,
                    source_element_region.addr,
                    source_element_region.size,
                    &target_variable.name,
                    &format!(
                        "destructuring to variable {}",
                        target_variable.assigned_name
                    ),
                );
            }
        }

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_constant_access(
        &mut self,
        node: &Node,
        constant_reference: &ConstantRef,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        //info!(?constant_reference, "looking up constant");
        let constant_region = self
            .state
            .constant_offsets
            .get(&constant_reference.id)
            .unwrap();
        assert_eq!(constant_region.size.0, ctx.target_size().0);

        self.builder.add_mov_mem(
            ctx.addr(),
            constant_region.addr,
            constant_region.size,
            node,
            &format!("load constant '{}'", constant_reference.assigned_name),
        );

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_coerce_option_to_bool(
        &mut self,
        expr: &Expression,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let region = self.gen_expression_location(expr)?;
        self.builder.add_mov(
            ctx.addr(),
            region.addr,
            MemorySize(1),
            &expr.node,
            "move option tag to bool",
        );

        Ok(GeneratedExpressionResult::default())
    }

    fn gen_start_of_chain(&mut self, start: &StartOfChain) -> Result<FrameMemoryRegion, Error> {
        match &start.kind {
            StartOfChainKind::Expression(expr) => self.gen_expression_location(expr),
            StartOfChainKind::Variable(variable) => {
                let (x, y) = self.get_variable_region(variable);
                Ok(x)
            }
        }
    }

    fn gen_internal_call(
        &mut self,
        node: &Node,
        internal_fn: &InternalFunctionDefinitionRef,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        self.gen_arguments(node, &internal_fn.signature.signature, None, arguments)?;

        self.add_call(
            node,
            internal_fn,
            &format!("frame size: {}", self.frame_size),
        ); // will be fixed up later

        self.call_post_helper(node, &internal_fn.signature.signature, None, arguments, ctx)
    }

    fn gen_host_call(
        &mut self,
        node: &Node,
        host_fn: &ExternalFunctionDefinitionRef,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let memory_region = self.gen_arguments(node, &host_fn.signature, None, arguments)?;

        self.builder.add_host_call(
            host_fn.id as u16,
            memory_region.size,
            node,
            &format!("host call frame size: {}", self.frame_size),
        ); // will be fixed up later

        self.call_post_helper(node, &host_fn.signature, None, arguments, ctx)
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
    pub fn iterate_over_collection_with_lambda(
        &mut self,
        node: &Node,
        source_collection_type: Collection,
        transformer: Transformer,
        source_collection_self_region: FrameMemoryRegion,
        source_collection_analyzed_type: &Type,
        lambda_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) -> Result<(), Error> {
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
                *self
                    .variable_offsets
                    .get(&x.unique_id_within_function)
                    .unwrap()
            })
            .collect();

        // Primary is the right most variable
        let primary_variable = target_variables[target_variables.len() - 1];

        let lambda_return_analyzed_type = &lambda_expr.ty;

        // 1. Optionally initialize the result vector if the transformer produces one.
        let lambda_return_gen_type = layout_type(lambda_return_analyzed_type, "for_iterator");

        if matches!(
            transformer.return_type(),
            TransformerResult::VecWithLambdaResult | TransformerResult::VecFromSourceCollection
        ) {
            let element_size_in_target_vec = match transformer.return_type() {
                TransformerResult::VecFromSourceCollection => {
                    let (element_size, _element_alignment) =
                        type_size_and_alignment(primary_element_type.unwrap());
                    element_size
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

            assert_eq!(ctx.target_size(), VEC_HEADER_SIZE);
            self.builder.add_vec_create(
                ctx.addr(),
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
                source_collection_self_region.addr,
                &target_variables,
            )?;

        // 3. Inline the lambda code for the current element(s).
        let lambda_result = self.gen_expression_location(lambda_expr)?;

        // 4. If the transformer supports early exit, set the Z flag based on the lambda result.
        let transformer_z_flag_state =
            self.check_if_transformer_sets_z_flag(Transformer::Filter, lambda_result, node);

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
            TransformerResult::Bool => {
                // Only alternative is that it is a bool return, so no need to take any action here
            }
            TransformerResult::VecWithLambdaResult => {
                self.transformer_add_to_collection(
                    &lambda_return_gen_type,
                    lambda_result,
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
        self.builder
            .add_jmp(continue_iteration_label, node, "jump to iter_next");

        self.builder
            .patch_jump_here(iteration_complete_patch_position);

        // 8. Finalize iteration, handling any post-processing (e.g., normalizing boolean results).
        if let Some(found_skip_early) = maybe_skip_early {
            self.builder.patch_jump_here(found_skip_early);
        }

        if matches!(transformer.return_type(), TransformerResult::Bool) {
            // It is a transformer that returns a bool, lets normalize it
            self.builder
                .add_stz(ctx.addr(), node, "transformer sets standard bool");
        }

        Ok(())
    }

    #[allow(clippy::unnecessary_wraps)]
    fn iter_init_and_next(
        &mut self,
        node: &Node,
        collection_type: Collection,
        collection_self_addr: FrameMemoryAddress,
        target_variables: &[FrameMemoryRegion],
    ) -> Result<(InstructionPosition, PatchPosition), Error> {
        let (iterator_size, iterator_alignment) = collection_type.iterator_size_and_alignment();

        let iterator_target = self
            .temp_allocator
            .reserve(iterator_size, iterator_alignment);

        let iter_next_position = InstructionPosition(self.builder.position().0 + 1);
        let placeholder = match collection_type {
            Collection::Vec => {
                self.builder.add_vec_iter_init(
                    iterator_target.addr,
                    collection_self_addr,
                    node,
                    "vec init",
                );

                if target_variables.len() == 2 {
                    self.builder.add_vec_iter_next_pair_placeholder(
                        iterator_target.addr,
                        target_variables[0].addr,
                        target_variables[1].addr,
                        node,
                        "vec iter next pair",
                    )
                } else {
                    self.builder.add_vec_iter_next_placeholder(
                        iterator_target.addr,
                        target_variables[0].addr,
                        node,
                        "vec iter next single",
                    )
                }
            }
            Collection::Map => {
                self.builder.add_map_iter_init(
                    iterator_target.addr,
                    collection_self_addr,
                    node,
                    "map init",
                );

                if target_variables.len() == 2 {
                    self.builder.add_map_iter_next_pair_placeholder(
                        iterator_target.addr,
                        target_variables[0].addr,
                        target_variables[1].addr,
                        node,
                        "map next_pair",
                    )
                } else {
                    self.builder.add_map_iter_next_placeholder(
                        iterator_target.addr,
                        target_variables[0].addr,
                        node,
                        "map next_single",
                    )
                }
            }
            Collection::Grid => todo!(),
            Collection::Range => todo!(),

            // Low  prio
            Collection::String => todo!(),
        };

        Ok((iter_next_position, placeholder))
    }

    fn check_if_transformer_sets_z_flag(
        &mut self,
        transformer: Transformer,
        in_value: FrameMemoryRegion,
        node: &Node,
    ) -> GeneratedExpressionResultKind {
        match transformer {
            Transformer::Filter => {
                assert_eq!(in_value.size.0, 1); // bool
                self.builder
                    .add_tst8(in_value.addr, node, "filter bool to z flag");
                GeneratedExpressionResultKind::ZFlagIsTrue
            }
            Transformer::Map => GeneratedExpressionResultKind::ZFlagUnmodified,
            Transformer::Any => {
                self.builder.add_tst8(in_value.addr, node, "any, check tag");
                GeneratedExpressionResultKind::ZFlagIsInversion
            }
            Transformer::All => {
                self.builder.add_tst8(in_value.addr, node, "all, check tag");
                GeneratedExpressionResultKind::ZFlagIsTrue
            }
            Transformer::FilterMap => {
                self.builder
                    .add_tst8(in_value.addr, node, "filter map, check tag");
                GeneratedExpressionResultKind::ZFlagIsTrue
            }
        }
    }

    fn add_to_collection(
        &mut self,
        node: &Node,
        collection: Collection,
        mut_collection: FrameMemoryRegion,
        value: FrameMemoryRegion,
    ) {
        match collection {
            Collection::Vec => {
                self.builder
                    .add_vec_push(mut_collection.addr, value.addr, node, "push");
            }
            Collection::Map => todo!(),
            Collection::Grid => todo!(),
            Collection::String => todo!(),
            Collection::Range => todo!(),
        }
    }

    fn transformer_add_to_collection(
        &mut self,
        in_value_type: &BasicType,
        in_value: FrameMemoryRegion,
        should_unwrap_value: bool,
        collection_type: Collection,
        mut_collection: FrameMemoryRegion,
        node: &Node,
    ) {
        let adjusted_region = if should_unwrap_value {
            let (_tag_offset, _tag_size, payload_offset, payload_max_size) =
                in_value_type.unwrap_info().unwrap();
            FrameMemoryRegion {
                addr: in_value.addr + payload_offset,
                size: payload_max_size,
            }
        } else {
            in_value
        };

        self.add_to_collection(node, collection_type, mut_collection, adjusted_region);
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

fn struct_field_offset(
    index_to_look_for: usize,
    anon_struct_type: &AnonymousStructType,
) -> (MemoryOffset, MemorySize, MemoryAlignment) {
    let mut offset = MemoryOffset(0);
    for (field_index, (_name, field)) in
        anon_struct_type.field_name_sorted_fields.iter().enumerate()
    {
        let (field_size, field_alignment) = type_size_and_alignment(&field.field_type);
        let field_start_offset = offset.space(field_size, field_alignment);
        if field_index == index_to_look_for {
            return (field_start_offset, field_size, field_alignment);
        }
    }

    panic!("field index is wrong")
}
