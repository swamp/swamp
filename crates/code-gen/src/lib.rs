/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

pub mod alloc;
pub mod alloc_util;
pub mod code_bld;
pub mod constants;
pub mod ctx;
mod intr;
pub mod layout;
mod location;
pub mod reg_pool;

use crate::alloc::ScopeAllocator;
use crate::constants::ConstantsManager;
use crate::ctx::Context;
use crate::layout::layout_variables;
use crate::reg_pool::TempRegister;
use seq_map::SeqMap;
use source_map_cache::{FileLineInfo, SourceMapLookup, SourceMapWrapper};
use source_map_node::{FileId, Node};
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{
    ConstantId, ConstantRef, Expression, ExpressionKind, InternalFunctionDefinitionRef,
    InternalFunctionId, InternalMainExpression, MutRefOrImmutableExpression, VariableRef,
};
use swamp_types::{Attributes, Type};
use swamp_vm_disasm::{SourceFileLineInfo, disasm_instructions_color};
use swamp_vm_instr_build::{InstructionBuilderState, PatchPosition};
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, FrameMemoryInfo, FramePlacedType, FunctionInfo, FunctionInfoKind,
    HeapPlacedType, TypedRegister, VmType, show_frame_memory,
};
use swamp_vm_types::{
    BinaryInstruction, CountU16, FrameMemoryRegion, GRID_HEADER_ALIGNMENT, GRID_HEADER_SIZE,
    InstructionPosition, InstructionPositionOffset, InstructionRange, MAP_HEADER_ALIGNMENT,
    MAP_HEADER_SIZE, MAP_ITERATOR_ALIGNMENT, MAP_ITERATOR_SIZE, MemoryAlignment, MemoryOffset,
    MemorySize, Meta, RANGE_HEADER_ALIGNMENT, RANGE_HEADER_SIZE, RANGE_ITERATOR_ALIGNMENT,
    RANGE_ITERATOR_SIZE, STRING_HEADER_ALIGNMENT, STRING_HEADER_SIZE, TempFrameMemoryAddress,
    VEC_HEADER_ALIGNMENT, VEC_HEADER_SIZE, VEC_ITERATOR_ALIGNMENT, VEC_ITERATOR_SIZE,
    ZFlagPolarity,
};

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

pub struct SpilledArgument {
    pub index: u8,
    pub frame_memory_region: FrameMemoryRegion,
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

pub enum DetailedLocationResolved {
    Register(TypedRegister),
    TempRegister(TempRegister),
}

impl DetailedLocationResolved {
    #[must_use]
    pub fn register(&self) -> &TypedRegister {
        match self {
            Self::Register(reg) => reg,
            Self::TempRegister(reg) => reg.register(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum DetailedLocationRevised {
    Register {
        reg: TypedRegister,
    },
    Memory {
        base_ptr_reg: TypedRegister,
        offset: MemoryOffset,
        ty: VmType,
    },
}

impl DetailedLocationRevised {
    pub fn get_type(&self) -> &VmType {
        match self {
            DetailedLocationRevised::Register { reg, .. } => &reg.basic_type,
            DetailedLocationRevised::Memory { ty, .. } => ty,
        }
    }

    pub fn add_offset(&self, new_offset: MemoryOffset, ty: VmType) -> Self {
        match self {
            Self::Register { reg } => {
                debug_assert!(
                    ty.is_pointer(),
                    "we can not take offset unless it is a pointer"
                );
                Self::Memory {
                    base_ptr_reg: reg.clone(),
                    offset: new_offset,
                    ty,
                }
            }

            Self::Memory {
                base_ptr_reg: base,
                offset,
                ..
            } => Self::Memory {
                base_ptr_reg: base.clone(),
                offset: offset.clone() + new_offset,
                ty,
            },
        }
    }

    pub fn is_register(&self) -> bool {
        matches!(self, DetailedLocationRevised::Register { .. })
    }

    pub fn is_memory(&self) -> bool {
        matches!(self, DetailedLocationRevised::Memory { .. })
    }

    pub fn base_register(&self) -> Option<&TypedRegister> {
        match self {
            DetailedLocationRevised::Register { .. } => None,
            DetailedLocationRevised::Memory {
                base_ptr_reg: base, ..
            } => Some(base),
        }
    }

    pub fn offset(&self) -> Option<MemoryOffset> {
        match self {
            DetailedLocationRevised::Register { .. } => None,
            DetailedLocationRevised::Memory { offset, .. } => Some(*offset),
        }
    }
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
    variable_offsets: SeqMap<usize, TypedRegister>,
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
        /* TODO:
        for function_fixup in &self.codegen_state.builder.function_fixups {
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

         */
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
        assert!(frame_and_variable_info.return_placement.ty().is_pointer());

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

        /*
        let mut function_generator = CodeBuilder::new(
            /* &mut swamp_vm_instr_build::InstructionBuilder<'_> */,
            /* seq_map::SeqMap<usize, swamp_vm_types::types::TypedRegister> */,
            /* seq_map::SeqMap<u32, swamp_vm_types::types::HeapPlacedType> */,
            /* reg_pool::RegisterPool */, /* alloc::ScopeAllocator */);

         */

        /*
                   &mut self.codegen_state,
           &mut func_builder,
           frame_and_variable_info.variable_offsets,
           frame_size,
           frame_and_variable_info.temp_allocator_region,
           &in_data.assigned_name,
           source_map_wrapper,

        */

        let return_register = TypedRegister::new(0, frame_and_variable_info.return_placement);

        let ctx = Context::new(return_register);
        //info!(?in_data, "generate");
        // TODO: function_generator.emit_expression_materialize(&in_data.expression, &ctx);

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

/*
pub struct FunctionCodeGen<'a> {
    //state: &'a mut CodeGenState,
    builder: &'a mut CodeBuilder<'a>,
    //variable_registers: SeqMap<usize, TypedRegister>,
    //total_frame_size: FrameMemorySize,
    variable_frame_size: FrameMemorySize,
    argument_registers: TempRegisterPool,
    debug_name: String,
    source_map_lookup: &'a SourceMapWrapper<'a>,
}

impl<'a> FunctionCodeGen<'a> {
    #[must_use]
    pub fn new(
        state: &'a mut CodeGenState,
        builder: &'a mut InstructionBuilder<'a>,
        variable_offsets: SeqMap<usize, TypedRegister>,
        frame_size: FrameMemorySize,
        temp_memory_region: FrameMemoryRegion,
        debug_name: &str,
        source_map_lookup: &'a SourceMapWrapper,
    ) -> Self {
        const ARGUMENT_MAX_SIZE: u16 = 2 * 1024;

        Self {
            state,
            //variable_registers: variable_offsets,
            total_frame_size: frame_size,
            variable_frame_size: temp_memory_region.addr.as_size(),
            argument_registers: TempRegisterPool::new(8, 32),

            builder: &mut CodeBuilder::new(builder, variable_offsets),
            debug_name: debug_name.to_string(),
            source_map_lookup,
        }
    }
}

impl FunctionCodeGen<'_> {

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
}
 */

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
