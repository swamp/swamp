/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

pub mod alloc;
mod anon_struct;
mod assignment;
mod bin_op;
mod call;
pub mod code_bld;
pub mod constants;
pub mod ctx;
pub mod disasm;
mod enum_variants;
mod equal;
mod expr;
mod func;
pub mod intr;
pub mod layout;
mod literal;
mod location;
mod logical;
mod lvalue;
mod map;
mod postfix;
pub mod reg_pool;
mod relational;
mod rvalue;
mod slice;
mod slice_pair;
pub mod state;
mod statement;
pub mod top_state;
mod tuple;
mod vec;

use crate::alloc::ScopeAllocator;
use crate::reg_pool::{RegisterPool, TempRegister};
use seq_map::SeqMap;
use source_map_node::Node;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{
    ConstantId, ConstantRef, Expression, ExpressionKind, InternalFunctionDefinitionRef,
    InternalFunctionId, MutRefOrImmutableExpression, VariableRef,
};
use swamp_types::Type;
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, FrameMemoryInfo, FramePlacedType, FunctionInfoKind, HeapPlacedType,
    TypedRegister, VariableRegister,
};
use swamp_vm_types::{
    CountU16, FrameMemoryRegion, GRID_HEADER_ALIGNMENT, GRID_HEADER_SIZE, InstructionPosition,
    InstructionRange, MAP_HEADER_ALIGNMENT, MAP_HEADER_SIZE, MAP_ITERATOR_ALIGNMENT,
    MAP_ITERATOR_SIZE, MemoryAlignment, MemorySize, RANGE_HEADER_ALIGNMENT, RANGE_HEADER_SIZE,
    RANGE_ITERATOR_ALIGNMENT, RANGE_ITERATOR_SIZE, STRING_HEADER_ALIGNMENT, STRING_HEADER_SIZE,
    TempFrameMemoryAddress, VEC_HEADER_ALIGNMENT, VEC_HEADER_SIZE, VEC_ITERATOR_ALIGNMENT,
    VEC_ITERATOR_SIZE, ZFlagPolarity,
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

pub struct SpilledRegister {
    pub register: TypedRegister,
    pub frame_memory_region: FrameMemoryRegion,
}
pub struct SpilledRegisterRegion {
    pub registers: Vec<TypedRegister>,
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
    pub const fn register(&self) -> &TypedRegister {
        match self {
            Self::Register(reg) => reg,
            Self::TempRegister(reg) => reg.register(),
        }
    }
}

impl Collection {
    #[must_use]
    pub const fn size_and_alignment(&self) -> (MemorySize, MemoryAlignment) {
        match self {
            Self::Vec => (VEC_HEADER_SIZE, VEC_HEADER_ALIGNMENT),
            Self::Map => (MAP_HEADER_SIZE, MAP_HEADER_ALIGNMENT),
            Self::Grid => (GRID_HEADER_SIZE, GRID_HEADER_ALIGNMENT),
            Self::String => (STRING_HEADER_SIZE, STRING_HEADER_ALIGNMENT),
            Self::Range => (RANGE_HEADER_SIZE, RANGE_HEADER_ALIGNMENT),
        }
    }

    #[must_use]
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

    #[must_use]
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

#[derive(PartialEq, Eq, Debug)]
pub enum FlagStateKind {
    TFlagIsIndeterminate,
    TFlagIsTrueWhenSet,
    TFlagIsTrueWhenClear,
}

pub struct FlagState {
    pub kind: FlagStateKind,
}

impl FlagState {
    pub(crate) fn invert_polarity(&self) -> Self {
        Self {
            kind: self.kind.invert_polarity(),
        }
    }
}

impl FlagStateKind {
    pub(crate) fn invert_polarity(&self) -> Self {
        match self {
            Self::TFlagIsIndeterminate => {
                panic!("can not invert polarity. status is unknown")
            }
            Self::TFlagIsTrueWhenSet => Self::TFlagIsTrueWhenClear,
            Self::TFlagIsTrueWhenClear => Self::TFlagIsTrueWhenSet,
        }
    }

    pub(crate) fn polarity(&self) -> ZFlagPolarity {
        match self {
            Self::TFlagIsIndeterminate => panic!("polarity is undefined"),
            Self::TFlagIsTrueWhenSet => ZFlagPolarity::TrueWhenSet,
            Self::TFlagIsTrueWhenClear => ZFlagPolarity::TrueWhenClear,
        }
    }
}

impl FlagState {
    pub(crate) fn polarity(&self) -> ZFlagPolarity {
        self.kind.polarity()
    }
}

impl Default for FlagState {
    fn default() -> Self {
        Self {
            kind: FlagStateKind::TFlagIsIndeterminate,
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

pub fn reserve(ty: &Type, allocator: &mut ScopeAllocator) -> FramePlacedType {
    allocator.reserve(ty)
}

pub struct FrameAndVariableInfo {
    pub frame_memory: FrameMemoryInfo,
    parameters: Vec<VariableRegister>,
    parameter_and_variable_offsets: SeqMap<usize, TypedRegister>,
    temp_allocator_region: FrameMemoryRegion,
    frame_registers: RegisterPool,
    highest_register_used: u8,
}

#[derive(Debug)]
pub struct FunctionInData {
    pub function_name_node: Node,
    pub kind: FunctionInfoKind,
    pub assigned_name: String,
    pub parameter_variables: Vec<VariableRef>,
    pub function_variables: Vec<VariableRef>,
    pub return_type: Type,
    pub expression: Expression,
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
