/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod alloc;
mod anon_struct;
mod assignment;
mod bin_op;
mod block;
mod call;
pub mod code_bld;
pub mod constants;
pub mod ctx;
pub mod disasm;
mod enum_variants;
mod equal;
mod err;
mod expr;
mod func;
mod guard;
mod host;
mod init;
mod initializer_list;
mod initializer_pair_list;
pub mod intr;
mod iter;
pub mod layout;
mod literal;
mod location;
mod logical;
mod lvalue;
mod map;
mod match_expr;
mod postfix;
pub mod prelude;
pub mod reg_pool;
mod relational;
mod rvalue;
mod sparse;
pub mod state;
mod statement;
pub mod top_state;
mod transfer;
mod transfer_instr;
mod transformer;
mod tuple;
mod variable;
mod vec;
mod when;

pub const MAX_REGISTER_INDEX_FOR_PARAMETERS: u8 = 6;

use crate::alloc::StackFrameAllocator;
use crate::reg_pool::TempRegister;
use seq_map::SeqMap;
use source_map_node::Node;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{
    ArgumentExpression, ConstantId, ConstantRef, Expression, ExpressionKind,
    InternalFunctionDefinitionRef, InternalFunctionId, VariableScopes,
};
use swamp_types::TypeRef;
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::types::{
    BasicTypeRef, FrameMemoryInfo, FramePlacedType, FunctionInfoKind, HeapPlacedType,
    TypedRegister, VmType,
};
use swamp_vm_types::{
    CountU16, FrameMemoryRegion, InstructionPosition, InstructionRange, MemorySize,
    TempFrameMemoryAddress, ZFlagPolarity,
};

#[derive(Clone)]
pub struct SpilledRegister {
    pub register: TypedRegister,
    pub frame_memory_region: FrameMemoryRegion,
}

#[derive(Clone)]
pub enum RepresentationOfRegisters {
    Individual(Vec<TypedRegister>),
    Range { start_reg: u8, count: u8 },
    Mask(u8),
}

#[derive(Clone)]
pub struct SpilledRegisterRegion {
    pub registers: RepresentationOfRegisters,
    pub frame_memory_region: FrameMemoryRegion,
}

#[derive(Clone)]
pub struct SpilledRegisterScope {
    pub regions: Vec<SpilledRegisterRegion>,
}

pub struct ArgumentAndTempScope {
    pub argument_registers: SpilledRegisterRegion,
    pub scratch_registers: Option<SpilledRegisterRegion>,
}

pub struct SpilledRegisterScopes {
    pub stack: Vec<SpilledRegisterScope>,
}

impl Default for SpilledRegisterScopes {
    fn default() -> Self {
        Self::new()
    }
}

impl SpilledRegisterScopes {
    #[must_use]
    pub const fn new() -> Self {
        Self { stack: Vec::new() }
    }
    pub fn push(&mut self, scope: SpilledRegisterScope) {
        self.stack.push(scope);
    }

    pub fn pop(&mut self) -> SpilledRegisterScope {
        self.stack.pop().unwrap()
    }
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

#[derive(Clone, Debug)]
pub struct GenFunctionInfo {
    pub ip_range: InstructionRange,
    pub return_type: BasicTypeRef,
    pub params: Vec<BasicTypeRef>,
    pub internal_function_definition: InternalFunctionDefinitionRef,
}

impl GenFunctionInfo {
    #[must_use]
    pub fn return_type(&self) -> &TypeRef {
        &self.internal_function_definition.signature.return_type
    }
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

pub fn reserve(
    ty: &TypeRef,
    layout_cache: &mut LayoutCache,
    allocator: &mut StackFrameAllocator,
) -> FramePlacedType {
    allocator.reserve(layout_cache, ty)
}

pub struct FrameAndVariableInfo {
    pub frame_memory: FrameMemoryInfo,
    pub return_type: VmType,
    parameter_and_variable_offsets: SeqMap<usize, TypedRegister>,
    local_frame_allocator: StackFrameAllocator,
}

#[derive(Debug)]
pub struct FunctionInData {
    pub function_name_node: Node,
    pub kind: FunctionInfoKind,
    pub assigned_name: String,
    pub function_variables: VariableScopes,
    pub return_type: TypeRef,
    pub expression: Expression,
}

fn single_intrinsic_fn(
    body: &Expression,
) -> Option<(&IntrinsicFunction, &Vec<ArgumentExpression>)> {
    let ExpressionKind::Block(block_expressions) = &body.kind else {
        return None;
    };

    if block_expressions.is_empty() {
        return None;
    }
    if let ExpressionKind::IntrinsicCallEx(found_intrinsic_fn, arguments) =
        &block_expressions[0].kind
    {
        Some((found_intrinsic_fn, arguments))
    } else {
        None
    }
}
