/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

pub mod alloc;
pub mod alloc_util;
pub mod constants;
pub mod ctx;
mod location;
mod vec;

use crate::alloc::{ConstantMemoryRegion, FrameMemoryRegion, ScopeAllocator};
use crate::alloc_util::{
    is_grid, is_map, is_range, is_stack, is_vec, layout_struct, layout_tuple,
    layout_tuple_elements, reserve_space_for_type, type_size_and_alignment,
};
use crate::constants::ConstantsManager;
use crate::ctx::Context;
use seq_map::SeqMap;
use source_map_cache::{SourceMapLookup, SourceMapWrapper};
use source_map_node::Node;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{
    AnonymousStructLiteral, BinaryOperator, BinaryOperatorKind, BooleanExpression,
    CompoundOperatorKind, ConstantId, ConstantRef, EnumLiteralData, Expression, ExpressionKind,
    ForPattern, Function, Guard, InternalFunctionDefinition, InternalFunctionDefinitionRef,
    InternalFunctionId, InternalMainExpression, Iterable, Literal, Match,
    MutRefOrImmutableExpression, NormalPattern, Pattern, Postfix, PostfixKind,
    SingleLocationExpression, TargetAssignmentLocation, UnaryOperator, UnaryOperatorKind,
    VariableRef, WhenBinding,
};
use swamp_types::{AnonymousStructType, EnumVariantType, Signature, StructTypeField, Type};
use swamp_vm_disasm::{
    BasicType, ComplexType, FrameAddressInfo, FrameAddressInfoKind, FrameMemoryInfo, disasm_color,
    disasm_instructions_color,
};
use swamp_vm_instr_build::{InstructionBuilder, Meta, PatchPosition};
use swamp_vm_types::{
    BOOL_SIZE, BinaryInstruction, CountU16, FrameMemoryAddress, FrameMemoryAddressIndirectPointer,
    FrameMemorySize, HEAP_PTR_ALIGNMENT, HEAP_PTR_SIZE, HeapMemoryAddress, INT_SIZE,
    InstructionPosition, MemoryAlignment, MemoryOffset, MemorySize, PTR_SIZE,
    TempFrameMemoryAddress, VEC_ITERATOR_ALIGNMENT, VEC_ITERATOR_SIZE,
};
use tracing::{error, info, trace};

#[derive(Clone)]
pub enum FrameRelativeInfoKind {
    Variable(VariableRef),
}

#[derive(Clone)]
pub struct FrameRelativeInfo {
    pub frame_memory_region: FrameMemoryRegion,
    pub kind: FrameRelativeInfoKind,
}

pub struct FunctionIp {
    start_ip: InstructionPosition,
    end_ip: InstructionPosition,
    internal_fn_id: InternalFunctionId,
}

pub struct GeneratedExpressionResult {
    pub has_set_bool_z_flag: bool,
}

impl Default for GeneratedExpressionResult {
    fn default() -> Self {
        Self {
            has_set_bool_z_flag: false,
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

pub struct FunctionInfo {
    pub starts_at_ip: InstructionPosition,
    pub internal_function_definition: InternalFunctionDefinitionRef,
}

pub struct FunctionFixup {
    pub patch_position: PatchPosition,
    pub fn_id: InternalFunctionId,
    //pub internal_function_definition: InternalFunctionDefinitionRef,
}

pub struct ConstantInfo {
    pub ip: InstructionPosition,
    pub constant_ref: ConstantRef,
    pub target_constant_memory: ConstantMemoryRegion,
}

pub struct FunctionDebugInfo {
    pub frame_relative: Vec<FrameRelativeInfo>,
}

pub struct CodeGenState {
    builder: InstructionBuilder,
    constants: ConstantsManager,
    constant_offsets: SeqMap<ConstantId, ConstantMemoryRegion>,
    constant_functions: SeqMap<ConstantId, ConstantInfo>,
    function_infos: SeqMap<InternalFunctionId, FunctionInfo>,
    function_fixups: Vec<FunctionFixup>,

    function_ips: Vec<FunctionIp>,
    function_debug_infos: SeqMap<InternalFunctionId, FunctionDebugInfo>,
    //source_map_lookup: &'b SourceMapWrapper<'b>,
    debug_last_ip: usize,
}

pub struct GenOptions {
    pub is_halt_function: bool,
}

impl CodeGenState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            builder: InstructionBuilder::default(),
            constants: ConstantsManager::new(),
            constant_offsets: SeqMap::default(),
            function_infos: SeqMap::default(),
            constant_functions: SeqMap::default(),
            function_fixups: vec![],
            function_ips: vec![],
            function_debug_infos: SeqMap::default(),
            debug_last_ip: 0,
        }
    }

    #[must_use]
    pub fn instructions(&self) -> &[BinaryInstruction] {
        &self.builder.instructions
    }

    pub fn ip(&self) -> InstructionPosition {
        InstructionPosition(self.builder.instructions.len() as u16)
    }

    pub fn create_function_sections(&self) -> SeqMap<InstructionPosition, String> {
        let mut lookups = SeqMap::new();
        for (_func_id, function_info) in &self.function_infos {
            let description = function_info
                .internal_function_definition
                .assigned_name
                .clone();
            lookups
                .insert(function_info.starts_at_ip.clone(), description)
                .unwrap();
        }

        for (_func_id, function_info) in &self.constant_functions {
            let description = format!("constant {}", function_info.constant_ref.assigned_name);
            lookups
                .insert(function_info.ip.clone(), description)
                .unwrap();
        }

        lookups
    }
    #[must_use]
    pub fn builder(&self) -> &InstructionBuilder {
        &self.builder
    }
    pub fn constant_functions(&self) -> &SeqMap<ConstantId, ConstantInfo> {
        &self.constant_functions
    }
    pub(crate) fn add_call(&mut self, internal_fn: &InternalFunctionDefinitionRef, comment: &str) {
        let call_comment = &format!("calling {} ({})", internal_fn.assigned_name, comment);

        if let Some(found) = self.function_infos.get(&internal_fn.program_unique_id) {
            self.builder
                .add_call(&found.starts_at_ip, &internal_fn.name.0, call_comment);
        } else {
            let patch_position = self
                .builder
                .add_call_placeholder(&internal_fn.name.0, call_comment);
            info!(
                id = internal_fn.program_unique_id,
                name = internal_fn.assigned_name,
                "calling function"
            );
            self.function_fixups.push(FunctionFixup {
                patch_position,
                fn_id: internal_fn.program_unique_id,
            });
        }
    }
    #[must_use]
    pub fn comments(&self) -> &[Meta] {
        &self.builder.meta
    }

    pub fn finalize(&mut self) {
        for function_fixup in &self.function_fixups {
            info!(fn_id = ?function_fixup.fn_id, "fixing up function");
            let func = self.function_infos.get(&function_fixup.fn_id).unwrap();
            self.builder.patch_call(
                PatchPosition(InstructionPosition(function_fixup.patch_position.0.0)),
                &func.starts_at_ip,
            );
        }

        self.function_fixups.clear();
    }

    #[must_use]
    pub fn take_instructions_and_constants(
        self,
    ) -> (
        Vec<BinaryInstruction>,
        Vec<u8>,
        SeqMap<ConstantId, ConstantInfo>,
    ) {
        (
            self.builder.instructions,
            self.constants.take_data(),
            self.constant_functions,
        )
    }

    pub fn gen_function_def(
        &mut self,
        internal_fn_def: &InternalFunctionDefinitionRef,
        options: &GenOptions,
        source_map_wrapper: &SourceMapWrapper,
    ) -> Result<(), Error> {
        assert_ne!(internal_fn_def.program_unique_id, 0);
        info!(id=?internal_fn_def.program_unique_id, internal_fn_def.assigned_name, "generating function");
        self.function_infos
            .insert(
                internal_fn_def.program_unique_id,
                FunctionInfo {
                    starts_at_ip: self.builder.position(),
                    internal_function_definition: internal_fn_def.clone(),
                },
            )
            .unwrap();

        let start_ip = self.ip();

        let debug_frame_relative_info = {
            let frame_relative_infos = {
                let mut function_generator = FunctionCodeGen::new(self, source_map_wrapper);

                function_generator.layout_variables(
                    &internal_fn_def.name.0,
                    &internal_fn_def.function_scope_state,
                    &internal_fn_def.signature.signature.return_type,
                )?;

                let ExpressionKind::Block(block_expressions) = &internal_fn_def.body.kind else {
                    panic!("function body should be a block")
                };

                let (return_type_size, _return_alignment) =
                    type_size_and_alignment(&internal_fn_def.signature.signature.return_type);
                let ctx = Context::new(FrameMemoryRegion::new(
                    FrameMemoryAddress(0),
                    return_type_size,
                ));
                function_generator.gen_expression(&internal_fn_def.body, &ctx)?;

                function_generator.frame_memory_infos.clone()
            };

            let ip_infos = SeqMap::new();

            let mut memory_infos = Vec::new();

            for x in &frame_relative_infos {
                let converted_kind = match &x.kind {
                    FrameRelativeInfoKind::Variable(var) => FrameAddressInfoKind::Variable {
                        is_mutable: var.is_mutable(),
                        name: var.assigned_name.clone(),
                        ty: ComplexType::BasicType(BasicType::S32),
                    },
                };

                memory_infos.push(FrameAddressInfo {
                    addr: x.frame_memory_region.addr,
                    size: FrameMemorySize(x.frame_memory_region.size.0),
                    kind: converted_kind,
                })
            }

            let mem_info = FrameMemoryInfo {
                infos: memory_infos,
            };

            let output =
                disasm_instructions_color(self.instructions(), &start_ip, &mem_info, &ip_infos);
            eprintln!("output\n{output}");

            frame_relative_infos
        };

        let end_ip = self.ip();

        self.function_ips.push(FunctionIp {
            start_ip,
            end_ip,
            internal_fn_id: internal_fn_def.program_unique_id,
        });

        self.function_debug_infos
            .insert(
                internal_fn_def.program_unique_id,
                FunctionDebugInfo {
                    frame_relative: debug_frame_relative_info,
                },
            )
            .unwrap();

        self.finalize_function(options);

        Ok(())
    }

    pub fn finalize_function(&mut self, options: &GenOptions) {
        if options.is_halt_function {
            self.builder.add_hlt(&Node::default(), "");
        } else {
            self.builder.add_ret(&Node::default(), "");
        }
    }

    pub fn reserve_space_for_constants(&mut self, constants: &[ConstantRef]) -> Result<(), Error> {
        for constant in constants {
            let (size, alignment) = type_size_and_alignment(&constant.resolved_type);

            let constant_memory_address = self.constants.reserve(size, alignment);

            let constant_memory_region = ConstantMemoryRegion {
                addr: constant_memory_address,
                size,
            };

            self.constant_offsets
                .insert(constant.id, constant_memory_region)
                .unwrap();
        }

        Ok(())
    }
    pub fn gen_constants_expression_functions_in_order(
        &mut self,
        constants: &[ConstantRef],
        source_map_wrapper: &SourceMapWrapper,
    ) -> Result<(), Error> {
        for constant in constants {
            let target_region = *self.constant_offsets.get(&constant.id).unwrap();
            let ip = self.builder.position();
            {
                let mut function_generator = FunctionCodeGen::new(self, source_map_wrapper);
                function_generator.layout_variables(
                    &constant.name,
                    &constant.function_scope_state,
                    &constant.resolved_type,
                )?;

                let constant_target_ctx = Context::new(FrameMemoryRegion::new(
                    FrameMemoryAddress(0),
                    target_region.size,
                ));

                /*
                function_generator.frame_size = FrameMemorySize(target_region.size.0);

                function_generator.argument_allocator = ScopeAllocator::new(FrameMemoryRegion {
                    addr: FrameMemoryAddress(function_generator.frame_size.0),
                    size: MemorySize(1024),
                });

                function_generator.temp_allocator = ScopeAllocator::new(FrameMemoryRegion {
                    addr: FrameMemoryAddress(function_generator.frame_size.0 + 1024),
                    size: MemorySize(1024),
                });

                 */

                function_generator.gen_expression(&constant.expr, &constant_target_ctx)?;

                self.finalize_function(&GenOptions {
                    is_halt_function: true,
                });
            }

            let constant_info = ConstantInfo {
                ip,
                target_constant_memory: target_region,
                constant_ref: constant.clone(),
            };

            self.constant_functions
                .insert(constant.id, constant_info)
                .unwrap();
        }

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
        let mut function_generator = FunctionCodeGen::new(self, source_map_lookup);

        function_generator.layout_variables(
            &main.expression.node,
            &main.function_scope_state,
            &main.expression.ty,
        )?;
        let empty_ctx = Context::new(FrameMemoryRegion::default());
        function_generator.gen_expression(&main.expression, &empty_ctx)?;
        self.finalize_function(options);
        Ok(())
    }
}

pub struct FunctionCodeGen<'a> {
    state: &'a mut CodeGenState,
    variable_offsets: SeqMap<usize, FrameMemoryRegion>,
    frame_size: FrameMemorySize,
    //extra_frame_allocator: ScopeAllocator,
    temp_allocator: ScopeAllocator,
    argument_allocator: ScopeAllocator,

    frame_memory_infos: Vec<FrameRelativeInfo>,

    source_map_lookup: &'a SourceMapWrapper<'a>,
}

impl<'a> FunctionCodeGen<'a> {
    #[must_use]
    pub fn new(state: &'a mut CodeGenState, source_map_lookup: &'a SourceMapWrapper) -> Self {
        Self {
            state,
            variable_offsets: SeqMap::default(),
            frame_size: FrameMemorySize(0),
            //  extra_frame_allocator: ScopeAllocator::new(FrameMemoryRegion::default()),
            temp_allocator: ScopeAllocator::new(FrameMemoryRegion::default()),
            argument_allocator: ScopeAllocator::new(FrameMemoryRegion::default()),
            frame_memory_infos: Vec::new(),
            source_map_lookup,
        }
    }
}

impl FunctionCodeGen<'_> {
    #[allow(clippy::too_many_lines)]
    pub fn gen_single_intrinsic_call(
        &mut self,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        self_addr: Option<FrameMemoryRegion>,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) -> Result<(), Error> {
        match intrinsic_fn {
            // Fixed
            IntrinsicFunction::FloatRound => self.state.builder.add_float_round(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float round",
            ),
            IntrinsicFunction::FloatFloor => self.state.builder.add_float_floor(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float floor",
            ),
            IntrinsicFunction::FloatSqrt => self.state.builder.add_float_sqrt(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float sqr",
            ),
            IntrinsicFunction::FloatSign => self.state.builder.add_float_sign(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float sign",
            ),
            IntrinsicFunction::FloatAbs => self.state.builder.add_float_abs(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float abs",
            ),
            IntrinsicFunction::FloatRnd => self.state.builder.add_float_prnd(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float rnd",
            ),
            IntrinsicFunction::FloatCos => self.state.builder.add_float_cos(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float round",
            ),
            IntrinsicFunction::FloatSin => self.state.builder.add_float_sin(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float round",
            ),
            IntrinsicFunction::FloatAcos => self.state.builder.add_float_acos(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float round",
            ),
            IntrinsicFunction::FloatAsin => self.state.builder.add_float_asin(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float round",
            ),
            IntrinsicFunction::FloatAtan2 => self.state.builder.add_float_atan2(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "float round",
            ),
            IntrinsicFunction::FloatMin => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.gen_expression_for_access(float_arg_expr)?;
                self.state.builder.add_float_min(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    float_region.addr,
                    node,
                    "float round",
                )
            }
            IntrinsicFunction::FloatMax => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.gen_expression_for_access(float_arg_expr)?;
                self.state.builder.add_float_max(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    float_region.addr,
                    node,
                    "float round",
                )
            }
            IntrinsicFunction::FloatClamp => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.gen_expression_for_access(float_arg_expr)?;

                let float_b = &arguments[1];
                let MutRefOrImmutableExpression::Expression(float_b_expr) = float_b else {
                    panic!();
                };
                let float_b_region = self.gen_expression_for_access(float_b_expr)?;

                self.state.builder.add_float_clamp(
                    ctx.addr(),
                    float_region.addr,
                    self_addr.unwrap().addr,
                    float_b_region.addr,
                    node,
                    "float round",
                )
            }
            IntrinsicFunction::FloatToString => self.state.builder.float_to_string(
                ctx.addr(),
                self_addr.unwrap().addr(),
                node,
                "float_to_string",
            ),
            // Int
            IntrinsicFunction::IntAbs => {
                self.state
                    .builder
                    .add_int_abs(ctx.addr(), self_addr.unwrap().addr, node, "intrnd")
            }

            IntrinsicFunction::IntRnd => {
                self.state
                    .builder
                    .add_int_rnd(ctx.addr(), self_addr.unwrap().addr, node, "intrnd");
            }
            IntrinsicFunction::IntMax => {
                self.state
                    .builder
                    .add_int_max(ctx.addr(), self_addr.unwrap().addr, node, "int_max")
            }
            IntrinsicFunction::IntMin => {
                self.state
                    .builder
                    .add_int_min(ctx.addr(), self_addr.unwrap().addr, node, "int_min")
            }
            IntrinsicFunction::IntClamp => self.state.builder.add_int_clamp(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "int_clamp",
            ),
            IntrinsicFunction::IntToFloat => self.state.builder.add_int_to_float(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "int to float",
            ),
            IntrinsicFunction::IntToString => self.state.builder.add_int_to_string(
                ctx.addr(),
                self_addr.unwrap().addr(),
                node,
                "int_to_string",
            ),

            // String
            IntrinsicFunction::StringLen => {
                self.state.builder.add_string_len(
                    ctx.addr(),
                    FrameMemoryAddressIndirectPointer(self_addr.unwrap().addr),
                    node,
                    "get the length",
                );
            }

            // Vec
            IntrinsicFunction::VecFromSlice => {
                let slice_variable = &arguments[0];
                let slice_region = self.gen_for_access_or_location_ex(slice_variable)?;
                let (element_size, element_alignment) =
                    type_size_and_alignment(&slice_variable.ty());
                self.state.builder.add_vec_from_slice(
                    ctx.addr(),
                    slice_region.addr,
                    element_size,
                    CountU16(slice_region.size.0 / element_size.0),
                    node,
                    "create vec from slice",
                );
            }
            IntrinsicFunction::VecPush => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.gen_expression_for_access(key_expr)?;
                self.state.builder.add_vec_push(
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    node,
                    "vec push",
                );
            }
            IntrinsicFunction::VecPop => {
                self.state.builder.add_vec_pop(
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
                let index_region = self.gen_expression_for_access(index_expr)?;
                self.state.builder.add_vec_remove_index(
                    self_addr.unwrap().addr,
                    index_region.addr,
                    node,
                    "get the vec length",
                );
            }
            IntrinsicFunction::VecRemoveIndexGetValue => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.gen_expression_for_access(key_expr)?;
                todo!();
                /*
                self.state.builder.add_vec_remove_index_get_value(
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    "vec push",
                );

                 */
            }
            IntrinsicFunction::VecClear => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.gen_expression_for_access(key_expr)?;
                /*
                self.state.builder.add_vec_clear(
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    "vec clear",
                );

                 */
            }
            IntrinsicFunction::VecGet => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.gen_expression_for_access(key_expr)?;
                /*
                self.state.builder.add_vec_get(
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    "vec get",
                );

                 */
            }
            IntrinsicFunction::VecCreate => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.gen_expression_for_access(key_expr)?;
                /*
                self.state.builder.add_vec_create(
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    "vec create",
                );

                 */
            }
            IntrinsicFunction::VecSubscript => {
                let maybe_index_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.gen_expression_for_access(index_expr)?;
                self.state.builder.add_vec_subscript(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    index_region.addr,
                    node,
                    "get the vec length",
                );
            }
            IntrinsicFunction::VecSubscriptMut => {
                let maybe_index_argument = &arguments[0];

                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.gen_expression_for_access(index_expr)?;

                let source_argument = &arguments[1];
                let MutRefOrImmutableExpression::Expression(value_expr) = source_argument else {
                    panic!();
                };
                let value_region = self.gen_expression_for_access(value_expr)?;

                self.state.builder.add_vec_subscript_mut(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    index_region.addr,
                    value_region.addr,
                    node,
                    "set the vec subscript",
                );
            }
            IntrinsicFunction::VecSubscriptRange => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.gen_expression_for_access(key_expr)?;
                /*
                self.state.builder.add_vec_subscript_range(
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    "vec subscript range",
                );

                 */
            }
            IntrinsicFunction::VecIter => todo!(),
            IntrinsicFunction::VecIterMut => todo!(),

            IntrinsicFunction::VecFor => todo!(),
            IntrinsicFunction::VecWhile => todo!(),
            IntrinsicFunction::VecFindMap => todo!(),
            IntrinsicFunction::VecSelfPush => todo!(),
            IntrinsicFunction::VecSelfExtend => todo!(),
            IntrinsicFunction::VecLen => self.state.builder.add_vec_len(
                ctx.addr(),
                self_addr.unwrap().addr,
                node,
                "get the vec length",
            ),
            IntrinsicFunction::VecIsEmpty => todo!(),

            // Map
            IntrinsicFunction::MapCreate => todo!(),
            IntrinsicFunction::MapFromSlicePair => {
                let slice_pair_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(expr) = slice_pair_argument else {
                    panic!();
                };

                let ExpressionKind::Literal(some_lit) = &expr.kind else {
                    panic!();
                };

                let Literal::SlicePair(slice_type, expression_pairs) = some_lit else {
                    panic!();
                };

                let slice_pair_info = self.gen_slice_pair_literal(slice_type, expression_pairs);
                self.state.builder.add_map_new_from_slice(
                    ctx.addr(),
                    slice_pair_info.addr.to_addr(),
                    slice_pair_info.key_size,
                    slice_pair_info.value_size,
                    slice_pair_info.element_count,
                    node,
                    "create map from temporary slice pair",
                );
            }
            IntrinsicFunction::MapHas => {
                self.state
                    .builder
                    .add_map_has(self_addr.unwrap().addr, node, "map_has")
            }
            IntrinsicFunction::MapRemove => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                self.gen_intrinsic_map_remove(self_addr.unwrap(), key_argument, ctx)?;
            }
            IntrinsicFunction::MapIter => todo!(),
            IntrinsicFunction::MapIterMut => todo!(),
            IntrinsicFunction::MapLen => {
                /*
                self.state.builder.add_map_len(
                    ctx.addr(),
                    self_addr.unwrap().addr, // mut self
                    "map len",
                );

                 */
            }
            IntrinsicFunction::MapIsEmpty => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                /*
                let key_region = self.gen_expression_for_access(key_expr)?;
                self.state.builder.add_map_is_empty(
                    self_addr.unwrap().addr, // mut self
                    key_region.addr,
                    "vec subscript range",
                );

                 */
            }
            IntrinsicFunction::MapSubscript => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.gen_expression_for_access(key_argument)?;
                self.state.builder.add_map_subscript(
                    ctx.addr(),
                    self_addr.unwrap().addr,
                    key.addr,
                    node,
                    "map_subscript",
                );
            }
            IntrinsicFunction::MapSubscriptSet => todo!(),
            IntrinsicFunction::MapSubscriptMut => todo!(),
            IntrinsicFunction::MapSubscriptMutCreateIfNeeded => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.gen_expression_for_access(key_argument)?;
                self.state.builder.add_map_subscript_mut_create(
                    self_addr.unwrap().addr,
                    key.addr,
                    node,
                    "map_subscript_mut_create (set)",
                );
            }

            // Map2
            IntrinsicFunction::Map2Remove => todo!(),
            IntrinsicFunction::Map2Insert => todo!(),
            IntrinsicFunction::Map2GetColumn => todo!(),
            IntrinsicFunction::Map2GetRow => todo!(),
            IntrinsicFunction::Map2Get => todo!(),
            IntrinsicFunction::Map2Has => todo!(),
            IntrinsicFunction::Map2Create => todo!(),

            // Sparse
            IntrinsicFunction::SparseCreate => {
                /*
                self.state.builder.add_sparse_create(
                    ctx.addr(),
                    "map_subscript_mut_create (set)",
                );

                 */
            }
            IntrinsicFunction::SparseFromSlice => todo!(),
            IntrinsicFunction::SparseIter => todo!(),
            IntrinsicFunction::SparseIterMut => todo!(),
            IntrinsicFunction::SparseSubscript => todo!(),
            IntrinsicFunction::SparseSubscriptMut => todo!(),
            IntrinsicFunction::SparseHas => todo!(),
            IntrinsicFunction::SparseRemove => todo!(),

            // Grid
            IntrinsicFunction::GridCreate => {
                /*
                self.state.builder.add_sparse_create(
                    ctx.addr(),
                    "map_subscript_mut_create (set)",
                );

                 */
            }
            IntrinsicFunction::GridFromSlice => todo!(),
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
            IntrinsicFunction::GridGetColumn => todo!(),

            IntrinsicFunction::Float2Magnitude => {
                // TODO:
            }

            IntrinsicFunction::SparseAdd => todo!(),
            IntrinsicFunction::SparseNew => todo!(),

            IntrinsicFunction::VecAny => todo!(),
            IntrinsicFunction::VecAll => todo!(),
            IntrinsicFunction::VecMap => todo!(),
            IntrinsicFunction::VecFilter => {
                // TODO:
            }
            IntrinsicFunction::VecFilterMap => todo!(),
            IntrinsicFunction::VecFind => todo!(),
            IntrinsicFunction::VecSwap => todo!(),
            IntrinsicFunction::VecInsert => todo!(),
            IntrinsicFunction::VecFirst => todo!(),
            IntrinsicFunction::VecLast => todo!(),
            IntrinsicFunction::VecFold => todo!(),

            IntrinsicFunction::RuntimePanic => {
                self.state
                    .builder
                    .add_panic(self_addr.unwrap().addr(), node, "intrinsic panic")
            }
            IntrinsicFunction::BoolToString => self.state.builder.bool_to_string(
                ctx.addr(),
                self_addr.unwrap().addr(),
                node,
                "bool_to_string",
            ),
        }

        Ok(())
    }

    fn gen_intrinsic_map_remove(
        &mut self,
        map_region: FrameMemoryRegion,
        key_expr: &Expression,
        ctx: &Context,
    ) -> Result<(), Error> {
        let key_region = self.gen_expression_for_access(key_expr)?;

        self.state
            .builder
            .add_map_remove(map_region.addr, key_region.addr, &key_expr.node, "");

        Ok(())
    }

    pub fn reserve(ty: &Type, allocator: &mut ScopeAllocator) -> FrameMemoryRegion {
        let (size, alignment) = type_size_and_alignment(ty);
        allocator.reserve(size, alignment)
    }

    /// # Errors
    ///
    pub fn layout_variables(
        &mut self,
        node: &Node,
        variables: &Vec<VariableRef>,
        return_type: &Type,
    ) -> Result<(), Error> {
        let mut allocator = ScopeAllocator::new(FrameMemoryRegion::new(
            FrameMemoryAddress(0),
            MemorySize(32 * 1024),
        ));
        let _current_offset = Self::reserve(return_type, &mut allocator);

        let mut enter_comment = "variables:\n".to_string();

        for var_ref in variables {
            let var_target = Self::reserve(&var_ref.resolved_type, &mut allocator);
            trace!(?var_ref.assigned_name, ?var_target, "laying out");
            enter_comment += &format!(
                "  ${:04X}:{} {}\n",
                var_target.addr.0, var_target.size.0, var_ref.assigned_name
            );

            self.add_frame_relative_info(
                var_target,
                FrameRelativeInfoKind::Variable(var_ref.clone()),
            );

            self.variable_offsets
                .insert(var_ref.unique_id_within_function, var_target)
                .map_err(|_| self.create_err(ErrorKind::VariableNotUnique, &var_ref.name))?;
        }

        //let extra_frame_size = MemorySize(80);
        self.frame_size = allocator.addr().as_size();

        self.state
            .builder
            .add_enter(self.frame_size, node, &enter_comment);

        const ARGUMENT_MAX_SIZE: u16 = 2 * 1024;
        self.argument_allocator = ScopeAllocator::new(FrameMemoryRegion::new(
            FrameMemoryAddress(self.frame_size.0),
            MemorySize(ARGUMENT_MAX_SIZE),
        ));

        self.temp_allocator = ScopeAllocator::new(FrameMemoryRegion::new(
            FrameMemoryAddress(self.frame_size.0 + ARGUMENT_MAX_SIZE),
            MemorySize(32 * 1024),
        ));

        Ok(())
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
    pub fn gen_expression_for_access(
        &mut self,
        expr: &Expression,
    ) -> Result<FrameMemoryRegion, Error> {
        let (region, _gen_result) = self.gen_expression_for_access_internal(expr)?;

        Ok(region)
    }

    /// # Panics
    ///
    #[allow(clippy::single_match_else)]
    pub fn gen_expression_for_access_internal(
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

            ExpressionKind::Literal(lit) => match lit {
                Literal::Slice(slice_type, expressions) => {
                    return Ok((
                        self.gen_slice_literal(slice_type, expressions)?,
                        GeneratedExpressionResult::default(),
                    ));
                }
                Literal::SlicePair(slice_pair_type, pairs) => {
                    let info = self.gen_slice_pair_literal(slice_pair_type, pairs);
                    return Ok((
                        FrameMemoryRegion::new(
                            info.addr.0,
                            MemorySize(info.element_count.0 * info.element_size.0),
                        ),
                        GeneratedExpressionResult::default(),
                    ));
                }
                _ => {}
            },
            _ => {}
        };

        let temp_ctx = self.temp_space_for_type(&expr.ty, "expression");

        let expression_result = self.gen_expression(expr, &temp_ctx)?;

        Ok((temp_ctx.target(), expression_result))
    }

    pub(crate) fn extra_frame_space_for_type(&mut self, ty: &Type) -> Context {
        let target = Self::reserve(ty, &mut self.temp_allocator);
        Context::new(target)
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

    pub fn gen_expression(
        &mut self,
        expr: &Expression,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let result = match &expr.kind {
            //ExpressionKind::InterpolatedString(_) => todo!(),
            ExpressionKind::ConstantAccess(constant_ref) => self
                .gen_constant_access(constant_ref, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::TupleDestructuring(variables, tuple_types, tuple_expression) => self
                .gen_tuple_destructuring(variables, tuple_types, tuple_expression)
                .map(|_| GeneratedExpressionResult::default()),

            ExpressionKind::Assignment(target_mut_location_expr, source_expr) => self
                .gen_assignment(&expr.node, target_mut_location_expr, source_expr)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::VariableAccess(variable_ref) => self
                .gen_variable_access(variable_ref, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::InternalFunctionAccess(function) => self
                .internal_function_access(function, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::BinaryOp(operator) => self.gen_binary_operator(operator, ctx),
            ExpressionKind::UnaryOp(operator) => self
                .gen_unary_operator(operator, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::PostfixChain(start, chain) => self
                .gen_postfix_chain(start, chain, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::VariableDefinition(variable, expression) => self
                .gen_variable_definition(variable, expression, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::VariableReassignment(variable, expression) => self
                .gen_variable_reassignment(variable, expression, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::AnonymousStructLiteral(anon_struct) => self
                .gen_anonymous_struct_literal(anon_struct, &expr.ty, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::Literal(basic_literal) => self
                .gen_literal(&expr.node, basic_literal, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::Option(maybe_option) => self
                .gen_option_expression(&expr.node, maybe_option.as_deref(), ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::ForLoop(a, b, c) => self
                .gen_for_loop(&expr.node, a, b, c)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::WhileLoop(condition, expression) => self
                .gen_while_loop(condition, expression, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::Block(expressions) => self
                .gen_block(expressions, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::Match(match_expr) => self
                .gen_match(match_expr, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::Guard(guards) => self
                .gen_guard(guards, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::If(conditional, true_expr, false_expr) => self
                .gen_if(conditional, true_expr, false_expr.as_deref(), ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::When(bindings, true_expr, false_expr) => self
                .gen_when(bindings, true_expr, false_expr.as_deref(), ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::CompoundAssignment(target_location, operator_kind, source_expr) => self
                .compound_assignment(target_location, operator_kind, source_expr, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                let self_arg = if arguments.is_empty() {
                    None
                } else {
                    let MutRefOrImmutableExpression::Expression(as_expr) = &arguments[0] else {
                        panic!("not sure")
                    };
                    let self_region = self.gen_expression_for_access(as_expr)?;
                    Some(self_region)
                };
                let rest_args = &arguments[1..];
                self.gen_single_intrinsic_call(&expr.node, intrinsic_fn, self_arg, rest_args, ctx)
                    .map(|_| GeneratedExpressionResult::default())
            }

            ExpressionKind::Lambda(vec, x) => {
                todo!()
            }
            // --------- Not high prio
            ExpressionKind::CoerceOptionToBool(a) => self
                .gen_coerce_option_to_bool(&a, ctx)
                .map(|_| GeneratedExpressionResult::default()),
            ExpressionKind::FunctionValueCall(_, _, _) => todo!(),

            // --------- TO BE REMOVED
            ExpressionKind::IntrinsicFunctionAccess(_) => todo!(), // TODO: IntrinsicFunctionAccess should be reduced away in analyzer
            ExpressionKind::ExternalFunctionAccess(_) => todo!(), // TODO: ExternalFunctionAccess should be reduced away in analyzer
            ExpressionKind::VariableBinding(_, _) => todo!(),
        };

        result
    }

    fn gen_unary_operator(
        &mut self,
        unary_operator: &UnaryOperator,
        ctx: &Context,
    ) -> Result<(), Error> {
        let node = &unary_operator.node;
        match &unary_operator.kind {
            UnaryOperatorKind::Not => match &unary_operator.left.ty {
                Type::Bool => {
                    let left_source = self.gen_expression_for_access(&unary_operator.left)?;
                    self.state.builder.add_not_u8(
                        ctx.addr(),
                        left_source.addr,
                        node,
                        "negate bool",
                    );
                }
                _ => panic!("unknown not"),
            },
            UnaryOperatorKind::Negate => match (&unary_operator.left.ty) {
                Type::Int => {
                    let left_source = self.gen_expression_for_access(&unary_operator.left)?;
                    self.state.builder.add_neg_i32(
                        ctx.addr(),
                        left_source.addr,
                        node,
                        "negate i32",
                    );
                }

                Type::Float => {
                    let left_source = self.gen_expression_for_access(&unary_operator.left)?;
                    self.state.builder.add_neg_f32(
                        ctx.addr(),
                        left_source.addr,
                        node,
                        "negate f32",
                    );
                }
                _ => todo!(),
            },
            UnaryOperatorKind::BorrowMutRef => todo!(),
        }

        Ok(())
    }

    fn gen_binary_operator(
        &mut self,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        match (&binary_operator.left.ty, &binary_operator.right.ty) {
            (Type::Int, Type::Int) => self.gen_binary_operator_i32(binary_operator, ctx),
            (Type::Float, Type::Float) => self.gen_binary_operator_f32(binary_operator, ctx),
            (Type::Bool, Type::Bool) => self.gen_binary_operator_bool(binary_operator),
            (Type::String, Type::String) => self.gen_binary_operator_string(binary_operator, ctx),
            _ => todo!(),
        }
    }

    fn gen_binary_operator_i32(
        &mut self,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let node = &binary_operator.node;
        let left_source = self.gen_expression_for_access(&binary_operator.left)?;
        let right_source = self.gen_expression_for_access(&binary_operator.right)?;

        match binary_operator.kind {
            BinaryOperatorKind::Add => {
                self.state.builder.add_add_i32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "i32 add",
                );
            }

            BinaryOperatorKind::Subtract => self.state.builder.add_sub_i32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "i32 sub",
            ),
            BinaryOperatorKind::Multiply => {
                self.state.builder.add_mul_i32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "i32 add",
                );
            }
            BinaryOperatorKind::Divide => self.state.builder.add_div_i32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "i32 div",
            ),
            BinaryOperatorKind::Modulo => self.state.builder.add_mod_i32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "i32 mod",
            ),
            BinaryOperatorKind::LogicalOr => todo!(),
            BinaryOperatorKind::LogicalAnd => todo!(),
            BinaryOperatorKind::Equal => {
                self.state.builder.add_eq_32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "i32 eq",
                );
            }
            BinaryOperatorKind::NotEqual => todo!(),
            BinaryOperatorKind::LessThan => {
                self.state.builder.add_lt_i32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "i32 lt",
                );
            }
            BinaryOperatorKind::LessEqual => {
                self.state.builder.add_le_i32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "i32 le",
                );
            }
            BinaryOperatorKind::GreaterThan => {
                self.state.builder.add_gt_i32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "i32 gt",
                );
            }
            BinaryOperatorKind::GreaterEqual => {
                self.state.builder.add_ge_i32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "i32 ge",
                );
            }
        }

        Ok(GeneratedExpressionResult {
            has_set_bool_z_flag: true,
        })
    }

    fn gen_binary_operator_f32(
        &mut self,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let node = &binary_operator.node;
        let left_source = self.gen_expression_for_access(&binary_operator.left)?;
        let right_source = self.gen_expression_for_access(&binary_operator.right)?;

        match binary_operator.kind {
            BinaryOperatorKind::Add => {
                self.state.builder.add_add_f32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 add",
                );
            }

            BinaryOperatorKind::Subtract => self.state.builder.add_sub_f32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "f32 sub",
            ),
            BinaryOperatorKind::Multiply => {
                self.state.builder.add_mul_f32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 add",
                );
            }
            BinaryOperatorKind::Divide => {
                self.state.builder.add_div_f32(
                    ctx.addr(),
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 div",
                );
            }
            BinaryOperatorKind::Modulo => self.state.builder.add_mod_f32(
                ctx.addr(),
                left_source.addr(),
                right_source.addr(),
                node,
                "f32 mod",
            ),
            BinaryOperatorKind::LogicalOr => panic!("not supported"),
            BinaryOperatorKind::LogicalAnd => panic!("not supported"),
            BinaryOperatorKind::Equal => {
                self.state.builder.add_eq_32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 eq",
                );
            }
            BinaryOperatorKind::NotEqual => {
                self.state.builder.add_ne_32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 ne",
                );
            }
            BinaryOperatorKind::LessThan => {
                self.state.builder.add_lt_f32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 lt",
                );
            }
            BinaryOperatorKind::LessEqual => {
                self.state.builder.add_le_f32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 le",
                );
            }
            BinaryOperatorKind::GreaterThan => {
                self.state.builder.add_gt_f32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 gt",
                );
            }
            BinaryOperatorKind::GreaterEqual => {
                self.state.builder.add_ge_f32(
                    left_source.addr(),
                    right_source.addr(),
                    node,
                    "f32 ge",
                );
            }
        }

        Ok(GeneratedExpressionResult {
            has_set_bool_z_flag: true,
        })
    }

    fn gen_binary_operator_string(
        &mut self,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        let node = &binary_operator.node;
        let left_source = self.gen_expression_for_access(&binary_operator.left)?;
        let right_source = self.gen_expression_for_access(&binary_operator.right)?;

        match binary_operator.kind {
            BinaryOperatorKind::Add => {
                self.state.builder.add_string_append(
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
            has_set_bool_z_flag: false,
        })
    }

    fn gen_binary_operator_bool(
        &mut self,
        binary_operator: &BinaryOperator,
    ) -> Result<GeneratedExpressionResult, Error> {
        let node = &binary_operator.node;
        match binary_operator.kind {
            BinaryOperatorKind::LogicalOr => {
                // this updates the z flag
                self.gen_boolean_access_set_z_flag(&binary_operator.left);

                let jump_after_patch = self
                    .state
                    .builder
                    .add_jmp_if_equal_placeholder(node, "skip rhs `or` expression");

                // this updates the z flag
                self.gen_boolean_access_set_z_flag(&binary_operator.right);

                self.state.builder.patch_jump_here(jump_after_patch);
            }
            BinaryOperatorKind::LogicalAnd => {
                // this updates the z flag
                self.gen_boolean_access_set_z_flag(&binary_operator.left);

                let jump_after_patch = self
                    .state
                    .builder
                    .add_jmp_if_not_equal_placeholder(node, "skip rhs `and` expression");

                // this updates the z flag
                self.gen_boolean_access_set_z_flag(&binary_operator.right);

                self.state.builder.patch_jump_here(jump_after_patch);
            }

            BinaryOperatorKind::Equal => {
                let left = self.gen_expression_for_access(&binary_operator.left)?;
                let right = self.gen_expression_for_access(&binary_operator.right)?;
                self.state
                    .builder
                    .add_cmp8(left.addr, right.addr, node, "bool equals ==");
            }
            _ => {
                panic!("unknown operator {:?}", binary_operator);
            }
        }

        Ok(GeneratedExpressionResult {
            has_set_bool_z_flag: true,
        })
    }

    fn gen_condition_context(
        &mut self,
        condition: &BooleanExpression,
    ) -> Result<(Context, PatchPosition), Error> {
        let condition_ctx = self.extra_frame_space_for_type(&Type::Bool);
        self.gen_expression(&condition.expression, &condition_ctx)?;

        let jump_on_false_condition = self.state.builder.add_jmp_if_not_equal_placeholder(
            &condition.expression.node,
            "jump boolean condition false",
        );

        Ok((condition_ctx, jump_on_false_condition))
    }

    fn gen_boolean_access_set_z_flag(&mut self, condition: &Expression) -> Result<(), Error> {
        let (frame_memory_region, gen_result) =
            self.gen_expression_for_access_internal(condition)?;

        if !gen_result.has_set_bool_z_flag {
            self.state.builder.add_tst8(
                frame_memory_region.addr,
                &condition.node,
                "convert to boolean expression (update z flag)",
            );
        }

        Ok(())
    }

    fn gen_boolean_expression(&mut self, condition: &BooleanExpression) -> Result<(), Error> {
        self.gen_boolean_access_set_z_flag(&condition.expression)
    }

    fn gen_if(
        &mut self,
        condition: &BooleanExpression,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> Result<(), Error> {
        let (_condition_ctx, jump_on_false_condition) = self.gen_condition_context(condition)?;

        // True expression just takes over our target
        self.gen_expression(true_expr, ctx)?;

        if let Some(false_expr) = maybe_false_expr {
            // we need to help the true expression to jump over false
            let skip_false_if_true = self
                .state
                .builder
                .add_jump_placeholder(&condition.expression.node, "condition is false skip");

            // If the expression was false, it should continue here
            self.state.builder.patch_jump_here(jump_on_false_condition);

            // Else expression also can just take over our if target
            self.gen_expression(false_expr, ctx)?;

            self.state.builder.patch_jump_here(skip_false_if_true);
        } else {
            self.state.builder.patch_jump_here(jump_on_false_condition);
        }

        Ok(())
    }

    fn gen_while_loop(
        &mut self,
        condition: &BooleanExpression,
        expression: &Expression,
        ctx: &Context,
    ) -> Result<(), Error> {
        // `while` loops are only for side effects, make sure that the target size is zero (Unit)
        assert_eq!(ctx.target_size().0, 0);

        let ip_for_condition = self.state.builder.position();

        let (_condition_ctx, jump_on_false_condition) = self.gen_condition_context(condition)?;

        // Expression is only for side effects
        let mut unit_ctx = self.temp_space_for_type(&Type::Unit, "while body expression");
        self.gen_expression(expression, &mut unit_ctx)?;

        // Always jump to the condition again to see if it is true
        self.state
            .builder
            .add_jmp(ip_for_condition, &expression.node, "jmp to while condition");

        self.state.builder.patch_jump_here(jump_on_false_condition);

        Ok(())
    }

    fn gen_location_argument(
        &mut self,
        argument: &SingleLocationExpression,
        ctx: &Context,
        comment: &str,
    ) -> Result<(), Error> {
        let region = self.gen_lvalue_address(argument)?;

        self.state.builder.add_mov(
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
    ) -> Result<(), Error> {
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

        let _ = self.gen_expression(expression, &init_ctx)?;

        Ok(())
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
    ) -> Result<(), Error> {
        let lhs_addr = self.gen_lvalue_address(&lhs.0)?;
        let access = self.gen_expression_for_access(rhs)?;

        self.state
            .builder
            .add_mov(lhs_addr.addr, access.addr, access.size, node, "assignment");

        Ok(())
    }

    fn gen_variable_definition(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) -> Result<(), Error> {
        self.gen_variable_assignment(variable, expression, ctx)
    }

    fn gen_variable_reassignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) -> Result<(), Error> {
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

        let _argument_addr = Self::reserve(&signature.return_type, &mut arguments_allocator);

        let mut parameters = signature.parameters.clone();
        if let Some(found_self) = maybe_self {
            let source_region =
                Self::reserve(&parameters[0].resolved_type, &mut arguments_allocator);
            self.state.builder.add_mov(
                found_self.addr,
                source_region.addr,
                source_region.size,
                node,
                "copy back to <self>",
            );
            parameters.remove(0);
        }
        for (parameter, argument) in parameters.iter().zip(arguments) {
            let source_region = Self::reserve(&parameter.resolved_type, &mut arguments_allocator);
            if !parameter.is_mutable {
                continue;
            }

            if let MutRefOrImmutableExpression::Location(found_location) = argument {
                let argument_target = self.gen_lvalue_address(found_location)?;
                self.state.builder.add_mov(
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
        let argument_addr = Self::reserve(&signature.return_type, &mut self.argument_allocator);
        //assert_eq!(argument_addr.addr.0, self.frame_size.0);

        let mut argument_targets = Vec::new();
        let mut argument_comments = Vec::new();

        // Layout arguments, must be continuous space
        for (index, type_for_parameter) in signature.parameters.iter().enumerate() {
            let argument_target = Self::reserve(
                &type_for_parameter.resolved_type,
                &mut self.argument_allocator,
            );
            let arg_ctx = Context::new(argument_target);
            argument_targets.push(arg_ctx);
            argument_comments.push(format!("argument {}", type_for_parameter.name));
        }

        if let Some(push_self) = self_region {
            self.state.builder.add_mov(
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
        start_expression: &Expression,
        chain: &[Postfix],
        ctx: &Context,
    ) -> Result<(), Error> {
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
                        self.state
                            .add_call(internal_fn, &format!("frame size: {}", self.frame_size)); // will be fixed up later
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

        let mut start_source = self.gen_expression_for_access(start_expression)?;

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
                            if let Some(intrinsic_fn) = single_intrinsic_fn(&internal_fn.body) {
                                self.gen_single_intrinsic_call(
                                    &start_expression.node,
                                    intrinsic_fn,
                                    Some(start_source),
                                    arguments,
                                    ctx,
                                )?;
                            } else {
                                self.gen_arguments(
                                    &start_expression.node,
                                    &internal_fn.signature.signature,
                                    Some(start_source),
                                    arguments,
                                )?;
                                self.state.add_call(
                                    internal_fn,
                                    &format!("frame size: {}", self.frame_size),
                                ); // will be fixed up later

                                let (return_size, _alignment) = type_size_and_alignment(
                                    &internal_fn.signature.signature.return_type,
                                );
                                if return_size.0 != 0 {
                                    self.state.builder.add_mov(
                                        ctx.addr(),
                                        self.infinite_above_frame_size().addr,
                                        return_size,
                                        &start_expression.node,
                                        "copy the return value to destination",
                                    );
                                }

                                self.copy_back_mutable_arguments(
                                    &start_expression.node,
                                    &internal_fn.signature.signature,
                                    Some(start_source),
                                    arguments,
                                )?;
                            }
                        }
                        Function::External(external_fn) => {
                            //self.state.builder.add_host_call(external_fn.id);
                        }
                    }
                }
                PostfixKind::FunctionCall(arguments) => {
                    //self.gen_arguments(arguments);
                    //self.state.add_call(start_expression)
                }
                PostfixKind::OptionalChainingOperator => todo!(),
                PostfixKind::NoneCoalescingOperator(_) => todo!(),
            }
        }

        Ok(())
    }

    fn gen_tuple(&mut self, expressions: &[Expression], ctx: &Context) -> Result<(), Error> {
        let mut scope = ScopeAllocator::new(ctx.target());

        for expr in expressions {
            let (memory_size, alignment) = type_size_and_alignment(&expr.ty);
            let start_addr = scope.allocate(memory_size, alignment);
            let element_region = FrameMemoryRegion::new(start_addr, memory_size);
            let element_ctx = Context::new(element_region);
            self.gen_expression(expr, &element_ctx)?;
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
            self.gen_expression(expression, &field_ctx)?;
        }

        Ok(())
    }

    fn gen_literal(&mut self, node: &Node, literal: &Literal, ctx: &Context) -> Result<(), Error> {
        match literal {
            Literal::IntLiteral(int) => {
                self.state
                    .builder
                    .add_ld32(ctx.addr(), *int, node, "int literal");
            }
            Literal::FloatLiteral(fixed_point) => {
                self.state
                    .builder
                    .add_ld32(ctx.addr(), fixed_point.inner(), node, "float literal");
            }
            Literal::NoneLiteral => {
                self.state
                    .builder
                    .add_ld8(ctx.addr(), 0, node, "none literal");
            }
            Literal::BoolLiteral(truthy) => {
                self.state
                    .builder
                    .add_ld8(ctx.addr(), u8::from(*truthy), node, "bool literal");
            }

            Literal::EnumVariantLiteral(enum_type, a, b) => {
                self.state.builder.add_ld8(
                    ctx.addr(),
                    a.common().container_index,
                    node,
                    &format!("enum variant {} tag", a.common().assigned_name),
                );

                let starting_offset = MemoryOffset(1);

                let (data_size, data_alignment) = match a {
                    EnumVariantType::Struct(enum_variant_struct) => {
                        layout_struct(&enum_variant_struct.anon_struct)
                    }
                    EnumVariantType::Tuple(tuple_type) => layout_tuple(&tuple_type.fields_in_order),
                    EnumVariantType::Nothing(_) => (MemorySize(0), MemoryAlignment::U8),
                };

                let skip_octets: usize = data_alignment.into();
                let skip = MemorySize(skip_octets as u16);
                let inner_addr = ctx.addr().add(skip);
                let region = FrameMemoryRegion::new(inner_addr, data_size);
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
            Literal::Slice(ty, expressions) => {
                //self.gen_slice_literal(ty, expressions, ctx)
                todo!()
            }
            Literal::SlicePair(ty, expression_pairs) => {
                todo!()
            }
        }

        Ok(())
    }

    fn gen_string_literal(&mut self, node: &Node, string: &str, ctx: &Context) {
        let string_bytes = string.as_bytes();
        let string_byte_count = string_bytes.len();

        let data_ptr = self
            .state
            .constants
            .allocate(string_bytes, MemoryAlignment::U8);

        let mem_size = MemorySize(string_byte_count as u16);

        self.state.builder.add_string_from_constant_slice(
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
    ) -> Result<(), Error> {
        if let Some(found_value) = maybe_option {
            self.state
                .builder
                .add_ld8(ctx.addr(), 1, node, "option Some tag"); // 1 signals `Some`
            let (inner_size, inner_alignment) = type_size_and_alignment(&found_value.ty);
            let one_offset_ctx = ctx.with_offset(inner_alignment.into(), inner_size);

            self.gen_expression(found_value, &one_offset_ctx)?; // Fills in more of the union
        } else {
            self.state
                .builder
                .add_ld8(ctx.addr(), 0, node, "option None tag"); // 0 signals `None`
            // No real need to clear the rest of the memory
        }

        Ok(())
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
            .allocate(MemorySize(VEC_ITERATOR_SIZE), VEC_ITERATOR_ALIGNMENT);
        self.state.builder.add_vec_iter_init(
            temp_iterator_region,
            FrameMemoryAddressIndirectPointer(collection_region.addr),
            node,
            "initialize vec iterator",
        );

        let loop_ip = self.state.builder.position();

        let placeholder_position = match for_pattern {
            ForPattern::Single(variable) => {
                let target_variable = self
                    .variable_offsets
                    .get(&variable.unique_id_within_function)
                    .unwrap();
                self.state.builder.add_vec_iter_next_placeholder(
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
                self.state.builder.add_vec_iter_next_pair_placeholder(
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
        self.state.builder.add_map_iter_init(
            FrameMemoryAddress(0x80),
            FrameMemoryAddressIndirectPointer(FrameMemoryAddress(0xffff)),
            node,
            "initialize map iterator",
        );

        let jump_ip = self.state.builder.position();

        let patch_position = match for_pattern {
            ForPattern::Single(_) => self.state.builder.add_map_iter_next_placeholder(
                FrameMemoryAddress(0x80),
                FrameMemoryAddress(0x16),
                node,
                "move to next or jump over",
            ),
            ForPattern::Pair(_, _) => self.state.builder.add_map_iter_next_pair_placeholder(
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
        self.state.builder.add_map_iter_init(
            FrameMemoryAddress(0x80),
            FrameMemoryAddressIndirectPointer(FrameMemoryAddress(0xffff)),
            node,
            "initialize map iterator",
        );

        let jump_ip = self.state.builder.position();

        let patch_position = match for_pattern {
            ForPattern::Single(_) => self.state.builder.add_range_iter_next_placeholder(
                FrameMemoryAddress(0x80),
                FrameMemoryAddress(0x16),
                node,
                "move to next or jump over",
            ),
            ForPattern::Pair(_, _) => self.state.builder.add_range_iter_next_pair_placeholder(
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
    ) -> Result<(), Error> {
        // Add check if the collection is empty, to skip everything

        // get some kind of iteration pointer

        // check if it has reached its end

        let collection_type = &iterable.resolved_expression.ty();
        let (jump_ip, placeholder_position) = match collection_type {
            Type::String => {
                todo!();
            }
            Type::NamedStruct(named_type) => {
                let node = &named_type.name;
                if let Some(found_info) = is_vec(collection_type) {
                    self.gen_for_loop_vec(node, for_pattern, &iterable.resolved_expression)?
                } else if let Some(found_info) = is_map(collection_type) {
                    self.gen_for_loop_map(node, for_pattern)?
                } else if let Some(found_info) = is_range(collection_type) {
                    self.gen_for_loop_range(node, for_pattern)?
                } else if let Some(found_info) = is_stack(collection_type) {
                    self.gen_for_loop_range(node, for_pattern)?
                } else if let Some(found_info) = is_grid(collection_type) {
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
        self.gen_expression(closure, &unit_expr)?;

        self.state
            .builder
            .add_jmp(jump_ip, node, "jump to next iteration");
        // advance iterator pointer
        // jump to check if iterator pointer has reached its end
        self.state.builder.patch_jump_here(placeholder_position);

        Ok(())
    }

    fn gen_for_loop_for_vec(
        &mut self,
        element_type: &Type,
        vector_expr: Expression,
        ctx: &mut Context,
    ) -> Result<GeneratedExpressionResult, Error> {
        // get the vector that is referenced
        let vector_ctx = self.temp_space_for_type(&vector_expr.ty, "vector space");
        self.gen_expression(&vector_expr, &vector_ctx)

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

    fn gen_block(&mut self, expressions: &[Expression], ctx: &Context) -> Result<(), Error> {
        if let Some((last, others)) = expressions.split_last() {
            for expr in others {
                let temp_context = self.temp_space_for_type(&Type::Unit, "block target");
                self.gen_expression(expr, &temp_context)?;
            }
            self.gen_expression(last, ctx)?;
        }

        Ok(())
    }

    fn get_variable_region(&self, variable: &VariableRef) -> (FrameMemoryRegion, MemoryAlignment) {
        let frame_address = self
            .variable_offsets
            .get(&variable.unique_id_within_function)
            .unwrap();
        let (_size, align) = type_size_and_alignment(&variable.resolved_type);

        (*frame_address, align)
    }

    fn gen_variable_access(&mut self, variable: &VariableRef, ctx: &Context) -> Result<(), Error> {
        let (region, alignment) = self.get_variable_region(variable);
        self.state.builder.add_mov(
            ctx.addr(),
            region.addr,
            region.size,
            &variable.name,
            &format!(
                "variable access '{}' ({})",
                variable.assigned_name,
                ctx.comment()
            ),
        );

        Ok(())
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
        ctx: &Context,
    ) -> Result<(), Error> {
        let target_location = self.gen_lvalue_address(&target_location.0)?;

        let source_info = self.gen_expression_for_access(source)?;

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

        Ok(())
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
                self.state.builder.add_add_i32(
                    target.addr(),
                    target.addr(),
                    source_ctx.addr(),
                    node,
                    "+=  (i32)",
                );
            }
            CompoundOperatorKind::Sub => todo!(),
            CompoundOperatorKind::Mul => todo!(),
            CompoundOperatorKind::Div => todo!(),
            CompoundOperatorKind::Modulo => todo!(),
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
                self.state.builder.add_add_f32(
                    target.addr(),
                    target.addr(),
                    source_ctx.addr(),
                    node,
                    "+=  (f32)",
                );
            }
            CompoundOperatorKind::Sub => self.state.builder.add_sub_f32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "-=  (f32)",
            ),
            CompoundOperatorKind::Mul => self.state.builder.add_mul_f32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "*=  (f32)",
            ),
            CompoundOperatorKind::Div => self.state.builder.add_div_f32(
                target.addr(),
                target.addr(),
                source_ctx.addr(),
                node,
                "/=  (f32)",
            ),
            CompoundOperatorKind::Modulo => self.state.builder.add_mod_f32(
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
        self.state.builder.add_ld_u16(
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
    ) -> Result<(), Error> {
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
    ) -> Result<(), Error> {
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
            self.gen_expression(expression, &field_ctx)?;
        }

        Ok(())
    }

    fn gen_slice_literal(
        &mut self,
        ty: &Type,
        expressions: &Vec<Expression>,
    ) -> Result<FrameMemoryRegion, Error> {
        let (element_size, element_alignment) = type_size_and_alignment(ty);
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(element_size.0 * element_count);

        let start_frame_address_to_transfer = self
            .temp_allocator
            .allocate(total_slice_size, element_alignment);
        for (index, expr) in expressions.iter().enumerate() {
            let memory_offset = MemoryOffset((index as u16) * element_size.0);
            let region = FrameMemoryRegion::new(
                start_frame_address_to_transfer.advance(memory_offset),
                element_size,
            );
            let element_ctx = Context::new(region);
            self.gen_expression(expr, &element_ctx)?;
        }

        Ok(FrameMemoryRegion::new(
            start_frame_address_to_transfer,
            total_slice_size,
        ))
    }

    fn gen_slice_pair_literal(
        &mut self,
        slice_type: &Type,
        expressions: &[(Expression, Expression)],
    ) -> SlicePairInfo {
        let Type::SlicePair(key_type, value_type) = slice_type else {
            panic!("should have been slice pair type")
        };

        let constructed_tuple = Type::Tuple(vec![*key_type.clone(), *value_type.clone()]);

        let (key_size, key_alignment) = type_size_and_alignment(key_type);
        let (value_size, value_alignment) = type_size_and_alignment(value_type);
        let (element_size, tuple_alignment) = type_size_and_alignment(&constructed_tuple);
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(element_size.0 * element_count);

        let start_frame_address_to_transfer = self
            .temp_allocator
            .allocate(total_slice_size, tuple_alignment);

        for (index, (key_expr, value_expr)) in expressions.iter().enumerate() {
            let memory_offset = MemoryOffset((index as u16) * element_size.0);
            let key_region = FrameMemoryRegion::new(
                start_frame_address_to_transfer.advance(memory_offset),
                element_size,
            );
            let key_ctx = Context::new(key_region);
            self.gen_expression(key_expr, &key_ctx);

            let value_region = FrameMemoryRegion::new(
                start_frame_address_to_transfer.advance(memory_offset.add(key_size, key_alignment)),
                value_size,
            );
            let value_ctx = Context::new(value_region);
            self.gen_expression(value_expr, &value_ctx);
        }

        SlicePairInfo {
            addr: TempFrameMemoryAddress(start_frame_address_to_transfer),
            key_size,
            value_size,
            element_count: CountU16(element_count),
            element_size,
        }
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
        self.state
            .builder
            .add_ld_u16(vec_len_addr, element_count, node, "slice len");

        let vec_capacity_addr = ctx.addr().advance(MemoryOffset(2));
        self.state
            .builder
            .add_ld_u16(vec_capacity_addr, element_count, node, "slice capacity");

        let vec_element_size_addr = ctx.addr().advance(MemoryOffset(4));
        self.state.builder.add_ld_u16(
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
            let memory = self.gen_expression_for_access(found_expr)?;
            self.state.builder.add_vec_from_slice(
                ctx.addr(),
                memory.addr,
                MemorySize(0),
                CountU16(0),
                node,
                "create vec",
            );
        } else {
            panic!("vec_from_slice");
        }

        Ok(())
    }

    fn gen_match(&mut self, match_expr: &Match, ctx: &Context) -> Result<(), Error> {
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
                        self.state.builder.add_eq_u8_immediate(
                            region_to_match.addr,
                            enum_variant.common().container_index,
                            &match_expr.expression.node(),
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
                Some(self.state.builder.add_jmp_if_not_equal_placeholder(
                    &match_expr.expression.node(),
                    "placeholder for enum match",
                ))
            } else {
                None
            };

            let maybe_guard_skip = if let Some(guard) = maybe_guard {
                self.gen_boolean_expression(guard)?;
                // z flag should have been updated now

                Some(self.state.builder.add_jmp_if_not_equal_placeholder(
                    &match_expr.expression.node(),
                    "placeholder for skip guard",
                ))
            } else {
                None
            };

            self.gen_expression(&arm.expression, ctx)?;

            if !is_last {
                let jump_to_exit_placeholder = self
                    .state
                    .builder
                    .add_jump_placeholder(&match_expr.expression.node(), "jump to exit");
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
            }

            if let Some(skip) = maybe_skip_added {
                self.state.builder.patch_jump_here(skip);
            }
            if let Some(guard_skip) = maybe_guard_skip {
                self.state.builder.patch_jump_here(guard_skip);
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.state.builder.patch_jump_here(placeholder);
        }

        Ok(())
    }

    fn gen_guard(&mut self, guards: &Vec<Guard>, ctx: &Context) -> Result<(), Error> {
        let mut jump_to_exit_placeholders = Vec::new();
        for guard in guards {
            if let Some(condition) = &guard.condition {
                self.gen_boolean_expression(condition)?; // update z flag
                let skip_expression_patch = self
                    .state
                    .builder
                    .add_jmp_if_not_equal_placeholder(&guard.result.node, "guard condition");
                self.gen_expression(&guard.result, ctx)?;
                let jump_to_exit_placeholder = self
                    .state
                    .builder
                    .add_jump_placeholder(&guard.result.node, "jump to exit");
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
                self.state.builder.patch_jump_here(skip_expression_patch);
            } else {
                // _ -> wildcard
                self.gen_expression(&guard.result, ctx)?;
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.state.builder.patch_jump_here(placeholder);
        }

        Ok(())
    }

    fn gen_when(
        &mut self,
        bindings: &Vec<WhenBinding>,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> Result<(), Error> {
        let mut all_false_jumps = Vec::new();

        for binding in bindings {
            let (variable_region, _alignment) = self.get_variable_region(&binding.variable);

            let old_variable_region = self.gen_for_access_or_location(&binding.expr)?;

            self.state.builder.add_tst8(
                old_variable_region.addr,
                &binding.expr.node(),
                "check binding",
            );
            let patch = self
                .state
                .builder
                .add_jmp_if_not_equal_placeholder(&binding.expr.node(), "jump if none");
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
                    self.gen_expression_for_access(variable_access_expression)?;
                let alignment_offset: MemoryOffset = alignment.into();
                let some_value_region = FrameMemoryRegion::new(
                    old_variable_region.addr.advance(alignment_offset),
                    MemorySize(variable_region.size.0),
                );
                self.state.builder.add_movlp(
                    variable_region.addr,
                    some_value_region.addr,
                    some_value_region.size,
                    &binding.expr.node(),
                    "move from Some to value",
                );
            }
        }

        self.gen_expression(true_expr, ctx)?;
        let maybe_jump_over_false = if let Some(else_expr) = maybe_false_expr {
            Some(
                self.state
                    .builder
                    .add_jump_placeholder(&else_expr.node, "jump over false section"),
            )
        } else {
            None
        };

        for false_jump_patch in all_false_jumps {
            self.state.builder.patch_jump_here(false_jump_patch);
        }

        if let Some(else_expr) = maybe_false_expr {
            self.gen_expression(else_expr, ctx)?;
            self.state
                .builder
                .patch_jump_here(maybe_jump_over_false.unwrap());
        }

        Ok(())
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
        target_variables: &Vec<VariableRef>,
        tuple_type: &Vec<Type>,
        source_tuple_expression: &Expression,
    ) -> Result<(), Error> {
        let source_region = self.gen_expression_for_access(source_tuple_expression)?;

        let (total_size, _max_alignment, element_offsets) = layout_tuple_elements(tuple_type);
        assert_eq!(total_size.0, source_region.size.0);

        for (target_variable, (element_offset, element_size)) in
            target_variables.iter().zip(element_offsets)
        {
            if target_variable.is_unused {
            } else {
                let (target_region, _variable_alignment) =
                    self.get_variable_region(target_variable);
                assert_eq!(target_region.size.0, element_size.0);

                let source_element_region = FrameMemoryRegion::new(
                    source_region.addr.advance(element_offset),
                    element_size,
                );
                self.state.builder.add_mov(
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

        Ok(())
    }

    fn gen_constant_access(
        &mut self,
        constant_reference: &ConstantRef,
        ctx: &Context,
    ) -> Result<(), Error> {
        let constant_region = self
            .state
            .constant_offsets
            .get(&constant_reference.id)
            .unwrap();
        assert_eq!(constant_region.size.0, ctx.target_size().0);

        self.state.builder.add_ld_constant(
            ctx.addr(),
            constant_region.addr,
            constant_region.size,
            &constant_reference.name,
            &format!("load constant '{}'", constant_reference.assigned_name),
        );

        Ok(())
    }

    fn gen_coerce_option_to_bool(&mut self, expr: &Expression, ctx: &Context) -> Result<(), Error> {
        let region = self.gen_expression_for_access(expr)?;
        self.state.builder.add_mov(
            ctx.addr(),
            region.addr,
            MemorySize(1),
            &expr.node,
            "move option tag to bool",
        );

        Ok(())
    }

    fn add_frame_relative_info(&mut self, region: FrameMemoryRegion, kind: FrameRelativeInfoKind) {
        self.frame_memory_infos.push(FrameRelativeInfo {
            frame_memory_region: region,
            kind,
        });
    }
}

fn single_intrinsic_fn(body: &Expression) -> Option<&IntrinsicFunction> {
    let ExpressionKind::Block(block_expressions) = &body.kind else {
        panic!("function body should be a block")
    };

    if let ExpressionKind::IntrinsicCallEx(found_intrinsic_fn, _non_instantiated_arguments) =
        &block_expressions[0].kind
    {
        Some(found_intrinsic_fn)
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
