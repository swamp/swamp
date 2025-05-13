use crate::alloc::ScopeAllocator;
use crate::ctx::Context;
use crate::layout::{
    layout_enum_into_tagged_union, layout_optional_type, layout_struct_type, layout_tuple_items,
    layout_type,
};
use crate::reg_pool::{HwmTempRegisterPool, RegisterPool, TempRegister};
use crate::state::{CodeGenState, FunctionFixup};
use crate::{
    Collection, DetailedLocation, DetailedLocationResolved, GeneratedExpressionResult,
    GeneratedExpressionResultKind, SpilledRegister, SpilledRegisterRegion, Transformer,
    TransformerResult, single_intrinsic_fn,
};
use seq_map::SeqMap;
use source_map_cache::{SourceMapLookup, SourceMapWrapper};
use source_map_node::Node;
use std::os::macos::raw::stat;
use swamp_semantic::{
    AnonymousStructLiteral, BinaryOperator, BinaryOperatorKind, BooleanExpression,
    CompoundOperatorKind, ConstantId, ConstantRef, EnumLiteralData, Expression, ExpressionKind,
    ExternalFunctionDefinitionRef, ForPattern, Function, Guard, InternalFunctionDefinitionRef,
    Iterable, Literal, Match, MutRefOrImmutableExpression, NormalPattern, Pattern, Postfix,
    PostfixKind, SingleLocationExpression, StartOfChain, StartOfChainKind,
    TargetAssignmentLocation, UnaryOperator, UnaryOperatorKind, VariableRef, WhenBinding,
};
use swamp_types::{AnonymousStructType, EnumVariantType, Signature, Type};
use swamp_vm_instr_build::{InstructionBuilder, InstructionBuilderState, PatchPosition};
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, FramePlacedType, HeapPlacedType, TypedRegister, VariableRegister,
    VmType, int_type, pointer_type, u8_type, u32_type, unit_type, unknown_type,
};
use swamp_vm_types::{
    FrameMemoryAddress, FrameMemoryRegion, FrameMemorySize, HeapMemoryAddress, HeapMemoryOffset,
    HeapMemorySize, InstructionPosition, MemoryAlignment, MemoryOffset, MemorySize,
    REG_ON_FRAME_ALIGNMENT, REG_ON_FRAME_SIZE, SLICE_HEADER_SIZE, SLICE_PAIR_HEADER_SIZE,
    SLICE_PTR_OFFSET, STRING_PTR_SIZE, StringHeader, VEC_PTR_SIZE,
};
use tracing::field::debug;
use tracing::{error, info};

struct MutableReturnReg {
    pub target_location_after_call: DetailedLocation,
    pub parameter_reg: TypedRegister,
}

pub(crate) struct CodeBuilder<'a> {
    pub state: &'a mut CodeGenState,
    pub(crate) builder: &'a mut InstructionBuilder<'a>,
    pub(crate) variable_registers: SeqMap<usize, TypedRegister>,
    frame_memory_registers: RegisterPool,
    pub(crate) temp_registers: HwmTempRegisterPool,
    frame_allocator: ScopeAllocator,
    pub source_map_lookup: &'a SourceMapWrapper<'a>,
}

impl<'a> CodeBuilder<'a> {}

impl<'a> CodeBuilder<'a> {
    pub fn new(
        state: &'a mut CodeGenState,
        builder: &'a mut InstructionBuilder<'a>,
        variable_registers: SeqMap<usize, TypedRegister>,
        frame_memory_registers: RegisterPool,
        temp_registers: HwmTempRegisterPool,
        temp_allocator: ScopeAllocator,
        source_map_lookup: &'a source_map_cache::SourceMapWrapper<'a>,
    ) -> Self {
        Self {
            state,
            builder,
            variable_registers,
            frame_memory_registers,
            temp_registers,
            frame_allocator: temp_allocator,
            source_map_lookup,
        }
    }
}
impl CodeBuilder<'_> {
    fn emit_copy_register(
        &mut self,
        target_reg: &TypedRegister,
        source_reg: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        if source_reg.ty.is_mutable_reference_semantic() {
            if target_reg.ty().is_mutable_reference() {
                self.builder.add_mov_reg(
                    &target_reg,
                    &source_reg,
                    node,
                    &format!("emit_copy_register. ptr to ptr. {}", comment),
                );
            } else {
                let size = source_reg.size();

                self.builder.add_block_copy_with_offset(
                    &target_reg,
                    MemoryOffset(0),
                    &source_reg,
                    MemoryOffset(0),
                    size,
                    node,
                    &format!("emit_copy_register.copy struct. {}", comment),
                );
            }
        } else {
            self.builder.add_mov_reg(
                &target_reg,
                &source_reg,
                node,
                &format!("emit_copy_register. primitive to primitive. {}", comment),
            );
        }
    }

    pub(crate) fn add_ld_regs_from_frame(
        &mut self,
        start_reg: &TypedRegister,
        start_address: FrameMemoryAddress,
        count: u8,
        node: &Node,
        comment: &str,
    ) {
        self.builder
            .add_ld_regs_from_frame(start_reg, start_address, count, node, comment);
    }

    pub fn total_frame_size(&self) -> FrameMemorySize {
        self.frame_allocator.addr().as_size()
    }

    pub fn patch_enter(&mut self, patch_position: PatchPosition) {
        self.builder
            .patch_enter(self.total_frame_size(), patch_position);
    }

    fn emit_load_primitive_from_absolute_memory_address(
        &mut self,
        target_reg: &TypedRegister,
        source_offset: HeapMemoryAddress,
        type_at_offset: &VmType,
        node: &Node,
        comment: &str,
    ) {
        match type_at_offset.basic_type.kind {
            BasicTypeKind::Empty => {
                // No need to copy, it has zero size
            }
            BasicTypeKind::U8 | BasicTypeKind::B8 => {
                self.builder.add_ld8_from_absolute_memory_address(
                    target_reg,
                    &source_offset,
                    node,
                    &format!("{} - load u8 primitive from memory", comment),
                );
            }
            BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                self.builder.add_ld32_from_absolute_memory_address(
                    target_reg,
                    &source_offset,
                    node,
                    &format!("{} - load u32 primitive from memory", comment),
                );
            }
            _ => panic!("this is not a primitive {type_at_offset:?}"),
        }
    }

    fn emit_load_primitive_from_memory(
        &mut self,
        target_reg: &TypedRegister,
        base_ptr_reg: &TypedRegister,
        source_offset: MemoryOffset,
        type_at_offset: &VmType,
        node: &Node,
    ) {
        match type_at_offset.basic_type.kind {
            BasicTypeKind::Empty => {
                // No need to copy, it has zero size
            }
            BasicTypeKind::U8 | BasicTypeKind::B8 => {
                self.builder.add_ld8_from_pointer_with_offset_u16(
                    target_reg,
                    base_ptr_reg,
                    source_offset,
                    node,
                    "load u8 primitive from memory",
                );
            }
            BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                self.builder.add_ld32_from_pointer_with_offset_u16(
                    target_reg,
                    base_ptr_reg,
                    source_offset,
                    node,
                    "load u32 primitive from memory",
                );
            }
            _ => panic!("this is not a primitive {type_at_offset:?}"),
        }
    }

    fn emit_load_from_memory(
        &mut self,
        target_reg: &TypedRegister,
        base_ptr_reg: &TypedRegister,
        source_offset: MemoryOffset,
        type_at_offset: &VmType,
        node: &Node,
        comment: &str,
    ) {
        let source_type = type_at_offset;
        if source_type.is_represented_as_pointer_inside_register() {
            if target_reg.ty().is_mutable_reference() {
                self.builder.add_load_primitive(
                    target_reg,
                    base_ptr_reg,
                    source_offset,
                    node,
                    &format!("emit_copy_register. ptr to ptr (mutable reference). {comment}"),
                );
            } else {
                let size = target_reg.size();

                self.builder.add_block_copy_with_offset(
                    target_reg,
                    MemoryOffset(0),
                    base_ptr_reg,
                    source_offset,
                    size,
                    node,
                    &format!("block copy {comment}"),
                );
            }
        } else {
            self.builder.add_load_primitive(
                target_reg,
                base_ptr_reg,
                source_offset,
                node,
                &format!("emit primitive value. ptr to primitive reg {comment}"),
            );
        }
    }

    fn emit_load_from_location(
        &mut self,
        target_reg: &TypedRegister,
        location: &DetailedLocation,
        node: &Node,
        comment: &str,
    ) {
        match location {
            DetailedLocation::Register { reg } => {
                self.emit_copy_register(target_reg, reg, node, comment);
            }
            DetailedLocation::Memory {
                base_ptr_reg,
                offset,
                ty,
            } => {
                self.emit_load_from_memory(
                    target_reg,
                    base_ptr_reg,
                    *offset,
                    ty,
                    node,
                    "load from location",
                );
            }
        }
    }

    fn emit_load_primitive_from_detailed_location_if_needed(
        &mut self,
        location: &DetailedLocation,
        node: &Node,
        comment: &str,
    ) -> DetailedLocationResolved {
        match location {
            DetailedLocation::Register { reg } => DetailedLocationResolved::Register(reg.clone()),
            DetailedLocation::Memory {
                base_ptr_reg,
                offset,
                ty,
            } => {
                let temp_reg_target = self
                    .temp_registers
                    .allocate(ty.clone(), "emit load primitive from location");
                self.emit_load_from_memory(
                    temp_reg_target.register(),
                    base_ptr_reg,
                    *offset,
                    ty,
                    node,
                    &format!("load primitive from detailed location {comment}"),
                );
                DetailedLocationResolved::TempRegister(temp_reg_target)
            }
        }
    }

    pub fn emit_ptr_reg_from_base_and_offset(
        &mut self,
        target_reg: &TypedRegister,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        node: &Node,
        comment: &str,
    ) {
        let hwm = self.temp_registers.save_mark();
        let offset_temp_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(u32_type()),
            "emit_ptr_reg_from_location_offset",
        );
        self.builder.add_mov_32_immediate_value(
            offset_temp_reg.register(),
            offset.0 as u32,
            node,
            "load offset",
        );
        self.builder.add_add_u32(
            &target_reg,
            &base_ptr_reg,
            offset_temp_reg.register(),
            node,
            "add base pointer to target reg",
        );

        self.temp_registers.restore_to_mark(hwm);
    }

    pub(crate) fn emit_ptr_reg_from_detailed_location(
        &mut self,
        location: DetailedLocation,
        node: &Node,
        comment: &str,
    ) -> (TypedRegister, Option<TempRegister>) {
        match location {
            DetailedLocation::Register { reg } => (reg, None),
            DetailedLocation::Memory {
                base_ptr_reg,
                offset,
                ty,
            } => {
                let hwm = self.temp_registers.save_mark();
                let offset_temp_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u32_type()),
                    "emit_ptr_reg_from_location_offset",
                );
                self.builder.add_mov_32_immediate_value(
                    offset_temp_reg.register(),
                    offset.0 as u32,
                    node,
                    "load offset",
                );
                let final_ptr_target_reg = self.temp_registers.allocate(ty, "final_ptr_target_reg");
                self.builder.add_add_u32(
                    &base_ptr_reg,
                    &base_ptr_reg,
                    offset_temp_reg.register(),
                    node,
                    "add base pointer reg",
                );
                self.temp_registers.restore_to_mark(hwm);
                (
                    final_ptr_target_reg.register().clone(),
                    Some(final_ptr_target_reg),
                )
            }
        }
    }

    fn emit_store_primitive_from_detailed_location_if_needed(
        &mut self,
        location: &DetailedLocationResolved,
        original: &DetailedLocation,
        node: &Node,
    ) {
        match location {
            DetailedLocationResolved::Register(_) => {} // intentionally do nothing, it is in a register
            DetailedLocationResolved::TempRegister(temp_reg) => self
                .store_register_contents_to_memory(
                    node,
                    original.base_register().unwrap(),
                    original.offset().unwrap(),
                    temp_reg.register(),
                    "copy back to memory",
                ),
        }
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

    pub fn emit_expression(
        &mut self,
        target_reg: &TypedRegister,
        expr: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        //self.debug_node(&expr.node);
        //        info!(t=?expr.ty, v=?expr.kind, "emit_expression");

        match &expr.kind {
            ExpressionKind::TupleDestructuring(variables, tuple_types, tuple_expression) => {
                self.emit_tuple_destructuring(variables, tuple_types, tuple_expression, ctx)
            }
            ExpressionKind::Assignment(target_mut_location_expr, source_expr) => {
                self.emit_assignment(&expr.node, target_mut_location_expr, source_expr, ctx)
            }
            ExpressionKind::VariableDefinition(variable, expression) => {
                self.emit_variable_definition(variable, expression, ctx)
            }

            ExpressionKind::VariableReassignment(variable, expression) => {
                self.emit_variable_reassignment(variable, expression, ctx)
            }
            ExpressionKind::ForLoop(for_pattern, collection, lambda_expr) => {
                self.emit_for_loop(&expr.node, for_pattern, collection, lambda_expr, ctx)
            }
            ExpressionKind::WhileLoop(condition, expression) => {
                self.emit_while_loop(condition, expression, ctx)
            }

            ExpressionKind::ConstantAccess(constant_ref) => {
                self.emit_constant_access(target_reg, &expr.node, constant_ref, ctx)
            }

            ExpressionKind::CompoundAssignment(target_location, operator_kind, source_expr) => {
                self.compound_assignment(target_location, operator_kind, source_expr, ctx)
            }
            ExpressionKind::VariableAccess(variable_ref) => {
                self.emit_variable_access_as_rvalue(target_reg, &expr.node, variable_ref, ctx)
            }
            ExpressionKind::BorrowMutRef(expression) => {
                self.emit_borrow_mutable_reference(target_reg, &expr.node, expression, ctx)
            }
            ExpressionKind::BinaryOp(operator) => {
                self.emit_binary_operator(target_reg, operator, ctx)
            }
            ExpressionKind::UnaryOp(operator) => {
                self.emit_unary_operator(target_reg, operator, ctx)
            }
            ExpressionKind::PostfixChain(start, chain) => {
                self.emit_rvalue_postfix_chain(target_reg, start, chain, ctx)
            }
            ExpressionKind::AnonymousStructLiteral(anon_struct) => self
                .emit_anonymous_struct_literal(target_reg, anon_struct, &expr.ty, &expr.node, ctx),
            ExpressionKind::Literal(basic_literal) => {
                self.emit_literal(target_reg, &expr.node, basic_literal, ctx)
            }
            ExpressionKind::Option(maybe_option) => {
                self.emit_option_expression(target_reg, &expr.node, maybe_option.as_deref(), ctx)
            }

            ExpressionKind::Block(expressions) => self.emit_block(target_reg, expressions, ctx),
            ExpressionKind::Match(match_expr) => self.emit_match(target_reg, match_expr, ctx),
            ExpressionKind::Guard(guards) => self.emit_guard(target_reg, guards, ctx),
            ExpressionKind::If(conditional, true_expr, false_expr) => self.emit_if(
                target_reg,
                conditional,
                true_expr,
                false_expr.as_deref(),
                ctx,
            ),
            ExpressionKind::When(bindings, true_expr, false_expr) => {
                self.emit_when(target_reg, bindings, true_expr, false_expr.as_deref(), ctx)
            }

            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => self
                .emit_single_intrinsic_call(target_reg, &expr.node, intrinsic_fn, arguments, ctx),
            ExpressionKind::CoerceOptionToBool(a) => {
                self.emit_coerce_option_to_bool(target_reg, a, ctx)
            }
            ExpressionKind::InternalCall(internal, arguments) => self.emit_internal_call(
                Option::from(target_reg),
                &expr.node,
                internal,
                arguments,
                ctx,
            ),
            ExpressionKind::HostCall(host_fn, arguments) => {
                self.emit_host_call(&expr.node, host_fn, arguments, ctx)
            }

            // Low priority
            ExpressionKind::VariableBinding(_, _) => todo!(), // only used for `when` expressions

            // Illegal
            ExpressionKind::Lambda(_vec, _x) => {
                panic!("something went wrong. non-capturing lambdas can not be evaluated")
            }
            _ => panic!("not an expression, probably a statement {:?} ", expr.kind),
        }
    }

    fn emit_unary_operator(
        &mut self,
        target_reg: &TypedRegister,
        unary_operator: &UnaryOperator,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let node = &unary_operator.node;
        let result = match &unary_operator.kind {
            UnaryOperatorKind::Not => match &unary_operator.left.ty {
                Type::Bool => {
                    let bool_result = self.emit_expression_to_z_flag(&unary_operator.left, ctx);
                    bool_result.invert_polarity()
                }
                _ => panic!("unknown not"),
            },
            UnaryOperatorKind::Negate => match &unary_operator.left.ty {
                Type::Int => {
                    let left_source = self.emit_rvalue(&unary_operator.left, ctx);
                    self.builder
                        .add_neg_i32(target_reg, &left_source, node, "negate i32");
                    GeneratedExpressionResult::default()
                }

                Type::Float => {
                    let left_source = self.emit_rvalue(&unary_operator.left, ctx);
                    self.builder
                        .add_neg_f32(target_reg, &left_source, node, "negate f32");
                    GeneratedExpressionResult::default()
                }
                _ => panic!("negate should only be possible on Int and Float"),
            },
        };

        result
    }

    fn emit_binary_operator(
        &mut self,
        target_reg: &TypedRegister,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        //info!(left=?binary_operator.left.ty, right=?binary_operator.right.ty, "binary_op");

        match &binary_operator.kind {
            BinaryOperatorKind::LogicalOr | BinaryOperatorKind::LogicalAnd => {
                self.emit_binary_operator_logical(binary_operator, ctx)
            }
            _ => self.emit_binary_operator_normal(target_reg, binary_operator, ctx),
        }
    }

    fn emit_binary_operator_normal(
        &mut self,
        target_reg: &TypedRegister,
        binary_operator: &BinaryOperator,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let hwm = self.temp_registers.save_mark();

        let left_temp_dest = self.temp_space_for_type(&binary_operator.left.ty, "binary op left");
        let left_source = left_temp_dest.register();
        self.emit_expression_materialize(left_temp_dest.register(), &binary_operator.left, ctx);

        let right_temp_dest =
            self.temp_space_for_type(&binary_operator.right.ty, "binary op right");
        let right_source = right_temp_dest.register();
        self.emit_expression_materialize(right_temp_dest.register(), &binary_operator.right, ctx);

        let result = match &binary_operator.kind {
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => {
                let polarity = match (&binary_operator.left.ty, &binary_operator.right.ty) {
                    (Type::Bool, Type::Bool) => self.emit_binary_operator_reg(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    (Type::Int, Type::Int) => self.emit_binary_operator_reg(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    (Type::Float, Type::Float) => self.emit_binary_operator_reg(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    (Type::String, Type::String) => self.emit_binary_operator_string_cmp(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    (Type::Enum(a), Type::Enum(b)) => self.emit_binary_operator_block_cmp(
                        &left_source,
                        &binary_operator.node,
                        &right_source,
                    ),
                    _ => todo!(),
                };

                if matches!(binary_operator.kind, BinaryOperatorKind::Equal) {
                    polarity
                } else {
                    polarity.invert_polarity()
                }
            }
            _ => match (&binary_operator.left.ty, &binary_operator.right.ty) {
                //(Type::Bool, Type::Bool) => self.emit_binary_operator_logical(binary_operator),
                (Type::Int, Type::Int) => self.emit_binary_operator_i32(
                    target_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                (Type::Float, Type::Float) => self.emit_binary_operator_f32(
                    target_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                (Type::String, Type::String) => self.emit_binary_operator_string(
                    target_reg,
                    &left_source,
                    &binary_operator.node,
                    &binary_operator.kind,
                    &right_source,
                    ctx,
                ),
                _ => todo!(),
            },
        };

        self.temp_registers.restore_to_mark(hwm);

        result
    }

    fn emit_binary_operator_i32(
        &mut self,
        target_reg: &TypedRegister,
        left_source: &TypedRegister,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: &TypedRegister,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let mut kind = GeneratedExpressionResultKind::TFlagIsIndeterminate;
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder.add_add_u32(
                    // u32 is the same as i32 when it comes to wrapping_add
                    target_reg,
                    left_source,
                    right_source,
                    node,
                    "i32 add",
                );
            }
            BinaryOperatorKind::Subtract => {
                self.builder.add_sub_u32(
                    target_reg,
                    left_source,
                    right_source,
                    node,
                    &format!("i32 sub {target_reg:?} = {left_source:?} - {right_source:?}"),
                );
            }
            BinaryOperatorKind::Multiply => {
                self.builder
                    .add_mul_i32(target_reg, left_source, right_source, node, "i32 mul");
            }
            BinaryOperatorKind::Divide => {
                self.builder
                    .add_div_i32(target_reg, left_source, right_source, node, "i32 div")
            }
            BinaryOperatorKind::Modulo => {
                self.builder
                    .add_mod_i32(target_reg, left_source, right_source, node, "i32 mod")
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
                kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_i32(left_source, right_source, node, "i32 le");
                kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_i32(left_source, right_source, node, "i32 gt");
                kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_i32(left_source, right_source, node, "i32 ge");
                kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
            }
        }

        GeneratedExpressionResult { kind }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn emit_binary_operator_f32(
        &mut self,
        target_reg: &TypedRegister,
        left_source: &TypedRegister,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: &TypedRegister,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let mut kind = GeneratedExpressionResultKind::TFlagIsIndeterminate;
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder
                    .add_add_f32(target_reg, left_source, right_source, node, "f32 add");
            }
            BinaryOperatorKind::Subtract => {
                self.builder
                    .add_sub_f32(target_reg, left_source, right_source, node, "f32 sub")
            }
            BinaryOperatorKind::Multiply => {
                self.builder
                    .add_mul_f32(target_reg, left_source, right_source, node, "f32 mul");
            }
            BinaryOperatorKind::Divide => {
                self.builder
                    .add_div_f32(target_reg, left_source, right_source, node, "f32 div");
            }
            BinaryOperatorKind::Modulo => {
                self.builder
                    .add_mod_f32(target_reg, left_source, right_source, node, "f32 mod")
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
                    .add_lt_i32(left_source, right_source, node, "f32 lt");
                kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
            }
            BinaryOperatorKind::LessEqual => {
                self.builder
                    .add_le_i32(left_source, right_source, node, "f32 le");
                kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
            }
            BinaryOperatorKind::GreaterThan => {
                self.builder
                    .add_gt_i32(left_source, right_source, node, "f32 gt");
                kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
            }
            BinaryOperatorKind::GreaterEqual => {
                self.builder
                    .add_ge_i32(left_source, right_source, node, "f32 ge");
                kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
            }
        }

        GeneratedExpressionResult { kind }
    }

    fn emit_binary_operator_string(
        &mut self,
        target_reg: &TypedRegister,
        left_source: &TypedRegister,
        node: &Node,
        binary_operator_kind: &BinaryOperatorKind,
        right_source: &TypedRegister,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        match binary_operator_kind {
            BinaryOperatorKind::Add => {
                self.builder.add_string_append(
                    target_reg,
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
            kind: GeneratedExpressionResultKind::TFlagIsIndeterminate,
        }
    }

    fn emit_binary_operator_cmp8(
        &mut self,
        left_source: &TypedRegister,
        node: &Node,
        right_source: &TypedRegister,
    ) -> GeneratedExpressionResult {
        assert_eq!(left_source.size().0, 1);
        assert_eq!(right_source.size().0, 1);
        self.builder
            .add_cmp_reg(left_source, right_source, &node, "compare bool");

        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_binary_operator_reg(
        &mut self,
        left_source: &TypedRegister,
        node: &Node,
        right_source: &TypedRegister,
    ) -> GeneratedExpressionResult {
        self.builder
            .add_cmp_reg(left_source, right_source, node, "compare to z flag");

        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_binary_operator_string_cmp(
        &mut self,
        left_source: &TypedRegister,
        node: &Node,
        right_source: &TypedRegister,
    ) -> GeneratedExpressionResult {
        self.builder
            .add_string_cmp(left_source, right_source, node, "compare strings");

        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_binary_operator_block_cmp(
        &mut self,
        left_source: &TypedRegister,
        node: &Node,
        right_source: &TypedRegister,
    ) -> GeneratedExpressionResult {
        self.builder.add_block_cmp(
            left_source,
            right_source,
            left_source.size(),
            &node,
            "compare block",
        );

        GeneratedExpressionResult {
            kind: GeneratedExpressionResultKind::TFlagIsTrueWhenSet,
        }
    }

    fn emit_binary_operator_logical(
        &mut self,
        binary_operator: &BinaryOperator,
        context: &Context,
    ) -> GeneratedExpressionResult {
        let node = &binary_operator.node;

        // the logical is always normalized
        let kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;

        match binary_operator.kind {
            BinaryOperatorKind::LogicalOr => {
                self.emit_expression_to_normalized_z_flag(&binary_operator.left, context);

                let jump_after_patch = self
                    .builder
                    .add_jmp_if_true_placeholder(node, "OR: skip rhs because lhs is true");

                self.emit_expression_to_normalized_z_flag(&binary_operator.right, context);

                self.builder.patch_jump_here(jump_after_patch);
            }
            BinaryOperatorKind::LogicalAnd => {
                self.emit_expression_to_normalized_z_flag(&binary_operator.left, context);

                let jump_after_patch = self
                    .builder
                    .add_jmp_if_not_true_placeholder(node, "AND: skip rhs because lhs is false");

                self.emit_expression_to_normalized_z_flag(&binary_operator.right, context);

                self.builder.patch_jump_here(jump_after_patch);
            }

            _ => {
                panic!("unknown operator {binary_operator:?}");
            }
        }

        GeneratedExpressionResult { kind }
    }

    fn emit_condition_context(
        &mut self,
        condition: &BooleanExpression,
        ctx: &Context,
    ) -> PatchPosition {
        let result = self.emit_expression_to_z_flag(&condition.expression, ctx);

        let jump_on_false_condition = self.builder.add_jmp_if_not_equal_polarity_placeholder(
            &result.polarity(),
            &condition.expression.node,
            "jump boolean condition false",
        );

        jump_on_false_condition
    }

    fn emit_expression_to_z_flag(
        &mut self,
        condition: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        match &condition.kind {
            ExpressionKind::CoerceOptionToBool(option_union_expr) => {
                let region = self.emit_rvalue(option_union_expr, ctx);
                // We can shortcut this, since we know that the tag location is basically a bool value
                self.builder.add_tst_u8(
                    &region,
                    &option_union_expr.node,
                    "shortcut directly to z-flag",
                );
                return GeneratedExpressionResult {
                    kind: GeneratedExpressionResultKind::TFlagIsTrueWhenSet,
                };
            }
            _ => {}
        }

        let (reg, mut gen_result) = self.emit_rvalue_leave_z_flag_if_possible(condition, ctx);

        if gen_result.kind == GeneratedExpressionResultKind::TFlagIsIndeterminate {
            self.builder.add_tst_u8(
                &reg,
                &condition.node,
                "convert to boolean expression (update z flag)",
            );
            gen_result.kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
        }

        gen_result
    }

    fn emit_expression_to_normalized_z_flag(&mut self, condition: &Expression, ctx: &Context) {
        let result = self.emit_expression_to_z_flag(condition, ctx);
        assert_ne!(
            result.kind,
            GeneratedExpressionResultKind::TFlagIsIndeterminate
        );

        if result.kind == GeneratedExpressionResultKind::TFlagIsTrueWhenClear {
            self.builder
                .add_not_z(&condition.node, "normalized z is required");
        }
    }

    fn emit_boolean_expression_z_flag(
        &mut self,
        condition: &BooleanExpression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        self.emit_expression_to_z_flag(&condition.expression, ctx)
    }

    fn emit_if(
        &mut self,
        target_reg: &TypedRegister,
        condition: &BooleanExpression,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let jump_on_false_condition = self.emit_condition_context(condition, ctx);

        // True expression just takes over our target
        // Both to reuse the current target, and for the fact when there is no else
        self.emit_expression_materialize(target_reg, true_expr, ctx);

        if let Some(false_expr) = maybe_false_expr {
            // we need to help the true expression to jump over false
            let skip_false_if_true = self.builder.add_jump_placeholder(
                &condition.expression.node,
                "since it was true, skip over false section",
            );

            // If the expression was false, it should continue here
            self.builder.patch_jump_here(jump_on_false_condition);

            // Else expression also can just take over our if target
            self.emit_expression_materialize(target_reg, false_expr, ctx);

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
        //assert_eq!(target_reg.size.0, 0);

        let hwm = self.temp_registers.save_mark();

        let ip_for_condition = self.builder.position();

        let jump_on_false_condition = self.emit_condition_context(condition, ctx);

        // Expression is only for side effects
        let temp_reg = self.temp_space_for_type(&Type::Unit, "while body expression");
        self.emit_expression_materialize(temp_reg.register(), expression, ctx);

        // Always jump to the condition again to see if it is true
        self.builder
            .add_jmp(ip_for_condition, &expression.node, "jmp to while condition");

        self.temp_registers.restore_to_mark(hwm);

        self.builder.patch_jump_here(jump_on_false_condition);

        GeneratedExpressionResult::default()
    }

    pub(crate) fn emit_absolute_pointer(
        &mut self,
        target_reg: &TypedRegister,
        argument: &SingleLocationExpression,
        ctx: &Context,
        comment: &str,
    ) {
        let region = self.emit_lvalue_chain(argument, ctx);
        match region {
            DetailedLocation::Register { reg } => {
                self.builder.add_mov_reg(
                    target_reg,
                    &reg,
                    &argument.node,
                    &format!("copy reg that has pointer {target_reg:?} <- {reg:?} {comment}"),
                );
            }
            DetailedLocation::Memory {
                base_ptr_reg,
                offset,
                ..
            } => {
                let hwm = self.temp_registers.save_mark();

                let temp_offset_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u32_type()),
                    "emit_absolute_pointer: temp_offset_reg",
                );
                self.builder.add_mov_32_immediate_value(
                    temp_offset_reg.register(),
                    offset.0 as u32,
                    &argument.node,
                    "set offset in temp register",
                );
                self.builder.add_add_u32(
                    target_reg,
                    &base_ptr_reg,
                    temp_offset_reg.register(),
                    &argument.node,
                    "forcing lvalue to be a complete pointer",
                );

                self.temp_registers.restore_to_mark(hwm);
            }
        }
    }

    fn emit_variable_assignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let target_relative_frame_pointer = self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| {
                panic!(
                    "could not find id {} {}",
                    variable.unique_id_within_function, variable.assigned_name
                )
            })
            .clone();

        self.emit_expression_materialize(&target_relative_frame_pointer, expression, ctx);

        GeneratedExpressionResult::default()
    }

    fn emit_variable_binding(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) {
        let target_relative_frame_pointer = self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| panic!("{}", variable.assigned_name))
            .clone();

        self.emit_mut_or_immute(
            &target_relative_frame_pointer,
            mut_or_immutable_expression,
            ctx,
        )
    }
    fn load_register_contents_from_memory(
        &mut self,
        node: &Node,
        target_reg: &TypedRegister,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        comment: &str,
    ) {
        let underlying_type = target_reg.underlying();

        let kind = &underlying_type.kind;
        match kind {
            BasicTypeKind::Empty => {
                // No need to copy, it has zero size
            }
            BasicTypeKind::U8 | BasicTypeKind::B8 => {
                self.builder.add_ld8_from_pointer_with_offset_u16(
                    target_reg,
                    base_ptr_reg,
                    offset,
                    node,
                    comment,
                );
            }
            BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                self.builder.add_ld32_from_pointer_with_offset_u16(
                    target_reg,
                    base_ptr_reg,
                    offset,
                    node,
                    comment,
                );
            }
            _ => {
                let hwm = self.temp_registers.save_mark();

                let temp = self.temp_registers.allocate(
                    VmType::new_unknown_placement(unknown_type()),
                    "load register contents from memory",
                );
                self.builder.add_mov_32_immediate_value(
                    temp.register(),
                    offset.0 as u32,
                    node,
                    "offset to field",
                );
                self.builder.add_add_u32(
                    target_reg,
                    base_ptr_reg,
                    temp.register(),
                    node,
                    "store offset",
                );
                self.temp_registers.restore_to_mark(hwm);
            } /*
              _ => self.builder.add_block_copy_with_offset(
                  base_ptr_reg,
                  offset,
                  source_reg,
                  MemoryOffset(0),
                  underlying_type.total_size,
                  node,
                  comment,
              )*/
        }
    }

    fn store_register_contents_to_memory(
        &mut self,
        node: &Node,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        source_reg: &TypedRegister,
        comment: &str,
    ) {
        let underlying_type = source_reg.underlying();

        let kind = &underlying_type.kind;
        match kind {
            BasicTypeKind::Empty => {
                // No need to copy, it has zero size
            }
            BasicTypeKind::U8 | BasicTypeKind::B8 => {
                self.builder.add_st8_using_ptr_with_offset(
                    base_ptr_reg,
                    offset,
                    source_reg,
                    node,
                    comment,
                );
            }
            BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                self.builder.add_st32_using_ptr_with_offset(
                    base_ptr_reg,
                    offset,
                    source_reg,
                    node,
                    comment,
                );
            }

            _ => self.builder.add_block_copy_with_offset(
                base_ptr_reg,
                offset,
                source_reg,
                MemoryOffset(0),
                underlying_type.total_size,
                node,
                comment,
            ),
        }
    }

    fn emit_assignment(
        &mut self,
        node: &Node,
        lhs: &TargetAssignmentLocation,
        rhs: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let assignment_target = self.emit_lvalue_chain(&lhs.0, ctx);
        match assignment_target {
            DetailedLocation::Register { reg } => {
                self.emit_expression_materialize(&reg, rhs, &ctx);
            }
            DetailedLocation::Memory {
                base_ptr_reg,
                offset,
                ..
            } => {
                let rhs_reg = self.emit_rvalue(rhs, &ctx);
                self.store_register_contents_to_memory(
                    node,
                    &base_ptr_reg,
                    offset,
                    &rhs_reg,
                    "assignment",
                );
            }
        }

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
    fn ensure_absolute_target_pointer_location(
        &mut self,
        node: &Node,
        call_return_slot: &TypedRegister,
        return_ctx_to_check: &TypedRegister,
    ) {
        self.builder.add_lea(
            call_return_slot,
            return_ctx_to_check.addr(),
            node,
            "placing pointer to real return target",
        );
    }

    fn emit_arguments(
        &mut self,
        target_reg: Option<&TypedRegister>,
        node: &Node,
        signature: &Signature,
        self_variable: Option<&TypedRegister>,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> (Vec<SpilledRegister>, Vec<MutableReturnReg>) {
        let mut all_mutable_arguments_including_hidden = Vec::new();

        let mut copy_back_mutable_reg_pairs = Vec::new();

        if let Some(return_param_reg) = target_reg {
            let return_reg = TypedRegister::new_vm_type(0, return_param_reg.ty.clone());
            if return_param_reg
                .ty
                .needs_allocated_space_for_return_in_reg0()
            {
                self.builder.add_mov_reg(
                    &return_reg,
                    return_param_reg,
                    node,
                    &format!("copy in the return pointer into R0 <- {return_param_reg:?}"),
                );
            } else {
                all_mutable_arguments_including_hidden.push(return_param_reg);
            }

            if !signature.return_type.is_unit() && return_reg.ty.needs_copy_back_for_mutable() {
                copy_back_mutable_reg_pairs.push(MutableReturnReg {
                    target_location_after_call: DetailedLocation::Register {
                        reg: return_param_reg.clone(),
                    },
                    parameter_reg: return_reg,
                });
            }
        }
        let mut argument_registers = RegisterPool::new(1, 10);

        let mut protected_argument_registers = Vec::new();
        let mut spilled_arguments = Vec::new();

        for (index_in_signature, type_for_parameter) in signature.parameters.iter().enumerate() {
            let parameter_basic_type = layout_type(&type_for_parameter.resolved_type);
            let argument_register = argument_registers.alloc_register(
                VmType::new_unknown_placement(parameter_basic_type.clone()),
                &format!("emit argument {index_in_signature}"),
            );

            if ctx.register_is_protected(&argument_register) {
                let save_region = self.temp_frame_space_for_register("emit_arguments");
                self.builder.add_st_regs_to_frame(
                    save_region.addr,
                    &argument_register,
                    1,
                    node,
                    &format!("spill register to stack memory {argument_register:?}"),
                );
                spilled_arguments.push(SpilledRegister {
                    register: argument_register.clone(),
                    frame_memory_region: save_region,
                });
            }

            let mut argument_ctx = ctx.clone();
            argument_ctx.add_protected_registers(&protected_argument_registers);

            if index_in_signature == 0 && self_variable.is_some() {
                let self_reg = self_variable.as_ref().unwrap();

                if self_reg.index != argument_register.index {
                    self.builder.add_mov_reg(
                        &argument_register,
                        self_reg,
                        node,
                        &format!(
                            "move self_variable ({}) to first argument register",
                            self_reg.ty
                        ),
                    );
                }
            } else {
                let argument_vector_index = if self_variable.is_some() {
                    index_in_signature - 1
                } else {
                    index_in_signature
                };
                let argument_expr_or_location = &arguments[argument_vector_index];
                let debug_pos = self.builder.position();

                match argument_expr_or_location {
                    MutRefOrImmutableExpression::Location(lvalue) => {
                        let detailed_location = self.emit_lvalue_chain(lvalue, ctx);
                        if parameter_basic_type.should_be_copied_back_when_mutable_arg_or_return() {
                            copy_back_mutable_reg_pairs.push(MutableReturnReg {
                                target_location_after_call: detailed_location,
                                parameter_reg: argument_register.clone(),
                            })
                        }
                    }
                    MutRefOrImmutableExpression::Expression(expr) => {
                        let source_reg = self.emit_rvalue(expr, &argument_ctx);
                        self.builder.add_mov_reg(
                            &argument_register,
                            &source_reg,
                            &expr.node,
                            "copy reg result into arg",
                        );
                    }
                }

                if debug_pos == self.builder.position() {
                    // eprintln!("problem with {argument_expr_or_location:?}");
                }
                //assert_ne!(debug_pos, self.builder.position());
            }

            protected_argument_registers.push(argument_register.clone());
        }

        (spilled_arguments, copy_back_mutable_reg_pairs)
    }

    fn temp_frame_space_for_register(&mut self, comment: &str) -> FrameMemoryRegion {
        let start = self
            .frame_allocator
            .allocate(REG_ON_FRAME_SIZE, REG_ON_FRAME_ALIGNMENT);

        //info!(?start, comment, "allocating register space on frame");

        FrameMemoryRegion {
            addr: start,
            size: REG_ON_FRAME_SIZE,
        }
    }

    #[allow(clippy::too_many_lines)]
    fn emit_rvalue_postfix_chain(
        &mut self,
        target_reg: &TypedRegister,
        start_expression: &StartOfChain,
        chain: &[Postfix],
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let mut current_location = self.emit_start_of_chain(start_expression, ctx);
        let mut z_flag_result = GeneratedExpressionResult::default();

        //info!(t=?current_location.vm_type(), "start r value chain");

        for (index, element) in chain.iter().enumerate() {
            //info!(t=?element.ty, index,t=?current_location.vm_type(), ?element.kind, "chain element");
            let is_last = index == chain.len() - 1;
            match &element.kind {
                PostfixKind::StructField(anonymous_struct, field_index) => {
                    let struct_layout =
                        layout_type(&Type::AnonymousStruct(anonymous_struct.clone()));
                    let offset_item = struct_layout.get_field_offset(*field_index).unwrap();

                    current_location = current_location.add_offset(
                        offset_item.offset,
                        VmType::new_unknown_placement(offset_item.ty.clone()),
                    );
                    //info!(?current_location, "after field offset lookup");
                }
                PostfixKind::MemberCall(function_to_call, arguments) => {
                    let hwm = self.temp_registers.save_mark();
                    //let return_temp_reg =
                    //  self.temp_space_for_type(&function_to_call.signature().return_type, "");
                    let resolved_location = self
                        .emit_load_primitive_from_detailed_location_if_needed(
                            &current_location,
                            &element.node,
                            &format!("emit_rvalue_postfix member call "),
                        );
                    //let target_ctx = Context::new(temp_reg.register.clone());

                    match &**function_to_call {
                        Function::Internal(internal_fn) => {
                            if let Some((intrinsic_fn, intrinsic_arguments)) =
                                single_intrinsic_fn(&internal_fn.body)
                            {
                                //info!(?intrinsic_fn, "intrinsic");
                                let merged_arguments = Self::merge_arguments_keep_literals(
                                    arguments,
                                    intrinsic_arguments,
                                );

                                let z_result = self.emit_single_intrinsic_call_with_self(
                                    target_reg,
                                    &start_expression.node,
                                    intrinsic_fn,
                                    Some(element.ty.clone()),
                                    Some(resolved_location.register()),
                                    &merged_arguments,
                                    ctx,
                                    &format!("rvalue intrinsic call "),
                                );

                                if is_last {
                                    z_flag_result = z_result;
                                }
                            } else {
                                let (spilled_argument_registers, copy_back) = self.emit_arguments(
                                    Some(target_reg),
                                    &start_expression.node,
                                    &internal_fn.signature.signature,
                                    Some(resolved_location.register()),
                                    arguments,
                                    ctx,
                                );
                                self.add_call(
                                    &element.node,
                                    internal_fn,
                                    &format!("emit_rvalue call"),
                                );

                                self.emit_post_call(
                                    &spilled_argument_registers,
                                    &copy_back,
                                    &element.node,
                                    &format!("emit_rvalue postcall"),
                                );
                            }
                        }
                        Function::External(external_function_def) => {
                            self.emit_host_self_call(
                                None,
                                &start_expression.node,
                                external_function_def,
                                resolved_location.register(),
                                arguments,
                                ctx,
                            );
                        }
                        Function::Intrinsic(_intr) => {}
                        _ => panic!(
                            "{}",
                            &format!("not supported as a member call {function_to_call:?}")
                        ),
                    }

                    self.temp_registers.restore_to_mark(hwm);

                    current_location = DetailedLocation::Register {
                        reg: target_reg.clone(),
                    };
                    //info!(?current_location, "after member call");
                }
                PostfixKind::OptionalChainingOperator => {
                    todo!()
                }
                PostfixKind::NoneCoalescingOperator(expression) => {
                    let hwm = self.temp_registers.save_mark();
                    let temp_reg = self.temp_space_for_type(&expression.ty, "");

                    // materialize an u8
                    let resolved_location = self
                        .emit_load_primitive_from_detailed_location_if_needed(
                            &current_location,
                            &element.node,
                            "",
                        );

                    self.builder.add_tst_u8(
                        resolved_location.register(),
                        &element.node,
                        "test if optional tag is Some",
                    );

                    let patch = self
                        .builder
                        .add_jmp_if_equal_placeholder(&element.node, "jump if some");

                    /*
                    if let DetailedLocationResolved::TempRegister(temp_reg) = resolved_location {
                        self.temp_registers.free(temp_reg);
                    }

                     */

                    self.emit_expression_materialize(target_reg, expression, ctx);

                    self.temp_registers.restore_to_mark(hwm);

                    self.builder.patch_jump_here(patch);

                    current_location = DetailedLocation::Register {
                        reg: target_reg.clone(),
                    };
                    info!(?current_location, "after none coalesce");
                }
            }

            //info!(t=?element.ty, index, t=?current_location.vm_type(), ?element.kind, "after element");
        }

        self.emit_load_from_location(
            target_reg,
            &current_location,
            &start_expression.node,
            "rvalue postfix chain",
        );

        z_flag_result
    }

    /*
    fn call_post_helper(
        &mut self,
        node: &Node,
        signature: &Signature,
        maybe_self: Option<TypedRegister>,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let return_placed_type = self.return_frame_address(&signature.return_type);
        if return_placed_type.size().0 != 0 {
            self.builder.add_mov_for_assignment(
                ctx.target(),
                &return_placed_type,
                node,
                "copy the return value to the caller",
            );
        }

        self.copy_back_mutable_arguments(node, signature, maybe_self, arguments);

        GeneratedExpressionResult::default()
    }

     */

    fn emit_tuple(
        &mut self,
        target_reg: &TypedRegister,
        types: &[Type],
        expressions: &[Expression],
        ctx: &Context,
        node: &Node,
    ) {
        let gen_tuple_type = layout_tuple_items(types);
        let gen_tuple_placed = BasicType {
            total_size: gen_tuple_type.total_size,
            max_alignment: gen_tuple_type.max_alignment,
            kind: BasicTypeKind::Tuple(gen_tuple_type.clone()),
        };

        let frame_placed_tuple = self.frame_allocator.allocate_type(gen_tuple_placed);

        self.builder.add_frame_memory_clear(
            frame_placed_tuple.region(),
            node,
            "clear memory for tuple",
        );

        self.builder.add_lea(
            target_reg,
            frame_placed_tuple.addr(),
            node,
            &format!(
                "store the pointer to the frame allocated tuple in reg size:{} ty:{}",
                frame_placed_tuple.size(),
                frame_placed_tuple.ty()
            ),
        );

        // TODO: Bring this back. //assert_eq!(gen_tuple_placed.total_size, target_reg.size());
        // TODO: Bring this back. //assert_eq!(gen_tuple_type.fields.len(), expressions.len());

        for (offset_item, expr) in gen_tuple_type.fields.iter().zip(expressions) {
            let hwm = self.temp_registers.save_mark();

            let temp_materialize_reg = self.temp_registers.allocate(
                VmType::new_unknown_placement(offset_item.ty.clone()),
                "emit_materialize for tuple element",
            );
            //let materialized_item_in_tuple_reg = temp_materialize_reg.register();

            let rvalue_reg = self.emit_rvalue(expr, ctx);

            self.store_register_contents_to_memory(
                node,
                target_reg,
                offset_item.offset,
                &rvalue_reg,
                &format!("emit tuple item {}", offset_item.name),
            );
            self.temp_registers.restore_to_mark(hwm);
        }
    }

    fn emit_anonymous_struct(
        &mut self,
        target_reg: &TypedRegister,
        anon_struct_type: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Option<Node>, Expression)>,
        node: &Node,
        base_context: &Context,
    ) {
        self.emit_struct_literal_helper(
            target_reg,
            &anon_struct_type,
            source_order_expressions,
            node,
            base_context,
        );
    }

    fn emit_literal(
        &mut self,
        target_reg: &TypedRegister,
        node: &Node,
        literal: &Literal,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        match literal {
            Literal::IntLiteral(int) => {
                self.builder.add_mov_32_immediate_value(
                    target_reg,
                    *int as u32,
                    node,
                    "int literal",
                );
            }
            Literal::FloatLiteral(fixed_point) => {
                self.builder.add_mov_32_immediate_value(
                    target_reg,
                    fixed_point.inner() as u32,
                    node,
                    "float literal",
                );
            }
            Literal::NoneLiteral => {
                self.builder
                    .add_mov8_immediate(target_reg, 0, node, "none literal");
            }
            Literal::BoolLiteral(truthy) => {
                self.builder.add_mov8_immediate(
                    target_reg,
                    u8::from(*truthy),
                    node,
                    "bool literal",
                );
            }
            Literal::EnumVariantLiteral(enum_type, a, b) => {
                let variant_index = a.common().container_index as usize;
                let variants = enum_type
                    .variants
                    .values()
                    .map(|x| x.clone())
                    .collect::<Vec<_>>();
                let layout_enum =
                    layout_enum_into_tagged_union(&enum_type.assigned_name, &variants);
                let layout_variant = layout_enum.get_variant_by_index(variant_index);

                let hwm = self.temp_registers.save_mark();

                let temp_payload_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(layout_variant.ty.clone()),
                    "variant literal payload",
                );

                self.builder.add_mov8_immediate(
                    temp_payload_reg.register(),
                    variant_index as u8,
                    node,
                    &format!("enum variant {} tag", a.common().assigned_name),
                );
                self.builder.add_st8_using_ptr_with_offset(
                    target_reg,
                    layout_enum.tag_offset,
                    temp_payload_reg.register(),
                    &node,
                    "put enum tag in place",
                );

                match b {
                    EnumLiteralData::Nothing => {}
                    EnumLiteralData::Tuple(expressions) => {
                        let EnumVariantType::Tuple(tuple_type) = a else {
                            panic!();
                        };
                        self.emit_tuple(
                            target_reg,
                            &tuple_type.fields_in_order,
                            expressions,
                            ctx,
                            node,
                        );
                    }
                    EnumLiteralData::Struct(sorted_expressions) => {
                        let EnumVariantType::Struct(variant_struct_type) = a else {
                            panic!()
                        };

                        self.emit_anonymous_struct(
                            target_reg,
                            &variant_struct_type.anon_struct,
                            sorted_expressions,
                            node,
                            ctx,
                        );
                    }
                }

                self.store_register_contents_to_memory(
                    node,
                    target_reg,
                    layout_enum.payload_offset,
                    temp_payload_reg.register(),
                    "copy enum payload into target",
                );

                self.temp_registers.restore_to_mark(hwm);
            }
            Literal::TupleLiteral(tuple_type, expressions) => {
                self.emit_tuple(target_reg, tuple_type, expressions, ctx, node)
            }
            Literal::StringLiteral(str) => {
                self.emit_string_literal(target_reg, node, str, ctx);
            }
            Literal::Slice(slice_type, expressions) => {
                self.emit_slice_literal(target_reg, node, slice_type, expressions, ctx);
            }
            Literal::SlicePair(slice_pair_type, pairs) => {
                self.emit_slice_pair_literal(slice_pair_type, pairs, node, ctx);
            }
        }

        GeneratedExpressionResult::default()
    }

    fn emit_string_literal(
        &mut self,
        target_reg: &TypedRegister,
        node: &Node,
        string: &str,
        ctx: &Context,
    ) {
        let string_bytes = string.as_bytes();
        let string_byte_count = string_bytes.len();

        let data_ptr = self
            .state
            .constants_manager
            .allocate_byte_array(string_bytes);

        let string_header = StringHeader {
            heap_offset: data_ptr.addr().0,
            byte_count: string_byte_count as u32,
        };

        // Convert string header to bytes (little-endian)
        let mut header_bytes = [0u8; 12];
        header_bytes[0..4].copy_from_slice(&string_header.heap_offset.to_le_bytes());
        header_bytes[4..8].copy_from_slice(&string_header.byte_count.to_le_bytes());

        let string_header_in_heap_ptr = HeapMemoryAddress(
            self.state
                .constants_manager
                .allocate_byte_array(&header_bytes)
                .addr()
                .0,
        );
        // TODO: Bring this back // assert_eq!(ctx.target_size(), STRING_PTR_SIZE);
        self.builder.add_mov_32_immediate_value(
            target_reg,
            string_header_in_heap_ptr.0,
            node,
            "constant string",
        );
    }

    fn emit_option_expression(
        &mut self,
        target_reg: &TypedRegister,
        node: &Node,
        maybe_option: Option<&Expression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        if let Some(some_expression) = maybe_option {
            let target_tag = target_reg.underlying().optional_info().unwrap().clone();
            let hwm = self.temp_registers.save_mark();
            {
                let tag_reg = self
                    .temp_registers
                    .allocate(VmType::new_unknown_placement(u8_type()), "emit_option tag");
                self.builder.add_mov8_immediate(
                    tag_reg.register(),
                    1,
                    node,
                    "load the tag Some (1)",
                );
                self.builder.add_st8_using_ptr_with_offset(
                    target_reg,
                    target_tag.tag_offset,
                    tag_reg.register(),
                    node,
                    "store optional Some tag",
                );
            }
            {
                let payload_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(unknown_type()),
                    "emit_option_expression",
                );
                self.emit_expression_materialize(payload_reg.register(), some_expression, ctx); // Fills in more of the union
            }
            self.temp_registers.restore_to_mark(hwm);
        } else {
            self.builder
                .add_mov8_immediate(target_reg, 0, node, "option None tag"); // 0 signals `None`
            // No real need to clear the rest of the memory
        }

        GeneratedExpressionResult::default()
    }

    pub fn emit_expression_materialize(
        &mut self,
        target_reg: &TypedRegister,
        expr: &Expression,
        ctx: &Context,
    ) {
        let result = self.emit_expression(target_reg, expr, ctx);

        self.materialize_z_flag_to_bool_if_needed(target_reg, result, &expr.node);
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
        let hwm = self.temp_registers.save_mark();

        let collection_basic_type = layout_type(collection_type);
        let discard_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(collection_basic_type),
            "emit_for_loop_discard",
        );

        let collection_reg =
            self.emit_expression_location_mut_ref_or_immutable(&iterable.resolved_expression, ctx);
        match collection_type {
            Type::String => {
                todo!();
            }
            Type::NamedStruct(named_type) => {
                if named_type.is_vec() {
                    self.emit_for_loop_lambda(
                        discard_reg.register(),
                        node,
                        Collection::Vec,
                        &collection_reg,
                        collection_type,
                        for_pattern,
                        &lambda_non_capturing_expr,
                        ctx,
                    )
                } else if named_type.is_map() {
                    self.emit_for_loop_lambda(
                        discard_reg.register(),
                        node,
                        Collection::Map,
                        &collection_reg,
                        collection_type,
                        for_pattern,
                        &lambda_non_capturing_expr,
                        ctx,
                    )
                } else if named_type.is_range() {
                    self.emit_for_loop_lambda(
                        discard_reg.register(),
                        node,
                        Collection::Range,
                        &collection_reg,
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

        self.temp_registers.restore_to_mark(hwm);

        GeneratedExpressionResult::default()
    }

    fn emit_block(
        &mut self,
        target_reg: &TypedRegister,
        expressions: &[Expression],
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        if let Some((last, others)) = expressions.split_last() {
            let hwm = self.temp_registers.save_mark();

            let unit_temp_reg = self.temp_registers.allocate(
                VmType::new_unknown_placement(unit_type()),
                "emit_block empty",
            );
            for expr in others {
                ///info!("this is others in block");
                self.emit_expression_materialize(unit_temp_reg.register(), expr, ctx);
            }
            self.temp_registers.restore_to_mark(hwm);
            //            info!(?last.ty, ?target_reg.ty, "this is the last in the block!");
            self.emit_expression_materialize(target_reg, last, ctx);
        } else {
            // empty blocks are allowed for side effects
        }

        GeneratedExpressionResult::default()
    }

    fn get_variable_register(&self, variable: &VariableRef) -> &TypedRegister {
        let frame_address = self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap();

        frame_address
    }

    fn get_variable_frame_placed(&self, variable: &VariableRef) -> FramePlacedType {
        let frame_address = self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap();

        frame_address.frame_placed()
    }

    fn emit_variable_access_as_rvalue(
        &mut self,
        target_reg: &TypedRegister,
        node: &Node, // Variable access node
        variable: &VariableRef,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let frame_placed_variable = self.get_variable_register(variable).clone();
        self.builder.add_mov_reg(
            target_reg,
            &frame_placed_variable,
            node,
            &format!("variable access {}", tinter::red(&variable.assigned_name)),
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

    pub(crate) fn emit_expression_location_mut_ref_or_immutable(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) -> TypedRegister {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_rvalue(found_expression, ctx)
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                let x = self.emit_lvalue_chain(location_expression, ctx);
                // TODO: FIX THIS
                if let DetailedLocation::Register { reg } = x {
                    reg
                } else if let DetailedLocation::Memory { base_ptr_reg, .. } = x {
                    error!("expected register");
                    base_ptr_reg
                } else {
                    panic!("not sure");
                }
            }
        }
    }

    fn compound_assignment(
        &mut self,
        target_location: &TargetAssignmentLocation,
        op: &CompoundOperatorKind,
        source: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let assignment_target = self.emit_lvalue_chain(&target_location.0, ctx);

        let hwm = self.temp_registers.save_mark();

        let resolved = self.emit_load_primitive_from_detailed_location_if_needed(
            &assignment_target,
            &target_location.0.node,
            "compound_assignment",
        );

        let source_info = self.emit_rvalue(source, ctx);

        let type_to_consider = Self::referenced_or_not_type(&source.ty);

        match &type_to_consider {
            Type::Int => {
                self.emit_compound_assignment_i32(
                    &source.node,
                    resolved.register(),
                    op,
                    &source_info,
                );
            }
            Type::Float => {
                self.emit_compound_assignment_f32(
                    &source.node,
                    resolved.register(),
                    op,
                    &source_info,
                );
            }
            Type::String => todo!(),
            _ => panic!("not allowed as a compound assignment"),
        }

        self.emit_store_primitive_from_detailed_location_if_needed(
            &resolved,
            &assignment_target,
            &target_location.0.node,
        );

        /*
        if let DetailedLocationResolved::TempRegister(temp_reg) = resolved {
            self.temp_registers.free(temp_reg);
        }

         */
        self.temp_registers.restore_to_mark(hwm);

        GeneratedExpressionResult::default()
    }

    fn emit_compound_assignment_i32(
        &mut self,
        node: &Node,
        target: &TypedRegister,
        op: &CompoundOperatorKind,
        source_ctx: &TypedRegister,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.builder
                    .add_add_u32(target, target, source_ctx, node, "+=  (i32)");
            }
            CompoundOperatorKind::Sub => {
                self.builder
                    .add_sub_u32(target, target, source_ctx, node, "-=  (i32)")
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
        target: &TypedRegister,
        op: &CompoundOperatorKind,
        source_ctx: &TypedRegister,
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

    fn emit_anonymous_struct_literal(
        &mut self,
        target_reg: &TypedRegister,
        anon_struct_literal: &AnonymousStructLiteral,
        ty: &Type,
        node: &Node,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let anon_struct_type = match ty {
            Type::NamedStruct(named_struct) => named_struct.anon_struct_type.clone(),
            Type::AnonymousStruct(anon_struct_type) => anon_struct_type.clone(),
            _ => panic!("internal error with struct literal"),
        };

        self.emit_struct_literal_helper(
            target_reg,
            &anon_struct_type,
            &anon_struct_literal.source_order_expressions,
            node,
            ctx,
        )
    }

    fn allocate_frame_space_and_assign_register(&mut self, ty: &BasicType) -> TypedRegister {
        let frame_placed_type = self.frame_allocator.allocate_type(ty.clone());

        self.frame_memory_registers.alloc_register(
            VmType::new_frame_placed(frame_placed_type),
            "allocate frame space",
        )
    }

    fn emit_struct_literal_helper(
        &mut self,
        target_reg: &TypedRegister,
        struct_type_ref: &AnonymousStructType,
        source_order_expressions: &Vec<(usize, Option<Node>, Expression)>,
        node: &Node,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let gen_source_struct_type = layout_struct_type(struct_type_ref, "");

        if target_reg.size() != gen_source_struct_type.total_size {
            info!("problem");
        }

        // TODO: Bring this back // assert_eq!(target_reg.size().0, gen_source_struct_type.total_size.0);
        assert_eq!(
            source_order_expressions.len(),
            gen_source_struct_type.fields.len()
        );

        let basic_type_for_struct = BasicType {
            kind: BasicTypeKind::Struct(gen_source_struct_type.clone()),
            total_size: gen_source_struct_type.total_size,
            max_alignment: gen_source_struct_type.max_alignment,
        };

        let struct_type = BasicType {
            total_size: gen_source_struct_type.total_size,
            max_alignment: gen_source_struct_type.max_alignment,
            kind: BasicTypeKind::Struct(gen_source_struct_type.clone()),
        };

        for (offset_item, (field_index, _node, expression)) in gen_source_struct_type
            .fields
            .iter()
            .zip(source_order_expressions)
        {
            let real_offset_item = struct_type.get_field_offset(*field_index).unwrap();
            /*
            let temp_register_to_materialize_to = self.temp_registers.allocate(
                VmType::new_unknown_placement(real_offset_item.ty.clone()),
                &format!("struct literal {field_index}"),
            );

             */
            let field_rvalue = self.emit_rvalue(expression, ctx);

            self.store_register_contents_to_memory(
                node,
                target_reg,
                real_offset_item.offset,
                &field_rvalue,
                &format!(
                    "fill in struct field {} ({})",
                    offset_item.name, field_index
                ),
            );

            //self.temp_registers.free(temp_register_to_materialize_to);
        }

        GeneratedExpressionResult::default()
    }

    /*
              if elements_fit_inside_register {
               self.builder.add_st_indirect(
                   base_ptr_reg.register(),
                   heap_offset,
                   temp_element_ctx.register(),
                   node,
                   "copy into slice",
               );
           } else {
               // it is pointer base, so we need to copy it into the slice
               self.builder.add_ld_addr_offset(
                   target_addr_temp_calculation_reg.register(),
                   base_ptr_reg.register(),
                   heap_offset,
                   node,
                   "get the slice offset",
               );
               self.builder.add_mov_mem(
                   target_addr_temp_calculation_reg.register(),
                   temp_element_ctx.register(),
                   element_gen_type.total_size,
                   node,
                   "copy 'struct' into slice",
               );
           }
    */

    fn emit_slice_literal(
        &mut self,
        target_reg: &TypedRegister,
        node: &Node,
        slice_type: &Type,
        expressions: &[Expression],
        ctx: &Context,
    ) {
        assert_eq!(target_reg.size(), SLICE_HEADER_SIZE);
        let Type::Slice(element_type) = slice_type else {
            panic!("incorrect slice type")
        };

        let element_gen_type = layout_type(element_type);
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(element_gen_type.total_size.0 * element_count);

        let hwm = self.temp_registers.save_mark();

        let base_ptr_reg = {
            self.temp_registers.allocate(
                VmType::new_unknown_placement(unknown_type()),
                "emit_slice_base",
            )
        };
        let element_size_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(unknown_type()),
            "element_size_reg",
        );

        self.builder.add_mov_32_immediate_value(
            element_size_reg.register(),
            element_gen_type.total_size.0 as u32,
            node,
            "element total size",
        );

        self.builder.add_alloc(
            base_ptr_reg.register(),
            total_slice_size,
            node,
            "allocate slice",
        );

        let element_temp_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(unknown_type()),
            "emit_slice_literal",
        );

        for (index, expr) in expressions.iter().enumerate() {
            self.emit_expression_materialize(element_temp_reg.register(), expr, ctx);

            self.store_register_contents_to_memory(
                node,
                base_ptr_reg.register(),
                MemoryOffset(0),
                element_temp_reg.register(),
                "copy into slice",
            );

            self.builder.add_add_u32(
                base_ptr_reg.register(),
                base_ptr_reg.register(),
                element_size_reg.register(),
                node,
                "move destination ptr forward by element size",
            );
        }

        let element_count_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(u32_type()),
            "emit slice literal",
        );

        self.builder.add_mov_32_immediate_value(
            element_count_reg.register(),
            element_count as u32,
            node,
            "set element count",
        );

        self.builder.add_slice_from_heap(
            target_reg,
            base_ptr_reg.register(),
            element_size_reg.register(),
            element_count_reg.register(),
            node,
            "slice literal",
        );

        self.temp_registers.restore_to_mark(hwm);
    }

    fn emit_slice_pair_literal(
        &mut self,
        slice_type: &Type,
        expressions: &[(Expression, Expression)],
        node: &Node,
        ctx: &Context,
    ) {
        /*
        let Type::SlicePair(key_type, value_type) = slice_type else {
            panic!("should have been slice pair type")
        };

        assert!(key_type.is_concrete());
        assert!(value_type.is_concrete());

        assert_eq!(ctx.target_size(), SLICE_PAIR_HEADER_SIZE);

        //let constructed_tuple = Type::Tuple(vec![*key_type.clone(), *value_type.clone()]);

        let key_layout = layout_type(key_type);
        let value_layout = layout_type(value_type);

        //info!(?key_layout, ?value_layout, "layouts");

        let pair_size = key_layout.total_size.0 + value_layout.total_size.0; // Alignment is not relevant, since we will only access them using byte chunks.
        let element_count = expressions.len() as u16;
        let total_slice_size = MemorySize(pair_size * element_count);

        let heap_ptr_header_addr = ctx
            .register()
            .move_with_offset(SLICE_PTR_OFFSET, pointer_type());

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
            ctx.register(),
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
        target_reg: &TypedRegister,
        node: &Node,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) {
        if let MutRefOrImmutableExpression::Expression(found_expr) = &arguments[0] {
            let memory = self.emit_rvalue(found_expr, ctx);
            self.builder
                .add_vec_from_slice(target_reg, &memory, node, "create vec");
        } else {
            panic!("vec_from_slice");
        }
    }

    fn emit_match(
        &mut self,
        target_reg: &TypedRegister,
        match_expr: &Match,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let region_to_match = self.emit_for_access_or_location(&match_expr.expression, ctx);

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
                Some(self.emit_condition_context(guard, ctx))

            //                Some(self.builder.add_jmp_if_not_equal_polarity_placeholder(
            //                  &polarity.polarity(),
            //                match_expr.expression.node(),
            //              "placeholder for skip guard",
            //        ))
            } else {
                None
            };

            self.emit_expression_materialize(target_reg, &arm.expression, ctx);

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

    fn emit_guard(
        &mut self,
        target_reg: &TypedRegister,
        guards: &Vec<Guard>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let mut jump_to_exit_placeholders = Vec::new();
        for guard in guards {
            if let Some(condition) = &guard.condition {
                //                let result = self.emit_boolean_expression_z_flag(condition)?;
                let skip_expression_patch = self.emit_condition_context(condition, ctx);
                //&result.polarity(),
                //&guard.result.node,
                //"guard condition",
                //);
                self.emit_expression_materialize(target_reg, &guard.result, ctx);
                let jump_to_exit_placeholder = self.builder.add_jump_placeholder(
                    &guard.result.debug_last_expression().node,
                    "jump to exit",
                );
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
                self.builder.patch_jump_here(skip_expression_patch);
            } else {
                // _ -> wildcard
                self.emit_expression_materialize(target_reg, &guard.result, ctx);
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.builder.patch_jump_here(placeholder);
        }

        GeneratedExpressionResult::default()
    }

    fn emit_when(
        &mut self,
        target_reg: &TypedRegister,
        bindings: &Vec<WhenBinding>,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let mut all_false_jumps = Vec::new();

        for binding in bindings {
            //            let placed_binding_variable = self.get_variable_region(&binding.variable);
            let old_variable_region = self.emit_for_access_or_location(&binding.expr, ctx);

            self.builder
                .add_tst_u8(&old_variable_region, binding.expr.node(), "check binding");
            let patch = self
                .builder
                .add_jmp_if_not_equal_placeholder(binding.expr.node(), "jump if none");
            all_false_jumps.push(patch);
        }

        // if we are here all bindings are `Some`, so it is safe to get the payload
        for binding in bindings {
            let placed_variable = self.get_variable_register(&binding.variable).clone();

            if binding.has_expression() {
                self.emit_mut_or_immute(&placed_variable, &binding.expr, &ctx);
            } else {
                let MutRefOrImmutableExpression::Expression(variable_access_expression) =
                    &binding.expr
                else {
                    panic!("must be expression");
                };
                let old_variable_region = self.emit_rvalue(variable_access_expression, ctx);

                let tagged_union_binding = old_variable_region.ty.underlying();
                let tagged_union = tagged_union_binding.optional_info().unwrap();

                self.builder.add_block_copy_with_offset(
                    &placed_variable,
                    MemoryOffset(0),
                    &old_variable_region,
                    tagged_union.payload_offset,
                    tagged_union.tag_size,
                    binding.expr.node(),
                    "copy in the payload. Unwrap.",
                );
            }
        }

        self.emit_expression_materialize(target_reg, true_expr, ctx);
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
            self.emit_expression_materialize(target_reg, else_expr, ctx);
            self.builder.patch_jump_here(maybe_jump_over_false.unwrap());
        }

        GeneratedExpressionResult::default()
    }

    fn emit_tuple_destructuring(
        &mut self,
        target_variables: &[VariableRef],
        tuple_type: &[Type],
        source_tuple_expression: &Expression,
        context: &Context,
    ) -> GeneratedExpressionResult {
        let node = &source_tuple_expression.node;
        let tuple_base_pointer_reg = self.emit_rvalue(source_tuple_expression, context);

        let tuple_type = layout_tuple_items(tuple_type);
        // TODO: Bring this back//assert_eq!(tuple_type.total_size.0, tuple_base_pointer_reg.size().0);

        for (tuple_index, target_variable) in target_variables.iter().enumerate() {
            if target_variable.is_unused {
            } else {
                let frame_placed_target_variable =
                    self.get_variable_register(target_variable).clone();

                //                assert_eq!(frame_placed_target_variable.size().0, offset_item.size.0);

                let field_offset_item = &tuple_type.fields[tuple_index];

                self.load_register_contents_from_memory(
                    &target_variable.name,
                    &frame_placed_target_variable,
                    &tuple_base_pointer_reg,
                    field_offset_item.offset,
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
        target_reg: &TypedRegister,
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
        // TODO: Bring this back// assert_eq!(target_reg.size(), constant_region.size());

        if constant_region.ty().is_represented_as_a_pointer_in_reg() {
            // Just copy the pointer to the target register
            self.builder.add_mov_32_immediate_value(
                target_reg,
                constant_region.addr().0,
                node,
                &format!(
                    "load constant pointer '{}' {:?}",
                    constant_reference.assigned_name,
                    constant_region.ty()
                ),
            );
        } else {
            self.emit_load_primitive_from_absolute_memory_address(
                target_reg,
                constant_region.addr(),
                &VmType::new_unknown_placement(constant_region.ty().clone()),
                node,
                &format!(
                    "load constant primitive '{}' {:?}",
                    constant_reference.assigned_name,
                    constant_region.ty()
                ),
            );
        }

        GeneratedExpressionResult::default()
    }

    fn emit_coerce_option_to_bool(
        &mut self,
        target_reg: &TypedRegister,
        expr: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        //info!(?target_reg.ty, "it wants to coerce this to bool");

        let base_pointer_of_tagged_union_reg = self.emit_rvalue(expr, ctx);

        /* TODO: Bring this back // let (tag_offset, tag_size, ..) = base_pointer_of_tagged_union_reg
            .underlying()
            .unwrap_info()
            .unwrap();
        assert_eq!(tag_size.0, 1);
        */

        // Move the tag portion to the target variable
        self.builder.add_ld8_from_pointer_with_offset_u16(
            target_reg,
            &base_pointer_of_tagged_union_reg,
            MemoryOffset(0),
            &expr.node,
            "load option tag to bool register",
        );

        GeneratedExpressionResult::default()
    }

    fn emit_start_of_chain(&mut self, start: &StartOfChain, ctx: &Context) -> DetailedLocation {
        match &start.kind {
            StartOfChainKind::Expression(expr) => DetailedLocation::Register {
                reg: self.emit_rvalue(expr, ctx),
            },
            StartOfChainKind::Variable(variable) => {
                let variable_reg = self.get_variable_register(variable);
                DetailedLocation::Register {
                    reg: variable_reg.clone(),
                }
            }
        }
    }

    fn emit_internal_call(
        &mut self,
        target_reg: Option<&TypedRegister>,
        node: &Node,
        internal_fn: &InternalFunctionDefinitionRef,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let (spilled_arguments, copy_back) = self.emit_arguments(
            target_reg,
            node,
            &internal_fn.signature.signature,
            None,
            arguments,
            ctx,
        );

        self.add_call(node, internal_fn, &format!("call")); // will be fixed up later

        self.emit_post_call(
            &spilled_arguments,
            &copy_back,
            node,
            "restore spilled after call",
        );

        GeneratedExpressionResult::default()
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
        /*
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

        */
        let patch_position = self.builder.add_call_placeholder(node, call_comment);
        self.state.function_fixups.push(FunctionFixup {
            patch_position,
            fn_id: internal_fn.program_unique_id,
            internal_function_definition: internal_fn.clone(),
        });
        //}
    }

    fn emit_host_call(
        &mut self,
        node: &Node,
        host_fn: &ExternalFunctionDefinitionRef,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let (spilled_arguments, copy_back) =
            self.emit_arguments(None, node, &host_fn.signature, None, arguments, ctx);

        let arg_count = arguments.len() as u8;
        self.builder.add_host_call(
            host_fn.id as u16,
            arg_count,
            node,
            &format!(
                "host: {} arguments_size:{}",
                host_fn.assigned_name, arg_count
            ),
        );

        self.emit_post_call(&spilled_arguments, &copy_back, node, "host call");

        GeneratedExpressionResult::default()
    }

    fn emit_host_self_call(
        &mut self,
        maybe_return: Option<&TypedRegister>,
        node: &Node,
        host_fn: &ExternalFunctionDefinitionRef,
        self_frame_placed_type: &TypedRegister,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let (spilled_arguments, copy_backs) = self.emit_arguments(
            maybe_return,
            node,
            &host_fn.signature,
            Some(self_frame_placed_type),
            arguments,
            ctx,
        );

        let arg_count = (1 + arguments.len()) as u8;
        self.builder.add_host_call(
            host_fn.id as u16,
            arg_count,
            node,
            &format!(
                "host self call: {} arguments_size:{}",
                host_fn.assigned_name, arg_count,
            ),
        ); // will be fixed up later

        self.emit_post_call(&spilled_arguments, &copy_backs, node, "host_self_call");

        GeneratedExpressionResult::default()
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
        target_reg: &TypedRegister,
        node: &Node,
        source_collection_type: Collection,
        transformer: Transformer,
        source_collection_self_region: &TypedRegister,
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
                self.variable_registers
                    .get(&x.unique_id_within_function)
                    .unwrap()
                    .clone()
            })
            .collect();

        // Primary is the right most variable
        let primary_variable = &target_variables[target_variables.len() - 1];

        let lambda_return_analyzed_type = &lambda_expr.ty;

        // 1. Optionally initialize the result vector if the transformer produces one.
        let lambda_return_gen_type = layout_type(lambda_return_analyzed_type);

        if matches!(
            transformer.return_type(),
            TransformerResult::VecWithLambdaResult | TransformerResult::VecFromSourceCollection
        ) {
            let element_size_in_target_vec = match transformer.return_type() {
                TransformerResult::VecFromSourceCollection => {
                    let element_gen_type = layout_type(primary_element_type.unwrap());
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

            assert_eq!(target_reg.size(), VEC_PTR_SIZE);
            self.builder.add_vec_create(
                target_reg,
                element_size_in_target_vec,
                node,
                "target result vector",
            );
        }

        let hwm = self.temp_registers.save_mark();

        // 2. Initialize the iterator and generate code to fetch the next element.
        let (continue_iteration_label, iteration_complete_patch_position, temp_reg) = self
            .iter_init_and_next(
                node,
                source_collection_type,
                source_collection_self_region,
                &target_variables,
            );

        // 3. Inline the lambda code for the current element(s).
        let lambda_result = self.emit_rvalue(lambda_expr, ctx);

        // 4. If the transformer supports early exit, set the Z flag based on the lambda result.
        let transformer_z_flag_state =
            self.check_if_transformer_sets_z_flag(transformer, &lambda_result, node);

        // 5. Conditionally skip result insertion if early exit is triggered.
        let maybe_skip_early = if matches!(
            transformer_z_flag_state,
            GeneratedExpressionResultKind::TFlagIsTrueWhenSet
                | GeneratedExpressionResultKind::TFlagIsTrueWhenClear
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
                    target_reg,
                    node,
                );
            }
            TransformerResult::VecFromSourceCollection => {
                self.add_to_collection(node, source_collection_type, target_reg, primary_variable);
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
                    .add_stz(target_reg, node, "transformer sets standard bool");
            }
            TransformerResult::WrappedValueFromSourceCollection => {
                let some_payload = layout_optional_type(&Type::Optional(Box::from(
                    lambda_return_analyzed_type.clone(),
                )));
                let BasicTypeKind::Optional(tagged_union) = &some_payload.kind else {
                    panic!("expected optional {:?}", target_reg.ty);
                };

                //let tag_target = ctx.target_register().move_to_optional_tag();
                let tag_target = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u8_type()),
                    "iterate over collection target",
                );

                self.builder.add_mov8_immediate(
                    &tag_target.register(),
                    1,
                    node,
                    "mark tag as Some",
                );

                self.builder.add_st8_using_ptr_with_offset(
                    primary_variable,
                    tagged_union.tag_offset,
                    tag_target.register(),
                    node,
                    "copy Tag value of (1)",
                );

                //self.copy_contents_to_memory(node, primary_variable, tagged_union.payload_offset, )

                // TODO: Unsure how to copy to memory in a good way
            }
            _ => {}
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    #[allow(clippy::unnecessary_wraps)]
    fn iter_init_and_next(
        &mut self,
        node: &Node,
        collection_type: Collection,
        collection_self_addr: &TypedRegister,
        target_variables: &[TypedRegister],
    ) -> (InstructionPosition, PatchPosition, TempRegister) {
        let iterator_gen_type = collection_type.iterator_gen_type();

        let iterator_target_placed = self.frame_allocator.allocate_type(iterator_gen_type);
        let iterator_target = self.temp_registers.allocate(
            VmType::new_frame_placed(iterator_target_placed),
            "iter_init_and_next",
        );
        let iter_next_position = InstructionPosition(self.builder.position().0 + 1);
        let placeholder = match collection_type {
            Collection::Vec => {
                self.builder.add_vec_iter_init(
                    iterator_target.register(),
                    collection_self_addr,
                    node,
                    "vec init",
                );

                if target_variables.len() == 2 {
                    self.builder.add_vec_iter_next_pair_placeholder(
                        iterator_target.register(),
                        &target_variables[0],
                        &target_variables[1],
                        node,
                        "vec iter next pair",
                    )
                } else {
                    self.builder.add_vec_iter_next_placeholder(
                        iterator_target.register(),
                        &target_variables[0],
                        node,
                        "vec iter next single",
                    )
                }
            }
            Collection::Map => {
                self.builder.add_map_iter_init(
                    iterator_target.register(),
                    collection_self_addr,
                    node,
                    "map init",
                );

                if target_variables.len() == 2 {
                    self.builder.add_map_iter_next_pair_placeholder(
                        iterator_target.register(),
                        &target_variables[0],
                        &target_variables[1],
                        node,
                        "map next_pair",
                    )
                } else {
                    self.builder.add_map_iter_next_placeholder(
                        iterator_target.register(),
                        &target_variables[0],
                        node,
                        "map next_single",
                    )
                }
            }
            Collection::Grid => todo!(),
            Collection::Range => {
                self.builder.add_range_iter_init(
                    iterator_target.register(),
                    collection_self_addr,
                    node,
                    "range init",
                );

                assert_eq!(target_variables.len(), 1);
                self.builder.add_range_iter_next_placeholder(
                    iterator_target.register(),
                    &target_variables[0],
                    node,
                    "range iter next single",
                )
            }

            // Low  prio
            Collection::String => todo!(),
        };

        (iter_next_position, placeholder, iterator_target)
    }

    fn check_if_transformer_sets_z_flag(
        &mut self,
        transformer: Transformer,
        in_value: &TypedRegister,
        node: &Node,
    ) -> GeneratedExpressionResultKind {
        match transformer {
            Transformer::For => GeneratedExpressionResultKind::TFlagIsIndeterminate,
            Transformer::Filter => {
                // TODO: Bring this back //assert_eq!(in_value.size().0, 1); // bool
                self.builder
                    .add_tst_u8(in_value, node, "filter bool to z flag");
                GeneratedExpressionResultKind::TFlagIsTrueWhenSet
            }
            Transformer::Find => {
                // TODO: Bring this back //assert_eq!(in_value.size().0, 1); // bool
                self.builder
                    .add_tst_u8(in_value, node, "find: bool to z flag");
                GeneratedExpressionResultKind::TFlagIsTrueWhenClear
            }
            Transformer::Map => GeneratedExpressionResultKind::TFlagIsIndeterminate,
            Transformer::Any => {
                self.builder.add_tst_u8(in_value, node, "any, check tag");
                GeneratedExpressionResultKind::TFlagIsTrueWhenClear
            }
            Transformer::All => {
                self.builder.add_tst_u8(in_value, node, "all, check tag");
                GeneratedExpressionResultKind::TFlagIsTrueWhenSet
            }
            Transformer::FilterMap => {
                self.builder
                    .add_tst_u8(in_value, node, "filter map, check tag");
                GeneratedExpressionResultKind::TFlagIsTrueWhenSet
            }
        }
    }

    fn add_to_collection(
        &mut self,
        node: &Node,
        collection: Collection,
        mut_collection: &TypedRegister,
        value: &TypedRegister,
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
        in_value: &TypedRegister,
        should_unwrap_value: bool,
        collection_type: Collection,
        mut_collection: &TypedRegister,
        node: &Node,
    ) {
        let hwm = self.temp_registers.save_mark();

        let (register_to_be_inserted_in_collection, maybe_temp) = if should_unwrap_value {
            let tagged_union = in_value.underlying().optional_info().unwrap().clone();
            let some_variant = tagged_union.get_variant_by_index(1);
            let payload_vm_type = VmType::new_unknown_placement(some_variant.ty.clone());
            let temp_reg = self
                .temp_registers
                .allocate(payload_vm_type.clone(), "transform add to collection");
            let (_tag_offset, _tag_size, payload_offset, _) =
                in_value.underlying().unwrap_info().unwrap();
            self.emit_load_from_memory(
                temp_reg.register(),
                in_value,
                payload_offset,
                &payload_vm_type,
                node,
                "transformer add to collection",
            );
            (temp_reg.register.clone(), Some(temp_reg))
        } else {
            (in_value.clone(), None)
        };

        self.add_to_collection(
            node,
            collection_type,
            mut_collection,
            &register_to_be_inserted_in_collection,
        );

        self.temp_registers.restore_to_mark(hwm);
    }
    fn materialize_z_flag_to_bool_if_needed(
        &mut self,
        target: &TypedRegister,
        z_flag_state: GeneratedExpressionResult,
        node: &Node,
    ) {
        match z_flag_state.kind {
            GeneratedExpressionResultKind::TFlagIsIndeterminate => {
                // intentionally do nothing
            }
            GeneratedExpressionResultKind::TFlagIsTrueWhenSet => {
                self.builder
                    .add_stz(target, node, "materialize positive z flag");
            }
            GeneratedExpressionResultKind::TFlagIsTrueWhenClear => {
                self.builder
                    .add_stnz(target, node, "materialize inverse z flag");
            }
        }
    }

    fn emit_for_loop_lambda(
        &mut self,
        target_reg: &TypedRegister,
        node: &Node,
        collection: Collection,
        source_collection: &TypedRegister,
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
            target_reg,
            node,
            collection,
            Transformer::For,
            source_collection,
            source_collection_type,
            &fake_lambda_expr,
            ctx,
        )
    }

    fn emit_borrow_mutable_reference(
        &mut self,
        target_register: &TypedRegister,
        node: &Node,
        expr: &Expression,
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        let inner = self.emit_rvalue(expr, ctx);

        if !inner.ty().is_mutable_reference() {
            self.builder.add_lea(
                target_register,
                inner.addr(),
                node,
                "wasn't a pointer, so converting",
            );
        }

        GeneratedExpressionResult::default()
    }

    fn emit_post_call(
        &mut self,
        spilled_arguments: &[SpilledRegister],
        copy_back: &[MutableReturnReg],
        node: &Node,
        comment: &str,
    ) {
        for copy_back in copy_back {
            match &copy_back.target_location_after_call {
                DetailedLocation::Register { reg } => {
                    self.builder
                        .add_mov_reg(reg, &copy_back.parameter_reg, node, "copy back reg");
                }
                DetailedLocation::Memory {
                    base_ptr_reg,
                    offset,
                    ..
                } => self.store_register_contents_to_memory(
                    node,
                    base_ptr_reg,
                    *offset,
                    &copy_back.parameter_reg,
                    "copy back from mem",
                ),
            }
        }
        self.emit_restore_spilled_registers(spilled_arguments, node, comment);
    }

    pub fn emit_restore_spilled_registers(
        &mut self,
        spilled_arguments: &[SpilledRegister],
        node: &Node,
        comment: &str,
    ) {
        for arg in spilled_arguments {
            self.builder.add_ld_regs_from_frame(
                &arg.register,
                arg.frame_memory_region.addr,
                1, // TODO: Calculate a count for all spilled registers
                node,
                &format!("restoring spilled arguments {comment}"),
            );
        }
    }

    /// # Panics
    ///
    #[allow(clippy::single_match_else)]
    pub fn emit_rvalue(&mut self, expr: &Expression, ctx: &Context) -> TypedRegister {
        let (target_region, z_flag_state) = self.emit_rvalue_leave_z_flag_if_possible(expr, ctx);

        self.materialize_z_flag_to_bool_if_needed(&target_region, z_flag_state, &expr.node);

        target_region
    }

    pub fn temp_space_for_type(&mut self, ty: &Type, comment: &str) -> TempRegister {
        let layout = layout_type(ty);

        let temp_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(layout),
            &format!("temp space {comment}"),
        );

        temp_reg
    }

    pub fn emit_rvalue_leave_z_flag_if_possible(
        &mut self,
        expr: &Expression,
        ctx: &Context,
    ) -> (TypedRegister, GeneratedExpressionResult) {
        match &expr.kind {
            ExpressionKind::VariableAccess(var_ref) => {
                let frame_address = self
                    .variable_registers
                    .get(&var_ref.unique_id_within_function)
                    .unwrap();

                return (frame_address.clone(), GeneratedExpressionResult::default());
            }

            _ => {}
        }

        let temp_reg = self.temp_space_for_type(&expr.ty, "expression");

        let z_flag_state = self.emit_expression(temp_reg.register(), expr, ctx);
        let hack = temp_reg.register.clone();

        (hack, z_flag_state)
    }
}
