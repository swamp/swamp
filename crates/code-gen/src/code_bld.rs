/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::ArgumentAndTempScope;
use crate::alloc::ScopeAllocator;
use crate::ctx::Context;
use crate::reg_pool::{HwmTempRegisterPool, RegisterPool};
use crate::state::CodeGenState;
use seq_map::SeqMap;
use source_map_cache::{SourceMapLookup, SourceMapWrapper};
use source_map_node::Node;
use swamp_semantic::{
    ArgumentExpression, BooleanExpression, ConstantRef, Expression, SingleLocationExpression,
    UnaryOperator, UnaryOperatorKind, VariableRef,
};
use swamp_types::Type;
use swamp_vm_instr_build::{InstructionBuilder, PatchPosition};
use swamp_vm_types::aligner::{SAFE_ALIGNMENT, align};
use swamp_vm_types::types::{
    BasicType, Destination, FramePlacedType, TypedRegister, VmType, b8_type, u8_type, u32_type,
};
use swamp_vm_types::{
    AggregateMemoryLocation, FrameMemoryRegion, FrameMemorySize, MemoryLocation, MemoryOffset,
    MemorySize, PointerLocation, REG_ON_FRAME_ALIGNMENT, REG_ON_FRAME_SIZE,
};

pub struct EmitArgumentInfo {
    pub argument_and_temp_scope: ArgumentAndTempScope,
    pub copy_back_of_registers_mutated_by_callee: Vec<MutableReturnReg>,
}

pub struct MutableReturnReg {
    pub target_location_after_call: Destination,
    pub parameter_reg: TypedRegister,
}

pub(crate) struct CodeBuilder<'a> {
    pub state: &'a mut CodeGenState,
    pub(crate) builder: &'a mut InstructionBuilder<'a>,
    pub(crate) variable_registers: SeqMap<usize, TypedRegister>,
    frame_memory_registers: RegisterPool,
    pub(crate) temp_registers: HwmTempRegisterPool,
    pub(crate) frame_allocator: ScopeAllocator,
    //pub spilled_registers: SpilledRegisterScopes,
    pub source_map_lookup: &'a SourceMapWrapper<'a>,
}

impl<'a> CodeBuilder<'a> {
    pub const fn new(
        state: &'a mut CodeGenState,
        builder: &'a mut InstructionBuilder<'a>,
        variable_registers: SeqMap<usize, TypedRegister>,
        frame_memory_registers: RegisterPool,
        temp_registers: HwmTempRegisterPool,
        temp_allocator: ScopeAllocator,
        source_map_lookup: &'a SourceMapWrapper<'a>,
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
    pub(crate) fn emit_copy_register(
        &mut self,
        target_reg: &TypedRegister,
        source_reg: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        if source_reg.ty.is_mutable_reference_semantic() {
            if target_reg.ty().is_mutable_reference() {
                self.builder.add_mov_reg(
                    target_reg,
                    source_reg,
                    node,
                    &format!("emit_copy_register. ptr to ptr. {comment}"),
                );
            } else {
                let size = source_reg.size();

                self.builder.add_block_copy(
                    target_reg,
                    source_reg,
                    size,
                    node,
                    &format!("emit_copy_register.copy struct. {comment}"),
                );
            }
        } else {
            self.builder.add_mov_reg(
                target_reg,
                source_reg,
                node,
                &format!("emit_copy_register. primitive to primitive. {comment}"),
            );
        }
    }

    pub(crate) fn add_ld_regs_from_frame(
        &mut self,
        start_reg: &TypedRegister,
        start_address: FrameMemoryRegion,
        count: u8,
        node: &Node,
        comment: &str,
    ) {
        self.builder.add_ld_contiguous_regs_from_frame(
            start_reg.index,
            start_address,
            count,
            node,
            comment,
        );
    }

    pub fn total_aligned_frame_size(&self) -> FrameMemorySize {
        let aligned = align(
            self.frame_allocator.addr().as_size().0 as usize,
            SAFE_ALIGNMENT,
        );
        FrameMemorySize(aligned as u16)
    }

    pub fn patch_enter(&mut self, patch_position: PatchPosition) {
        self.builder
            .patch_enter(self.total_aligned_frame_size(), patch_position);
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

    pub(crate) fn emit_unary_operator(
        &mut self,
        target_reg: &TypedRegister,
        unary_operator: &UnaryOperator,
        ctx: &Context,
    ) {
        let node = &unary_operator.node;
        match &unary_operator.kind {
            UnaryOperatorKind::Not => match &unary_operator.left.ty.underlying() {
                Type::Bool => {
                    let t_flag = self.emit_unary_operator_logical(target_reg, unary_operator, ctx);
                }
                _ => panic!("unknown not op"),
            },

            UnaryOperatorKind::Negate => match &unary_operator.left.ty {
                Type::Int => {
                    let left_source = self.emit_scalar_rvalue(&unary_operator.left, ctx);
                    self.builder
                        .add_neg_i32(target_reg, &left_source, node, "negate i32");
                }

                Type::Float => {
                    let left_source = self.emit_scalar_rvalue(&unary_operator.left, ctx);
                    self.builder
                        .add_neg_f32(target_reg, &left_source, node, "negate f32");
                }
                _ => panic!("negate should only be possible on Int and Float"),
            },
        }
    }

    pub(crate) fn emit_if(
        &mut self,
        output_destination: &Destination,
        condition: &BooleanExpression,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) {
        let jump_on_false_condition = self.emit_condition_context(condition, ctx);

        // True expression just takes over our target
        // Both to reuse the current target, and for the fact when there is no else
        self.emit_expression(output_destination, true_expr, ctx);

        if let Some(false_expr) = maybe_false_expr {
            // we need to help the true expression to jump over false
            let skip_false_if_true = self.builder.add_jump_placeholder(
                &condition.expression.node,
                "since it was true, skip over false section",
            );

            // If the expression was false, it should continue here
            self.builder.patch_jump_here(jump_on_false_condition);

            // Else expression also can just take over our if target
            self.emit_expression(output_destination, false_expr, ctx);

            self.builder.patch_jump_here(skip_false_if_true);
        } else {
            self.builder.patch_jump_here(jump_on_false_condition);
        }
    }

    fn emit_variable_binding(
        &mut self,
        variable: &VariableRef,
        mut_or_immutable_expression: &ArgumentExpression,
        ctx: &Context,
    ) {
        let target_relative_frame_pointer = self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| panic!("{}", variable.assigned_name))
            .clone();

        self.emit_argument_expression_binding(
            &target_relative_frame_pointer,
            mut_or_immutable_expression,
            ctx,
        );
    }

    pub(crate) fn temp_frame_space_for_register(
        &mut self,
        count: u8,
        comment: &str,
    ) -> FrameMemoryRegion {
        let total_size = MemorySize(REG_ON_FRAME_SIZE.0 * u16::from(count));
        let start = self
            .frame_allocator
            .allocate(total_size, REG_ON_FRAME_ALIGNMENT);

        //info!(?start, comment, "allocating register space on frame");

        FrameMemoryRegion {
            addr: start,
            size: total_size,
        }
    }

    pub(crate) fn emit_option_expression_into_target_memory_location(
        &mut self,
        memory_lvalue_location: &AggregateMemoryLocation,
        node: &Node,
        maybe_option: Option<&Expression>,
        ctx: &Context,
    ) {
        let hwm = self.temp_registers.save_mark();

        let tag_reg = self
            .temp_registers
            .allocate(VmType::new_unknown_placement(u8_type()), "emit_option tag");

        if let Some(some_expression) = maybe_option {
            let union_information = memory_lvalue_location
                .location
                .ty
                .underlying()
                .optional_info()
                .unwrap()
                .clone();

            {
                self.builder.add_mov8_immediate(
                    tag_reg.register(),
                    1,
                    node,
                    "set the tag Some (1) in register",
                );
                self.builder.add_st8_using_ptr_with_offset(
                    &memory_lvalue_location
                        .offset(union_information.tag_offset, b8_type())
                        .location,
                    tag_reg.register(),
                    node,
                    "store optional Some tag",
                );
            }
            {
                let payload_location = &memory_lvalue_location
                    .offset(union_information.payload_offset, b8_type())
                    .location;
                self.emit_expression_into_target_memory(
                    payload_location,
                    some_expression,
                    "store option payload",
                    ctx,
                );
            }
        } else {
            self.builder
                .add_mov8_immediate(tag_reg.register(), 0, node, "option None tag"); // 0 signals `None`

            self.builder.add_st8_using_ptr_with_offset(
                &memory_lvalue_location.location,
                tag_reg.register(),
                node,
                "store optional Some tag",
            );
        }
        self.temp_registers.restore_to_mark(hwm);
    }

    pub(crate) fn emit_block(
        &mut self,
        target_reg: &Destination,
        expressions: &[Expression],
        ctx: &Context,
    ) {
        if let Some((last, others)) = expressions.split_last() {
            for expr in others {
                // info!("this is others in block");
                self.emit_statement(expr, ctx);
            }
            if last.ty.is_unit() {
                self.emit_statement(last, ctx);
            } else {
                //            info!(?last.ty, ?target_reg.ty, "this is the last in the block!");
                self.emit_expression(target_reg, last, ctx);
            }
        } else {
            // empty blocks are allowed for side effects
        }
    }

    pub(crate) fn get_variable_register(&self, variable: &VariableRef) -> &TypedRegister {
        (self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap()) as _
    }

    fn get_variable_frame_placed(&self, variable: &VariableRef) -> FramePlacedType {
        let frame_address = self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap();

        frame_address.frame_placed()
    }

    pub(crate) fn referenced_or_not_type(ty: &Type) -> Type {
        if let Type::MutableReference(inner_type) = ty {
            *inner_type.clone()
        } else {
            ty.clone()
        }
    }

    pub fn allocate_frame_space_and_return_absolute_pointer_reg(
        &mut self,
        ty: &BasicType,
        node: &Node,
        comment: &str,
    ) -> TypedRegister {
        let frame_placed_type = self.frame_allocator.allocate_type(ty.clone());

        let reg = self.frame_memory_registers.alloc_register(
            VmType::new_frame_placed(frame_placed_type),
            &format!("{comment}: allocate frame space"),
        );

        self.builder.add_lea_from_frame_region(
            &reg,
            reg.region(),
            node,
            &format!("{comment}: set the allocated memory to pointer reg"),
        );

        reg
    }

    pub fn allocate_frame_space_and_return_pointer_location(
        &mut self,
        ty: &BasicType,
        node: &Node,
        comment: &str,
    ) -> PointerLocation {
        let absolute_base_ptr_reg =
            self.allocate_frame_space_and_return_absolute_pointer_reg(ty, node, comment);
        PointerLocation {
            ptr_reg: absolute_base_ptr_reg,
        }
    }

    pub fn allocate_frame_space_and_return_memory_location(
        &mut self,
        ty: &BasicType,
        node: &Node,
        comment: &str,
    ) -> MemoryLocation {
        let absolute_base_ptr_reg =
            self.allocate_frame_space_and_return_pointer_location(ty, node, comment);
        MemoryLocation {
            ty: absolute_base_ptr_reg.ptr_reg.ty.clone(),
            base_ptr_reg: absolute_base_ptr_reg.ptr_reg,
            offset: MemoryOffset(0),
        }
    }

    pub fn allocate_frame_space_and_return_destination_to_it(
        &mut self,
        ty: &BasicType,
        node: &Node,
        comment: &str,
    ) -> Destination {
        let location = self.allocate_frame_space_and_return_memory_location(ty, node, comment);
        Destination::new_location(location)
    }

    pub(crate) fn emit_constant_access(
        &mut self,
        output: &Destination,
        constant_reference: &ConstantRef,
        node: &Node,
        ctx: &Context,
    ) {
        let constant_region = self
            .state
            .constant_offsets
            .get(&constant_reference.id)
            .unwrap();
        // TODO: Bring this back// assert_eq!(target_reg.size(), constant_region.size());

        if constant_region.ty().is_aggregate() {
            // load the known constant address into a temp register to use as a base for the block copy
            let source_base_ptr = self.temp_registers.allocate(
                VmType::new_contained_in_register(u32_type()),
                "temp register for the base pointer to the constant",
            );
            self.builder.add_mov_32_immediate_value(
                source_base_ptr.register(),
                constant_region.addr().0,
                node,
                &format!(
                    "load constant pointer '{}' {:?}",
                    constant_reference.assigned_name,
                    constant_region.ty()
                ),
            );

            let source_memory_location = MemoryLocation {
                base_ptr_reg: source_base_ptr.register,
                offset: MemoryOffset(0),
                ty: VmType::new_heap_placement(
                    constant_region.ty().clone(),
                    constant_region.region(),
                ),
            };

            let output_reg = output.memory_location_or_pointer_reg();

            self.emit_copy_aggregate_value_helper(
                &output_reg,
                &source_memory_location,
                node,
                "copy from constant memory area to target memory",
            );
        } else if let Some(output_memory_location) = output.memory_location() {
            let hwm = self.temp_registers.save_mark();
            let temp_reg = self.temp_registers.allocate(
                VmType::new_contained_in_register(constant_region.ty().clone()),
                "temporary for constant",
            );

            self.emit_load_scalar_from_absolute_address_instruction(
                temp_reg.register(),
                constant_region.addr(),
                &VmType::new_unknown_placement(constant_region.ty().clone()),
                node,
                &format!(
                    "load constant primitive '{}' {:?}",
                    constant_reference.assigned_name,
                    constant_region.ty()
                ),
            );

            self.emit_store_scalar_to_memory_offset_instruction(
                output_memory_location,
                temp_reg.register(),
                node,
                "put constant into memory",
            );

            self.temp_registers.restore_to_mark(hwm);
        } else {
            self.emit_load_scalar_from_absolute_address_instruction(
                output.grab_register(),
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
    }

    pub(crate) fn emit_coerce_option_to_bool(
        &mut self,
        target_reg: &TypedRegister,
        expr: &Expression,
        ctx: &Context,
    ) {
        //info!(?target_reg.ty, "it wants to coerce this to bool");

        let base_pointer_of_tagged_union_reg = self.emit_scalar_rvalue(expr, ctx);

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
    }

    pub(crate) fn merge_arguments_keep_literals(
        outer_args: &Vec<ArgumentExpression>,
        intrinsic_args: &Vec<ArgumentExpression>,
    ) -> Vec<ArgumentExpression> {
        // HACK: we assume that the parameters are in the same order.
        // If one has more arguments, we assume that those extra arguments are in the end
        // We also assume that the first is self
        let mut all_args = outer_args.clone();

        if intrinsic_args.len() > outer_args.len() + 1 {
            all_args.extend_from_slice(&intrinsic_args[outer_args.len() + 1..]);
        }

        all_args
    }

    pub(crate) fn emit_borrow_mutable_reference(
        &mut self,
        target_register: &TypedRegister,
        node: &Node,
        expr: &SingleLocationExpression,
        ctx: &Context,
    ) {
        let location = self.emit_lvalue_address(expr, ctx);

        let abs_pointer = self.emit_compute_effective_address_to_register(
            &location,
            node,
            "calculate absolute address for reference",
        );

        self.builder.add_mov_reg(
            target_register,
            &abs_pointer,
            node,
            "copy calculated address for borrow",
        );
    }
}
