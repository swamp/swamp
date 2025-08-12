/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::alloc::StackFrameAllocator;
use crate::ctx::Context;
use crate::err;
use crate::reg_pool::HwmTempRegisterPool;
use crate::state::CodeGenState;
use seq_map::SeqMap;
use source_map_cache::{
    KeepTrackOfSourceLine, SourceFileLineInfo, SourceMapLookup, SourceMapWrapper,
};
use source_map_node::Node;
use swamp_semantic::{
    ArgumentExpression, BooleanExpression, ConstantRef, Expression, SingleLocationExpression,
    UnaryOperator, UnaryOperatorKind, VariableRef,
};
use swamp_types::TypeKind;
use swamp_vm_instr_build::{InstructionBuilder, PatchPosition};
use swamp_vm_isa::aligner::{align, SAFE_ALIGNMENT};
use swamp_vm_isa::{
    FrameMemorySize,
    MemoryOffset, MemorySize, ANY_HEADER_HASH_OFFSET, ANY_HEADER_PTR_OFFSET, ANY_HEADER_SIZE_OFFSET,
    REG_ON_FRAME_ALIGNMENT, REG_ON_FRAME_SIZE,
};
use swamp_vm_types::types::BasicTypeKind;
use swamp_vm_types::types::{
    b8_type, u32_type, u8_type, BasicTypeRef, Destination, TypedRegister, VmType,
};
use swamp_vm_types::{AggregateMemoryLocation, FrameMemoryRegion, MemoryLocation, PointerLocation};
use tracing::info;

#[derive(Copy, Clone)]
pub struct CodeBuilderOptions {
    pub should_show_debug: bool,
}
pub struct CodeBuilder<'a> {
    pub state: &'a mut CodeGenState,
    pub builder: &'a mut InstructionBuilder<'a>,
    pub variable_registers: SeqMap<usize, TypedRegister>,
    pub temp_registers: HwmTempRegisterPool,
    pub frame_allocator: StackFrameAllocator,
    pub debug_line_tracker: KeepTrackOfSourceLine,
    //pub spilled_registers: SpilledRegisterScopes,
    pub source_map_lookup: &'a SourceMapWrapper<'a>,
    pub options: CodeBuilderOptions,
    pub errors: Vec<err::Error>,
}

impl<'a> CodeBuilder<'a> {
    pub fn new(
        state: &'a mut CodeGenState,
        builder: &'a mut InstructionBuilder<'a>,
        variable_registers: SeqMap<usize, TypedRegister>,
        temp_registers: HwmTempRegisterPool,
        temp_allocator: StackFrameAllocator,
        options: CodeBuilderOptions,
        source_map_lookup: &'a SourceMapWrapper<'a>,
    ) -> Self {
        Self {
            state,
            builder,
            variable_registers,
            //frame_memory_registers,
            temp_registers,
            frame_allocator: temp_allocator,
            debug_line_tracker: KeepTrackOfSourceLine::default(),
            options,
            source_map_lookup,
            errors: Vec::new(),
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

                let target_pointer_location = PointerLocation {
                    ptr_reg: target_reg.clone(),
                };
                let source_pointer_location = PointerLocation {
                    ptr_reg: source_reg.clone(),
                };
                self.builder.add_block_copy_with_immediate_size(
                    &target_pointer_location,
                    &source_pointer_location,
                    size,
                    node,
                    &format!("emit_copy_register.copy struct. {comment}"),
                );
            }
        } else {
            {
                self.builder.add_mov_reg(
                    target_reg,
                    source_reg,
                    node,
                    &format!("emit_copy_register. primitive to primitive. {comment}"),
                );
            }
        }
    }

    #[must_use]
    pub fn total_aligned_frame_size(&self) -> FrameMemorySize {
        let aligned = align(
            self.frame_allocator.addr().as_size().0 as usize,
            SAFE_ALIGNMENT,
        );
        FrameMemorySize(aligned as u32)
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
            line_info.relative_file_name, line_info.row, line_info.col, span_text
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
            UnaryOperatorKind::Not => match &*unary_operator.left.ty.kind {
                TypeKind::Bool => {
                    let t_flag = self.emit_unary_operator_logical(target_reg, unary_operator, ctx);
                    self.force_normalized_bool_reg_if_needed(target_reg, t_flag, node);
                }
                _ => panic!("unknown not op"),
            },

            UnaryOperatorKind::Negate => match &*unary_operator.left.ty.kind {
                TypeKind::Int => {
                    let left_source = self.emit_scalar_rvalue(&unary_operator.left, ctx);
                    self.builder
                        .add_neg_i32(target_reg, &left_source, node, "negate i32");
                }

                TypeKind::Float => {
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

    pub(crate) fn temp_frame_space_for_register(
        &mut self,
        count: u8,
        comment: &str,
    ) -> FrameMemoryRegion {
        let total_size = MemorySize(REG_ON_FRAME_SIZE.0 * u32::from(count));
        let start = self
            .frame_allocator
            .allocate(total_size, REG_ON_FRAME_ALIGNMENT);

        FrameMemoryRegion {
            addr: start,
            size: total_size,
        }
    }

    pub(crate) fn emit_option_expression_into_target_memory_location(
        &mut self,
        output: &Destination,
        node: &Node,
        maybe_option: Option<&Expression>,
        ctx: &Context,
    ) {
        let memory_target = output.memory_location_or_pointer_reg();
        let memory_lvalue_location = AggregateMemoryLocation::new(memory_target);

        let hwm = self.temp_registers.save_mark();

        let tag_reg = self
            .temp_registers
            .allocate(VmType::new_unknown_placement(u8_type()), "emit_option tag");

        if let Some(some_expression) = maybe_option {
            let union_information = memory_lvalue_location
                .location
                .ty
                .basic_type()
                .optional_info()
                .unwrap()
                .clone();

            {
                // Overwrite the tag with 1 (`Some`)
                let ty = memory_lvalue_location.location.ty.basic_type();
                self.builder.add_mov8_immediate(
                    tag_reg.register(),
                    1,
                    node,
                    &format!("set the tag Some (1) in register {ty}"),
                );
                // for options, we know that the tag size is one byte
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

            // For `none` we simply overwrite the tag with zero
            self.builder.add_st8_using_ptr_with_offset(
                &memory_lvalue_location.location,
                tag_reg.register(),
                node,
                "store optional None tag",
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
                self.emit_statement(expr, ctx);
            }
            if matches!(&*last.ty.kind, TypeKind::Unit) {
                self.emit_statement(last, ctx);
            } else {
                self.emit_expression(target_reg, last, ctx);
            }
        } else {
            // empty blocks are allowed for side effects
        }
    }

    pub(crate) fn get_variable_register(&self, variable: &VariableRef) -> &TypedRegister {
        //info!(unique_id=?variable.unique_id_within_function, name=?variable.assigned_name, "trying to fetch");
        self.variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap()
    }

    pub fn allocate_frame_space_and_return_absolute_pointer_reg(
        &mut self,
        ty: &BasicTypeRef,
        node: &Node,
        comment: &str,
    ) -> TypedRegister {
        let frame_placed_type = self.frame_allocator.allocate_type(ty);

        let temp = self.temp_registers.allocate(
            VmType::new_frame_placed(frame_placed_type),
            &format!("{comment}: allocate frame space"),
        );

        self.builder.add_lea_from_frame_region(
            &temp.register,
            temp.register.region(),
            node,
            &format!("{comment}: set the allocated memory to pointer reg"),
        );

        temp.register
    }

    pub fn allocate_frame_space_and_return_pointer_location(
        &mut self,
        ty: &BasicTypeRef,
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
        ty: &BasicTypeRef,
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
        ty: &BasicTypeRef,
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
                    "load constant pointer '{}' type:{}",
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

            self.emit_copy_value_from_memory_location(
                output,
                &source_memory_location,
                node,
                &format!("copy to target memory {output_reg} from constant memory area {source_memory_location}"),
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
                &format!("put constant into memory {output_memory_location} <- {temp_reg}"),
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

    pub(crate) fn emit_coerce_int_to_char(
        &mut self,
        target_reg: &TypedRegister,
        expr: &Expression,
        ctx: &Context,
    ) {
        let destination = Destination::Register(target_reg.clone());

        // Since Char (u32) is same size as Int(i32), we can just use it directly
        self.emit_expression(&destination, expr, ctx);
    }

    pub(crate) fn emit_coerce_int_to_byte(
        &mut self,
        output: &Destination,
        expr: &Expression,
        ctx: &Context,
    ) {
        // Since u32 is same size as byte (a register), we can just use it directly
        self.emit_expression(output, expr, ctx);

        match output {
            Destination::Unit => {}
            Destination::Register(dest_reg) => {
                self.builder
                    .add_check_u8(dest_reg, &expr.node, "trunc int to byte");
            }
            Destination::Memory(mem) => {
                let hwm = self.temp_registers.save_mark();
                let temp_u8 = self
                    .temp_registers
                    .allocate(VmType::new_contained_in_register(u8_type()), "temp u8");
                self.builder.add_ld8_from_pointer_with_offset(
                    temp_u8.register(),
                    &mem.base_ptr_reg,
                    mem.offset,
                    &expr.node,
                    "load it to check it",
                );
                self.builder
                    .add_check_u8(temp_u8.register(), &expr.node, "trunc int to byte");
                self.temp_registers.restore_to_mark(hwm);
            }
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
        self.builder.add_ld8_from_pointer_with_offset(
            target_reg,
            &base_pointer_of_tagged_union_reg,
            MemoryOffset(0),
            &expr.node,
            "load option tag to bool register",
        );
    }

    pub(crate) fn emit_coerce_to_any(
        &mut self,
        output: &Destination,
        expr: &Expression,
        ctx: &Context,
    ) {
        //info!(?target_reg.ty, "it wants to coerce this to bool");

        let source_aggregate_pointer = self.emit_scalar_rvalue(expr, ctx);

        let pointer_register = self.emit_compute_effective_address_to_register(
            output,
            &expr.node,
            "get starting ptr to output",
        );
        let output_aggregate_location = AggregateMemoryLocation::new(
            MemoryLocation::new_copy_over_whole_type_with_zero_offset(pointer_register),
        );

        self.builder.add_st32_using_ptr_with_offset(
            &output_aggregate_location
                .offset(ANY_HEADER_PTR_OFFSET, u32_type())
                .location,
            &source_aggregate_pointer,
            &expr.node,
            "store aggregate pointer into Any Header",
        );
        let temp_size = self.temp_registers.allocate(
            VmType::new_contained_in_register(u32_type()),
            "Any header size temp",
        );

        self.builder.add_mov_32_immediate_value(
            temp_size.register(),
            source_aggregate_pointer.ty.basic_type.total_size.0,
            &expr.node,
            "fixed size",
        );
        self.builder.add_st32_using_ptr_with_offset(
            &output_aggregate_location
                .offset(ANY_HEADER_SIZE_OFFSET, u32_type())
                .location,
            temp_size.register(),
            &expr.node,
            "copy size into Any Header",
        );

        self.builder.add_mov_32_immediate_value(
            temp_size.register(),
            source_aggregate_pointer.ty.basic_type.universal_hash_u64() as u32,
            &expr.node,
            "reuse for hash",
        );
        self.builder.add_st32_using_ptr_with_offset(
            &output_aggregate_location
                .offset(ANY_HEADER_HASH_OFFSET, u32_type())
                .location,
            temp_size.register(),
            &expr.node,
            "copy size into Any Header",
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

    pub fn debug_expression(&mut self, expr: &Expression, description: &str) {
        let node = &expr.node;
        let (line, _column) = self
            .source_map_lookup
            .source_map
            .get_span_location_utf8(node.span.file_id, node.span.offset as usize);
        let source_line_info = SourceFileLineInfo {
            row: line,
            file_id: node.span.file_id as usize,
        };

        let answer = self.debug_line_tracker.check_if_new_line(&source_line_info);
        if let Some((start, end)) = answer {
            let relative_file_name = self.source_map_lookup.get_relative_path(node.span.file_id);
            let (line, col) = self
                .source_map_lookup
                .source_map
                .get_span_location_utf8(node.span.file_id, node.span.offset as usize);
            let source_line = self
                .source_map_lookup
                .source_map
                .get_source_line(node.span.file_id, line)
                .unwrap_or("<source line not found>");

            info!(
                file=%relative_file_name,
                line=%line,
                col=%col,
                source=%source_line,
                "{}",
                description
            );
        }
    }
}
