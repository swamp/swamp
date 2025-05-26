use crate::alloc::ScopeAllocator;
use crate::ctx::Context;
use crate::reg_pool::{HwmTempRegisterPool, RegisterPool};
use crate::state::CodeGenState;
use crate::{ArgumentAndTempScope, DetailedLocationResolved};
use seq_map::SeqMap;
use source_map_cache::{SourceMapLookup, SourceMapWrapper};
use source_map_node::Node;
use swamp_semantic::{
    BooleanExpression, ConstantRef, Expression, MutRefOrImmutableExpression,
    SingleLocationExpression, UnaryOperator, UnaryOperatorKind, VariableRef,
};
use swamp_types::Type;
use swamp_vm_instr_build::{InstructionBuilder, PatchPosition};
use swamp_vm_types::aligner::{SAFE_ALIGNMENT, align};
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, Destination, FramePlacedType, RValueOrLValue, TypedRegister, VmType,
    b8_type, u8_type, u32_type,
};
use swamp_vm_types::{
    AggregateMemoryLocation, FrameMemoryRegion, FrameMemorySize, HeapMemoryAddress, MemoryLocation,
    MemoryOffset, MemorySize, REG_ON_FRAME_ALIGNMENT, REG_ON_FRAME_SIZE,
};

pub struct EmitArgumentInfo {
    pub argument_and_temp_scope: ArgumentAndTempScope,
    pub copy_back_of_registers_mutated_by_callee: Vec<MutableReturnReg>,
}

pub(crate) struct MutableReturnReg {
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
        self.builder
            .add_ld_regs_from_frame(start_reg.index, start_address, count, node, comment);
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
                    &format!("{comment} - load u8 primitive from memory"),
                );
            }
            BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                self.builder.add_ld32_from_absolute_memory_address(
                    target_reg,
                    &source_offset,
                    node,
                    &format!("{comment} - load u32 primitive from memory"),
                );
            }
            _ => panic!("this is not a primitive {type_at_offset:?}"),
        }
    }

    pub(crate) fn emit_load_into_register(
        &mut self,
        target_reg: &TypedRegister,
        source: &Destination,
        node: &Node,
        comment: &str,
    ) {
        match source {
            Destination::Register(source_reg) => {
                if target_reg.index != source_reg.index {
                    self.emit_copy_register(target_reg, source_reg, node, comment);
                }
            }
            Destination::Memory(memory_location) => {
                self.emit_load_from_memory(
                    target_reg,
                    &memory_location.base_ptr_reg,
                    memory_location.offset,
                    &memory_location.ty,
                    node,
                    comment,
                );
            }
            Destination::Unit => panic!("Cannot load from Unit destination"),
        }
    }

    pub(crate) fn emit_store_to_pointer_target(
        &mut self,
        pointer_reg: &TypedRegister,
        value_source: &Destination,
        node: &Node,
        comment: &str,
    ) {
        let offset = match value_source {
            Destination::Memory(mem_loc) => mem_loc.offset,
            _ => MemoryOffset(0),
        };

        match value_source {
            Destination::Register(value_reg) => match value_reg.ty.basic_type.kind {
                BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                    self.builder.add_st32_using_ptr_with_offset(
                        &MemoryLocation {
                            base_ptr_reg: pointer_reg.clone(),
                            offset,
                            ty: value_reg.ty.clone(),
                        },
                        value_reg,
                        node,
                        &format!("store {comment} to memory pointed by register {pointer_reg} <- {value_reg}"),
                    );
                }
                BasicTypeKind::U8 | BasicTypeKind::B8 => {
                    self.builder.add_st8_using_ptr_with_offset(
                        &MemoryLocation {
                            base_ptr_reg: pointer_reg.clone(),
                            offset,
                            ty: value_reg.ty.clone(),
                        },
                        value_reg,
                        node,
                        &format!("store byte {comment} to memory pointed by register {pointer_reg} <- {value_reg}"),
                    );
                }
                _ => {
                    self.builder.add_block_copy_with_offset(
                        &MemoryLocation {
                            base_ptr_reg: pointer_reg.clone(),
                            offset,
                            ty: value_reg.ty.clone(),
                        },
                        value_reg,
                        MemoryOffset(0),
                        value_reg.ty.basic_type.total_size,
                        node,
                        &format!("block copy {comment} to memory pointed by register {pointer_reg} <- {value_reg}"),
                    );
                }
            },
            Destination::Memory(source_mem_loc) => {
                let temp_reg = self
                    .temp_registers
                    .allocate(source_mem_loc.ty.clone(), "temp_for_memory_to_memory_store");

                self.emit_load_from_memory(
                    temp_reg.register(),
                    &source_mem_loc.base_ptr_reg,
                    source_mem_loc.offset,
                    &source_mem_loc.ty,
                    node,
                    &format!("load {comment} from memory for store"),
                );

                match source_mem_loc.ty.basic_type.kind {
                    BasicTypeKind::S32 | BasicTypeKind::Fixed32 | BasicTypeKind::U32 => {
                        self.builder.add_st32_using_ptr_with_offset(
                            &MemoryLocation {
                                base_ptr_reg: pointer_reg.clone(),
                                offset,
                                ty: source_mem_loc.ty.clone(),
                            },
                            temp_reg.register(),
                            node,
                            &format!("store {comment} from temp to memory pointed by register"),
                        );
                    }
                    _ => {
                        self.builder.add_block_copy_with_offset(
                            &MemoryLocation {
                                base_ptr_reg: pointer_reg.clone(),
                                offset,
                                ty: source_mem_loc.ty.clone(),
                            },
                            temp_reg.register(),
                            MemoryOffset(0),
                            source_mem_loc.ty.basic_type.total_size,
                            node,
                            &format!(
                                "block copy {comment} from temp to memory pointed by register"
                            ),
                        );
                    }
                }
            }
            Destination::Unit => panic!("Cannot store from Unit source"),
        }
    }

    pub(crate) fn emit_load_from_memory(
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
                self.builder
                    .add_mov_reg(target_reg, base_ptr_reg, node, "copy pointer reg to reg");

                /*
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

                 */
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

    pub(crate) fn emit_load_primitive_from_detailed_location_if_needed(
        &mut self,
        location: &Destination,
        node: &Node,
        comment: &str,
    ) -> DetailedLocationResolved {
        match location {
            Destination::Register(reg) => DetailedLocationResolved::Register(reg.clone()),
            Destination::Memory(memory_location) => {
                let temp_reg_target = self.temp_registers.allocate(
                    memory_location.ty.clone(),
                    "emit load primitive from location",
                );
                self.emit_load_from_memory(
                    temp_reg_target.register(),
                    &memory_location.base_ptr_reg,
                    memory_location.offset,
                    &memory_location.ty,
                    node,
                    &format!("load primitive from detailed location {comment}"),
                );
                DetailedLocationResolved::TempRegister(temp_reg_target)
            }
            Destination::Unit => {
                panic!("")
            }
        }
    }

    pub(crate) fn emit_ptr_reg_from_detailed_location(
        &mut self,
        location: &Destination,
        node: &Node,
        comment: &str,
    ) -> TypedRegister {
        match location {
            Destination::Register(reg) => reg.clone(),
            Destination::Memory(memory_location) => {
                //let hwm = self.temp_registers.save_mark();
                let offset_temp_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u32_type()),
                    &format!("{comment} (emit_ptr_reg_from_location_offset)"),
                );
                self.builder.add_mov_32_immediate_value(
                    offset_temp_reg.register(),
                    u32::from(memory_location.offset.0),
                    node,
                    &format!("{comment} (load offset value)"),
                );
                let final_ptr_target_reg = self
                    .temp_registers
                    .allocate(memory_location.ty.clone(), "final_ptr_target_reg");
                self.builder.add_add_u32(
                    final_ptr_target_reg.register(),
                    &memory_location.base_ptr_reg,
                    offset_temp_reg.register(),
                    node,
                    &format!("{comment} (add to resolved new base_ptr)"),
                );
                final_ptr_target_reg.register().clone()
            }
            Destination::Unit => {
                panic!("not sure")
            }
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
                    let t_flag = self.emit_unary_operator_logical_to_t_flag(unary_operator, ctx);
                    self.materialize_t_flag_to_bool_if_needed(
                        target_reg,
                        t_flag,
                        &unary_operator.node,
                    );
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

    pub(crate) fn emit_absolute_pointer_if_needed(
        &mut self,
        destination: &Destination,
        node: &Node,
        comment: &str,
    ) -> TypedRegister {
        match destination {
            Destination::Register(reg) => reg.clone(),
            Destination::Memory(memory_location) => {
                if memory_location.offset.0 == 0 {
                    return memory_location.base_ptr_reg.clone();
                }

                let temp_offset_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u32_type()),
                    "emit_absolute_pointer: temp_offset_reg",
                );

                self.builder.add_add_u32_imm(
                    temp_offset_reg.register(),
                    &memory_location.base_ptr_reg,
                    u32::from(memory_location.offset.0),
                    node,
                    "forcing lvalue to be a complete pointer",
                );

                temp_offset_reg.register
            }
            Destination::Unit => {
                panic!("can not get absolute pointer")
            }
        }
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
                //i nfo!("this is others in block");
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

    pub(crate) fn emit_expression_location_mut_ref_or_immutable(
        &mut self,
        mut_or_immutable_expression: &MutRefOrImmutableExpression,
        ctx: &Context,
    ) -> RValueOrLValue {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                RValueOrLValue::Scalar(self.emit_scalar_rvalue(found_expression, ctx))
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                RValueOrLValue::Memory(self.emit_lvalue_address(location_expression, ctx))
            }
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
            "allocate frame space",
        );

        self.builder.add_lea(
            &reg,
            reg.addr(),
            node,
            "set the allocated memory to pointer reg",
        );

        reg
    }

    pub fn allocate_frame_space_and_return_destination_to_it(
        &mut self,
        ty: &BasicType,
        node: &Node,
        comment: &str,
    ) -> Destination {
        let absolute_base_ptr_reg =
            self.allocate_frame_space_and_return_absolute_pointer_reg(ty, node, comment);
        let location = MemoryLocation {
            ty: absolute_base_ptr_reg.ty.clone(),
            base_ptr_reg: absolute_base_ptr_reg,
            offset: MemoryOffset(0),
        };

        Destination::new_location(location)
    }

    pub(crate) fn emit_constant_access(
        &mut self,
        target_reg: &TypedRegister,
        node: &Node,
        constant_reference: &ConstantRef,
        ctx: &Context,
    ) {
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

    pub(crate) fn emit_borrow_mutable_reference(
        &mut self,
        target_register: &TypedRegister,
        node: &Node,
        expr: &SingleLocationExpression,
        ctx: &Context,
    ) {
        let location = self.emit_lvalue_address(expr, ctx);

        let abs_pointer = self.emit_absolute_pointer_if_needed(
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
