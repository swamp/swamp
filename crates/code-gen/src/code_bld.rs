use crate::alloc::ScopeAllocator;
use crate::ctx::Context;
use crate::layout::{layout_optional_type, layout_type};
use crate::reg_pool::{HwmTempRegisterPool, RegisterPool, TempRegister};
use crate::state::CodeGenState;
use crate::{
    Collection, DetailedLocationResolved, FlagState, FlagStateKind, Transformer, TransformerResult,
    single_intrinsic_fn,
};
use seq_map::SeqMap;
use source_map_cache::{SourceMapLookup, SourceMapWrapper};
use source_map_node::Node;
use swamp_semantic::{
    BooleanExpression, ConstantRef, Expression, ExpressionKind, ExternalFunctionDefinitionRef,
    Function, Guard, Match, MutRefOrImmutableExpression, NormalPattern, Pattern, Postfix,
    PostfixKind, SingleLocationExpression, StartOfChain, StartOfChainKind, UnaryOperator,
    UnaryOperatorKind, VariableRef, WhenBinding,
};
use swamp_types::Type;
use swamp_vm_instr_build::{InstructionBuilder, PatchPosition};
use swamp_vm_types::aligner::{SAFE_ALIGNMENT, align};
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, Destination, FramePlacedType, TypedRegister, VmType, b8_type,
    int_type, string_type, u8_type, u32_type, unknown_type,
};
use swamp_vm_types::{
    AggregateMemoryLocation, FrameMemoryAddress, FrameMemoryRegion, FrameMemorySize,
    HeapMemoryAddress, InstructionPosition, MemoryLocation, MemoryOffset, REG_ON_FRAME_ALIGNMENT,
    REG_ON_FRAME_SIZE, StringHeader, VEC_PTR_SIZE,
};
use tracing::{error, info};

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
    frame_allocator: ScopeAllocator,
    pub source_map_lookup: &'a SourceMapWrapper<'a>,
}

impl CodeBuilder<'_> {}

impl<'a> CodeBuilder<'a> {
    pub const fn new(
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
        start_address: FrameMemoryAddress,
        count: u8,
        node: &Node,
        comment: &str,
    ) {
        self.builder
            .add_ld_regs_from_frame(start_reg, start_address, count, node, comment);
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

    fn emit_load_from_location(
        &mut self,
        target_reg: &TypedRegister,
        location: &Destination,
        node: &Node,
        comment: &str,
    ) {
        match location {
            Destination::Register(reg) => {
                self.emit_copy_register(target_reg, reg, node, comment);
            }
            Destination::Memory(memory_location) => {
                self.emit_load_from_memory(
                    target_reg,
                    &memory_location.base_ptr_reg,
                    memory_location.offset,
                    &memory_location.ty,
                    node,
                    "load from location",
                );
            }
            Destination::Unit => {
                panic!("not sure")
            }
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
            &format!("{comment} (temp register for offset)"),
        );
        self.builder.add_mov_32_immediate_value(
            offset_temp_reg.register(),
            u32::from(offset.0),
            node,
            &format!("{comment} (set offset value to get from base to register)"),
        );
        self.builder.add_add_u32(
            target_reg,
            base_ptr_reg,
            offset_temp_reg.register(),
            node,
            &format!("{comment} (add offset to base ptr reg to get new base ptr)"),
        );

        self.temp_registers.restore_to_mark(hwm);
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
            UnaryOperatorKind::Not => match &unary_operator.left.ty {
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

    pub(crate) fn emit_absolute_pointer(
        &mut self,
        target_reg: &TypedRegister,
        argument: &SingleLocationExpression,
        ctx: &Context,
        comment: &str,
    ) {
        let region = self.emit_lvalue_address(argument, ctx);
        match region {
            Destination::Register(reg) => {
                self.builder.add_mov_reg(
                    target_reg,
                    &reg,
                    &argument.node,
                    &format!("copy reg that has pointer {target_reg:?} <- {reg:?} {comment}"),
                );
            }
            Destination::Memory(memory_location) => {
                let hwm = self.temp_registers.save_mark();

                let temp_offset_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u32_type()),
                    "emit_absolute_pointer: temp_offset_reg",
                );
                self.builder.add_mov_32_immediate_value(
                    temp_offset_reg.register(),
                    u32::from(memory_location.offset.0),
                    &argument.node,
                    "set offset in temp register",
                );
                self.builder.add_add_u32(
                    target_reg,
                    &memory_location.base_ptr_reg,
                    temp_offset_reg.register(),
                    &argument.node,
                    "forcing lvalue to be a complete pointer",
                );

                self.temp_registers.restore_to_mark(hwm);
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
    pub(crate) fn load_register_contents_from_memory(
        &mut self,
        node: &Node,
        target_reg: &TypedRegister,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        comment: &str,
    ) {
        let underlying_type = target_reg.underlying();
        if underlying_type.is_represented_as_a_pointer_in_reg() {
            self.builder.add_ld32_from_pointer_with_offset_u16(
                target_reg,
                base_ptr_reg,
                offset,
                node,
                comment,
            );
            return;
        }

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
                    u32::from(offset.0),
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

    pub(crate) fn temp_frame_space_for_register(&mut self, comment: &str) -> FrameMemoryRegion {
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
    pub(crate) fn emit_postfix_chain(
        &mut self,
        output_destination: &Destination,
        start_expression: &StartOfChain,
        chain: &[Postfix],
        ctx: &Context,
    ) {
        let mut current_location = self.emit_start_of_chain(start_expression, ctx);
        let mut t_flag_result = FlagState::default();

        //info!(t=?current_location.vm_type(), "start r value chain");

        for (index, element) in chain.iter().enumerate() {
            //info!(t=?element.ty, index,t=?current_location.vm_type(), ?element.kind, "chain element");
            let is_last = index == chain.len() - 1;
            match &element.kind {
                PostfixKind::StructField(anonymous_struct, field_index) => {
                    debug_assert!(current_location.is_memory_location());
                    let struct_layout =
                        layout_type(&Type::AnonymousStruct(anonymous_struct.clone()));
                    let offset_item = struct_layout.get_field_offset(*field_index).unwrap();

                    current_location = current_location.add_offset(
                        offset_item.offset,
                        VmType::new_unknown_placement(offset_item.ty.clone()),
                    );
                    //info!(?current_location, "after field offset lookup");
                }
                PostfixKind::SliceSubscript(slice_type, int_expression) => {
                    let element_basic_type = layout_type(&slice_type.element);

                    todo!()
                    /*
                    current_location = self.subscript_helper_from_location_to_location(
                        current_location,
                        &element_basic_type,
                        int_expression,
                        BoundsCheck::KnownSizeAtCompileTime(slice_type.fixed_size as u16),
                        &int_expression.node,
                        "emit rvalue",
                        ctx,
                    );

                     */
                }

                PostfixKind::VecSubscript(vec_type, int_expression) => {
                    current_location = self.vec_subscript_helper(
                        &current_location,
                        &vec_type.element,
                        int_expression,
                        ctx,
                    );
                }

                PostfixKind::MapSubscript(map_type, key_expression) => {
                    current_location = self.map_subscript_helper(
                        &current_location,
                        &map_type.key,
                        key_expression,
                        ctx,
                    );
                }

                PostfixKind::MemberCall(function_to_call, arguments) => {
                    let hwm = self.temp_registers.save_mark();
                    //let return_temp_reg =
                    //  self.temp_space_for_type(&function_to_call.signature().return_type, "");
                    let resolved_location = self
                        .emit_load_primitive_from_detailed_location_if_needed(
                            &current_location,
                            &element.node,
                            "emit_rvalue_postfix member call ",
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
                                    output_destination, // TODO: Intrinsic calls can only set to register?
                                    &start_expression.node,
                                    intrinsic_fn,
                                    Some(element.ty.clone()),
                                    Some(resolved_location.register()),
                                    &merged_arguments,
                                    ctx,
                                    "rvalue intrinsic call ",
                                );

                                if is_last {
                                    t_flag_result = z_result;
                                }
                            } else {
                                let (spilled_argument_registers, copy_back) = self.emit_arguments(
                                    output_destination,
                                    &start_expression.node,
                                    &internal_fn.signature.signature,
                                    Some(resolved_location.register()),
                                    arguments,
                                    ctx,
                                );
                                self.emit_call(&element.node, internal_fn, "emit_rvalue call");

                                self.emit_post_call(
                                    &spilled_argument_registers,
                                    &copy_back,
                                    &element.node,
                                    "emit_rvalue postcall",
                                );
                            }
                        }
                        Function::External(external_function_def) => {
                            self.emit_host_self_call(
                                output_destination,
                                &start_expression.node,
                                external_function_def,
                                resolved_location.register(),
                                arguments,
                                ctx,
                            );
                        }
                        Function::Intrinsic(intrinsic_def) => {
                            let z_result = self.emit_single_intrinsic_call_with_self(
                                output_destination,
                                &start_expression.node,
                                &intrinsic_def.intrinsic,
                                Some(element.ty.clone()),
                                Some(resolved_location.register()),
                                arguments,
                                ctx,
                                "rvalue intrinsic call ",
                            );

                            if is_last {
                                t_flag_result = z_result;
                            }
                        }
                        _ => panic!(
                            "{}",
                            &format!("not supported as a member call {function_to_call:?}")
                        ),
                    }

                    self.temp_registers.restore_to_mark(hwm);

                    current_location =
                        Destination::Register(output_destination.grab_register().clone());
                    //info!(?current_location, "after member call");
                }
                PostfixKind::OptionalChainingOperator => {
                    todo!()
                }
                PostfixKind::NoneCoalescingOperator(expression) => {
                    let hwm = self.temp_registers.save_mark();
                    let temp_reg = self.temp_register_for_analyzed_type(&expression.ty, "");

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

                    self.emit_expression(output_destination, expression, ctx);

                    self.temp_registers.restore_to_mark(hwm);

                    self.builder.patch_jump_here(patch);

                    current_location =
                        Destination::Register(output_destination.grab_register().clone());
                    info!(?current_location, "after none coalesce");
                }
            }

            //info!(t=?element.ty, index, t=?current_location.vm_type(), ?element.kind, "after element");
        }

        self.emit_load_from_location(
            output_destination.grab_register(),
            &current_location,
            &start_expression.node,
            "rvalue postfix chain",
        );
    }

    pub(crate) fn emit_string_literal(
        &mut self,
        destination: &Destination,
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
        let mut header_bytes = [0u8; 8];
        header_bytes[0..4].copy_from_slice(&string_header.heap_offset.to_le_bytes());
        header_bytes[4..8].copy_from_slice(&string_header.byte_count.to_le_bytes());

        let string_header_in_heap_ptr = HeapMemoryAddress(
            self.state
                .constants_manager
                .allocate_byte_array(&header_bytes)
                .addr()
                .0,
        );

        match destination {
            Destination::Unit => {
                panic!("can not write string to unit")
            }
            Destination::Register(target_register) => {
                self.builder.add_mov_32_immediate_value(
                    target_register,
                    string_header_in_heap_ptr.0,
                    node,
                    &format!("constant string '{string}'"),
                );
            }
            Destination::Memory(memory_location) => {
                let temp_string_literal_reg = self.temp_registers.allocate(
                    VmType::new_contained_in_register(string_type()),
                    "temporary for string literal",
                );
                self.builder.add_mov_32_immediate_value(
                    temp_string_literal_reg.register(),
                    string_header_in_heap_ptr.0,
                    node,
                    "string literal",
                );
                self.builder.add_st32_using_ptr_with_offset(
                    memory_location,
                    temp_string_literal_reg.register(),
                    node,
                    "copy string pointer literal into destination memory",
                );
            }
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
                    "load the tag Some (1)",
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
                ); // Fills in more of the union
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
    ) -> TypedRegister {
        match &mut_or_immutable_expression {
            MutRefOrImmutableExpression::Expression(found_expression) => {
                self.emit_scalar_rvalue(found_expression, ctx)
            }
            MutRefOrImmutableExpression::Location(location_expression) => {
                let x = self.emit_lvalue_address(location_expression, ctx);
                // TODO: FIX THIS
                if let Destination::Register(reg) = x {
                    reg
                } else if let Destination::Memory(memory_location) = x {
                    error!("expected register");
                    memory_location.base_ptr_reg
                } else {
                    panic!("not sure");
                }
            }
        }
    }

    pub fn allocate_frame_space_and_assign_register(
        &mut self,
        ty: &BasicType,
        node: &Node,
        comment: &str,
    ) -> Destination {
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

        let location = MemoryLocation {
            ty: reg.ty.clone(),
            base_ptr_reg: reg,
            offset: MemoryOffset(0),
        };

        Destination::new_location(location)
    }

    pub(crate) fn emit_match(
        &mut self,
        output_destination: &Destination,
        match_expr: &Match,
        ctx: &Context,
    ) {
        let enum_ptr_reg = self.emit_for_access_or_location(&match_expr.expression, ctx);

        let mut jump_to_exit_placeholders = Vec::new();

        let arm_len_to_consider = if match_expr.contains_wildcard() {
            match_expr.arms.len()
        } else {
            match_expr.arms.len()
        };

        let enum_tag_temp_reg = self.temp_registers.allocate(
            VmType::new_contained_in_register(u8_type()),
            "temp reg for enum tag",
        ); // TODO: support different tag sizes

        self.builder.add_ld8_from_pointer_with_offset_u16(
            enum_tag_temp_reg.register(),
            &enum_ptr_reg,
            MemoryOffset(0), // TODO: take offset from tag union info
            match_expr.expression.node(),
            "read enum tag",
        );

        for (index, arm) in match_expr.arms.iter().enumerate() {
            let is_last = index == arm_len_to_consider - 1;

            //  Each arm must set the CPU zero flag
            let maybe_guard = match &arm.pattern {
                Pattern::Normal(normal_pattern, maybe_guard) => match normal_pattern {
                    NormalPattern::PatternList(_) => None,
                    NormalPattern::EnumPattern(enum_variant, maybe_patterns) => {
                        self.builder.add_eq_u8_immediate(
                            enum_tag_temp_reg.register(),
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

            let maybe_guard_skip = maybe_guard.map(|guard| self.emit_condition_context(guard, ctx));

            self.emit_expression(output_destination, &arm.expression, ctx);

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
    }

    pub(crate) fn emit_guard(
        &mut self,
        output_destination: &Destination,
        guards: &Vec<Guard>,
        ctx: &Context,
    ) {
        let mut jump_to_exit_placeholders = Vec::new();
        for guard in guards {
            if let Some(condition) = &guard.condition {
                //                let result = self.emit_boolean_expression_t_flag(condition)?;
                let skip_expression_patch = self.emit_condition_context(condition, ctx);
                //&result.polarity(),
                //&guard.result.node,
                //"guard condition",
                //);
                self.emit_expression(output_destination, &guard.result, ctx);
                let jump_to_exit_placeholder = self.builder.add_jump_placeholder(
                    &guard.result.debug_last_expression().node,
                    "jump to exit",
                );
                jump_to_exit_placeholders.push(jump_to_exit_placeholder);
                self.builder.patch_jump_here(skip_expression_patch);
            } else {
                // _ -> wildcard
                self.emit_expression(output_destination, &guard.result, ctx);
            }
        }

        for placeholder in jump_to_exit_placeholders {
            self.builder.patch_jump_here(placeholder);
        }
    }

    pub(crate) fn emit_when(
        &mut self,
        target_reg: &Destination,
        bindings: &Vec<WhenBinding>,
        true_expr: &Expression,
        maybe_false_expr: Option<&Expression>,
        ctx: &Context,
    ) {
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
                self.emit_mut_or_immute(&placed_variable, &binding.expr, ctx);
            } else {
                let MutRefOrImmutableExpression::Expression(variable_access_expression) =
                    &binding.expr
                else {
                    panic!("must be expression");
                };
                let old_variable_region = self.emit_scalar_rvalue(variable_access_expression, ctx);

                let tagged_union_binding = old_variable_region.ty.underlying();
                let tagged_union = tagged_union_binding.optional_info().unwrap();

                let memory_location = MemoryLocation {
                    ty: placed_variable.ty.clone(),
                    base_ptr_reg: placed_variable,
                    offset: MemoryOffset(0),
                };

                self.builder.add_block_copy_with_offset(
                    &memory_location,
                    &old_variable_region,
                    tagged_union.payload_offset,
                    tagged_union.tag_size,
                    binding.expr.node(),
                    "copy in the payload. Unwrap.",
                );
            }
        }

        self.emit_expression(target_reg, true_expr, ctx);
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
            self.emit_expression(target_reg, else_expr, ctx);
            self.builder.patch_jump_here(maybe_jump_over_false.unwrap());
        }
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

    fn emit_start_of_chain(&mut self, start: &StartOfChain, ctx: &Context) -> Destination {
        match &start.kind {
            StartOfChainKind::Expression(expr) => {
                Destination::Register(self.emit_scalar_rvalue(expr, ctx))
            }
            StartOfChainKind::Variable(variable) => {
                let variable_reg = self.get_variable_register(variable);
                Destination::Register(variable_reg.clone())
            }
        }
    }

    pub(crate) fn emit_host_call(
        &mut self,
        output_destination: &Destination,
        node: &Node,
        host_fn: &ExternalFunctionDefinitionRef,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> FlagState {
        let (spilled_arguments, copy_back) = self.emit_arguments(
            output_destination,
            node,
            &host_fn.signature,
            None,
            arguments,
            ctx,
        );

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

        FlagState::default()
    }

    fn emit_host_self_call(
        &mut self,
        return_output_destination: &Destination,
        node: &Node,
        host_fn: &ExternalFunctionDefinitionRef,
        self_frame_placed_type: &TypedRegister,
        arguments: &Vec<MutRefOrImmutableExpression>,
        ctx: &Context,
    ) -> FlagState {
        let (spilled_arguments, copy_backs) = self.emit_arguments(
            return_output_destination,
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

        FlagState::default()
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
    /// Generates code to iterate over a collection using a transformer (e.g., map, filter, `filter_map`)
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
        let lambda_result = self.emit_scalar_rvalue(lambda_expr, ctx);

        // 4. If the transformer supports early exit, set the Z flag based on the lambda result.
        let transformer_t_flag_state =
            self.check_if_transformer_sets_t_flag(transformer, &lambda_result, node);

        // 5. Conditionally skip result insertion if early exit is triggered.
        let maybe_skip_early = if matches!(
            transformer_t_flag_state,
            FlagStateKind::TFlagIsTrueWhenSet | FlagStateKind::TFlagIsTrueWhenClear
        ) {
            // The z flag is set so we can act on it
            let skip_early = self.builder.add_jmp_if_not_equal_polarity_placeholder(
                &transformer_t_flag_state.polarity(),
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

                self.builder
                    .add_mov8_immediate(tag_target.register(), 1, node, "mark tag as Some");
                /* TODO:

                self.builder.add_st8_using_ptr_with_offset(
                    primary_variable,
                    tagged_union.tag_offset,
                    tag_target.register(),
                    node,
                    "copy Tag value of (1)",
                );

                 */

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

    fn check_if_transformer_sets_t_flag(
        &mut self,
        transformer: Transformer,
        in_value: &TypedRegister,
        node: &Node,
    ) -> FlagStateKind {
        match transformer {
            Transformer::For => FlagStateKind::TFlagIsIndeterminate,
            Transformer::Filter => {
                // TODO: Bring this back //assert_eq!(in_value.size().0, 1); // bool
                self.builder
                    .add_tst_u8(in_value, node, "filter bool to z flag");
                FlagStateKind::TFlagIsTrueWhenSet
            }
            Transformer::Find => {
                // TODO: Bring this back //assert_eq!(in_value.size().0, 1); // bool
                self.builder
                    .add_tst_u8(in_value, node, "find: bool to z flag");
                FlagStateKind::TFlagIsTrueWhenClear
            }
            Transformer::Map => FlagStateKind::TFlagIsIndeterminate,
            Transformer::Any => {
                self.builder.add_tst_u8(in_value, node, "any, check tag");
                FlagStateKind::TFlagIsTrueWhenClear
            }
            Transformer::All => {
                self.builder.add_tst_u8(in_value, node, "all, check tag");
                FlagStateKind::TFlagIsTrueWhenSet
            }
            Transformer::FilterMap => {
                self.builder
                    .add_tst_u8(in_value, node, "filter map, check tag");
                FlagStateKind::TFlagIsTrueWhenSet
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
                self.builder.add_vec_push_addr(
                    mut_collection,
                    value,
                    value.ty.basic_type.total_size,
                    node,
                    "push",
                );
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

    pub(crate) fn emit_borrow_mutable_reference(
        &mut self,
        target_register: &TypedRegister,
        node: &Node,
        expr: &Expression,
        ctx: &Context,
    ) {
        let inner = self.emit_scalar_rvalue(expr, ctx);

        if !inner.ty().is_mutable_reference() {
            self.builder.add_lea(
                target_register,
                inner.addr(),
                node,
                "wasn't a pointer, so converting",
            );
        }
    }

    /// Emits code to resolve an expression of a simple (primitive) type to its value,
    /// placing that value into a register.
    ///
    /// This function is typically used to prepare operands for simple arithmetic, logical,
    /// or comparison operations, or for passing simple-type arguments by value.
    ///
    /// For simple rvalues, we do not need a target to materialize (write) into, we
    /// only need the register to use in, for example, in the operands for an operator.
    /// it can allocate a temporary register if needed, but not allocate any memory.
    /// the returned register always contains a simple (primitive) value.

    /// Emits code to evaluate an expression of a complex type (e.g., struct, vec)
    /// and materializes (constructs or copies) its instance directly into a pre-allocated
    /// memory location specified by a target pointer `target_ptr_reg`.
    ///
    /// For complex rvalues, the rvalue need a pointer to write the expression into. The function
    /// never reserves or allocates memory for the target, the target memory is already allocated.
    /// After it returns, the expression has been written (materialized) into the `target_ptr_reg`.
    ///
    /// # Example Usage Context
    /// 1. Assignment: `complex_lvalue = complex_literal_expr;`
    /// ```ignore
    /// let reg_addr_lhs = self.emit_lvalue(complex_lvalue.expression_node(), ctx);
    /// self.emit_complex_rvalue(reg_addr_lhs, complex_literal_expr, ctx);
    /// ```
    /// 2. Materializing an `RValue` argument: `foo(StructLiteral{...})`
    /// ```ignore
    /// let reg_temp_stack_addr = alloc_for_type(complex_literal_type);
    /// self.emit_complex_rvalue(reg_temp_stack_addr, StructLiteral_expr, ctx);
    /// // use reg_temp_stack_addr to the function
    /// ```

    pub fn temp_register_for_analyzed_type(&mut self, ty: &Type, comment: &str) -> TempRegister {
        let layout = layout_type(ty);

        self.temp_registers.allocate(
            VmType::new_unknown_placement(layout),
            &format!("temp space {comment}"),
        )
    }
}
