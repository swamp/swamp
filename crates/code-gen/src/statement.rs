use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::{layout_tuple_items, layout_type};
use crate::{Collection, DetailedLocation, Transformer};
use source_map_node::Node;
use swamp_semantic::{
    BooleanExpression, CompoundOperatorKind, Expression, ExpressionKind, ForPattern, Iterable,
    MutRefOrImmutableExpression, TargetAssignmentLocation, VariableRef,
};
use swamp_types::Type;
use swamp_vm_types::types::{OutputDestination, TypedRegister, VmType, unit_type};
use swamp_vm_types::{MemoryLocation, MemoryOffset};
use tracing::info;

impl CodeBuilder<'_> {
    pub fn emit_statement(&mut self, expr: &Expression, ctx: &Context) {
        debug_assert!(matches!(expr.ty, Type::Unit));
        let output_destination = OutputDestination::new_unit();
        self.emit_expression(&output_destination, expr, ctx);
    }
    pub(crate) fn emit_variable_definition(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) {
        self.emit_variable_assignment(variable, expression, ctx)
    }

    fn emit_variable_assignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) {
        let target_register = self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| {
                panic!(
                    "could not find id {} {}",
                    variable.unique_id_within_function, variable.assigned_name
                )
            })
            .clone();

        if variable.resolved_type.is_primitive() {
            self.emit_expression_into_register(
                &target_register,
                expression,
                "variable primitive",
                ctx,
            );
        } else {
            let memory_location = MemoryLocation {
                base_ptr_reg: target_register.clone(),
                offset: MemoryOffset(0),
                ty: target_register.ty.underlying().clone(),
            };
            self.emit_expression_into_target_memory(
                &memory_location,
                expression,
                "variable assignment",
                ctx,
            );
        }
    }

    pub(crate) fn emit_variable_reassignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) {
        self.emit_variable_assignment(variable, expression, ctx)
    }
    pub(crate) fn emit_assignment(
        &mut self,
        lhs: &TargetAssignmentLocation,
        rhs: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        let assignment_target = self.emit_lvalue_location(&lhs.0, ctx);
        let memory_location = match &assignment_target {
            DetailedLocation::Register { reg } => MemoryLocation {
                base_ptr_reg: reg.clone(),
                offset: MemoryOffset(0),
                ty: assignment_target.vm_type().basic_type.clone(),
            },
            DetailedLocation::Memory {
                base_ptr_reg,
                offset,
                ty,
            } => MemoryLocation {
                base_ptr_reg: base_ptr_reg.clone(),
                offset: offset.clone(),
                ty: ty.underlying().clone(),
            },
        };
        self.emit_expression_into_target_memory(&memory_location, rhs, comment, ctx);
        /*
        match &assignment_target {
            DetailedLocation::Register { reg } => {
                let target_location_type = layout_type(&lhs.0.ty);

                self.emit_assignment_like(&reg, &target_location_type, rhs, ctx);
            }
            DetailedLocation::Memory {
                base_ptr_reg,
                offset,
                ..
            } => {
                if rhs.ty.is_scalar() {
                    let rhs_reg = self.emit_scalar_rvalue(rhs, &ctx);
                    self.store_register_contents_to_memory(
                        node,
                        &base_ptr_reg,
                        *offset,
                        &rhs_reg,
                        "assignment",
                    );
                } else {
                    let absolute_ptr_reg = self.emit_ptr_reg_from_detailed_location(
                        &assignment_target,
                        node,
                        "get absolute pointer for complex rvalue materialize",
                    );
                    self.emit_complex_rvalue(&absolute_ptr_reg, rhs, ctx);
                }
            }
        }

         */
    }

    pub(crate) fn emit_tuple_destructuring(
        &mut self,
        target_variables: &[VariableRef],
        tuple_type: &[Type],
        source_tuple_expression: &Expression,
        context: &Context,
    ) {
        let node = &source_tuple_expression.node;
        let tuple_base_pointer_reg = self.emit_scalar_rvalue(source_tuple_expression, context);

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
    }

    pub(crate) fn emit_for_loop(
        &mut self,
        node: &Node,
        for_pattern: &ForPattern,
        iterable: &Iterable,
        lambda_non_capturing_expr: &Box<Expression>,
        ctx: &Context,
    ) {
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

    pub(crate) fn emit_while_loop(
        &mut self,
        condition: &BooleanExpression,
        expression: &Expression,
        ctx: &Context,
    ) {
        // `while` loops are only for side effects, make sure that the target size is zero (Unit)
        //assert_eq!(target_reg.size.0, 0);

        let ip_for_condition = self.builder.position();

        let jump_on_false_condition = self.emit_condition_context(condition, ctx);

        // Expression is only for side effects
        self.emit_statement(expression, ctx);

        // Always jump to the condition again to see if it is true
        self.builder
            .add_jmp(ip_for_condition, &expression.node, "jmp to while condition");

        self.builder.patch_jump_here(jump_on_false_condition);
    }
    pub(crate) fn emit_compound_assignment(
        &mut self,
        target_location: &TargetAssignmentLocation,
        op: &CompoundOperatorKind,
        source: &Expression,
        ctx: &Context,
    ) {
        let assignment_target = self.emit_lvalue_location(&target_location.0, ctx);

        let hwm = self.temp_registers.save_mark();

        let resolved = self.emit_load_primitive_from_detailed_location_if_needed(
            &assignment_target,
            &target_location.0.node,
            "compound_assignment",
        );

        let source_info = self.emit_scalar_rvalue(source, ctx);

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

        /*
        TODO:
        self.emit_store_primitive_from_detailed_location_if_needed(
            &resolved,
            &assignment_target,
            &target_location.0.node,
        );

         */

        /*
        if let DetailedLocationResolved::TempRegister(temp_reg) = resolved {
            self.temp_registers.free(temp_reg);
        }

         */
        self.temp_registers.restore_to_mark(hwm);
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
}
