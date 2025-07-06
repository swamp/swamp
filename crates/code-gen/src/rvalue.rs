/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{Expression, ExpressionKind};
use swamp_types::TypeKind;
use swamp_vm_types::types::{Destination, TypedRegister, VmType};

impl CodeBuilder<'_> {
    /// Make sure we have a pointer to something, no matter if it is a scalar or aggregate
    ///
    /// Mostly (only?) used for making sure we have a key value memory region for
    /// `Map` to calculate a hash for (and copy this memory for inserting)
    pub fn emit_aggregate_pointer_or_pointer_to_scalar_memory(
        &mut self,
        aggregate_or_scalar_expr: &Expression,
        ctx: &Context,
    ) -> TypedRegister {
        let gen_key_type = self.state.layout_cache.layout(&aggregate_or_scalar_expr.ty);
        if gen_key_type.is_aggregate() {
            self.emit_scalar_rvalue(aggregate_or_scalar_expr, ctx)
        } else {
            // for scalar values, we need to materialize in a temp memory
            let memory_location = self.allocate_frame_space_and_return_memory_location(
                &gen_key_type,
                &aggregate_or_scalar_expr.node,
                "temp space for scalar key",
            );
            self.emit_expression_into_target_memory(
                &memory_location,
                aggregate_or_scalar_expr,
                "temp space for scalar",
                ctx,
            );
            memory_location.pointer_location().unwrap().ptr_reg
        }
    }

    /// Emits code to evaluate an expression and return a pointer register.
    /// 
    /// This function handles both regular expressions that can be materialized as scalar rvalues
    /// and expressions that need temporary memory storage (like initializer lists).
    /// 
    /// For expressions that need memory materialization:
    /// - If allow_temporary is true: Allocates temporary frame space, initializes it, and returns pointer
    /// - If allow_temporary is false: Falls back to emit_scalar_rvalue (may fail for some expressions)
    /// 
    /// For regular expressions:
    /// - Uses emit_scalar_rvalue to get the pointer directly
    /// 
    /// # Parameters
    /// - `expr`: The expression to evaluate
    /// - `ctx`: Code generation context
    /// - `allow_temporary`: Whether temporary storage is acceptable (false for mutable iteration)
    pub fn emit_scalar_rvalue_or_pointer_to_temporary(
        &mut self,
        expr: &Expression,
        ctx: &Context,
        allow_temporary: bool,
    ) -> TypedRegister {
        if allow_temporary && Self::rvalue_needs_memory_location_to_materialize_in(&mut self.state.layout_cache, expr) {
            // Expression needs temporary storage (like initializer lists)
            let expr_basic_type = self.state.layout_cache.layout(&expr.ty);
            let temp_memory = self.allocate_frame_space_and_return_destination_to_it(
                &expr_basic_type,
                &expr.node,
                "temporary storage for expression that needs memory materialization"
            );
            
            // Initialize the temporary memory for collections (vectors, etc.)
            if let Destination::Memory(ref memory_location) = temp_memory {
                self.emit_initialize_memory_for_any_type(
                    memory_location,
                    &expr.node,
                    "initialize temporary storage for expression"
                );
            }
            
            // Materialize the expression into the temporary memory
            self.emit_expression(&temp_memory, expr, ctx);
            
            // Return the pointer to the temporary memory
            self.emit_compute_effective_address_to_register(
                &temp_memory,
                &expr.node,
                "get pointer to temporary storage"
            )
        } else {
            // Regular case: expression can be materialized as scalar rvalue
            // Note: This may fail for expressions that need memory materialization
            // when allow_temporary is false, but that's intentional behavior
            self.emit_scalar_rvalue(expr, ctx)
        }
    }

    pub fn emit_bool_expression(
        &mut self,
        target_reg: &Destination,
        expr: &Expression,
        ctx: &Context,
    ) {
        debug_assert!(
            matches!(&*expr.ty.kind, TypeKind::Bool),
            "must have scalar type"
        );
        self.emit_expression(target_reg, expr, ctx);
    }

    pub fn emit_bool_value(&mut self, expr: &Expression, ctx: &Context) -> TypedRegister {
        debug_assert!(
            matches!(&*expr.ty.kind, TypeKind::Bool),
            "must have scalar type"
        );
        self.emit_scalar_rvalue(expr, ctx)
    }

    /// Emits code to evaluate an expression into a scalar rvalue.
    ///
    /// In compiler terminology:
    /// - "emit" means to generate the machine code
    /// - "scalar" refers to single-value types (numbers, booleans, pointers) as opposed to aggregates
    /// - "rvalue" is a value-producing expression that can appear on the right side of an assignment
    ///
    /// This method provides an optimized path for scalar values, avoiding the complexity
    /// of aggregate type handling in the general `emit_expression`. It's particularly
    /// important for efficient code generation of expressions that must produce values
    /// (rvalues) rather than storage locations (lvalues).
    ///
    /// Basically, for aggregate types, it shouldn't to materialize (copy) the aggregate value
    /// to a destination, but instead just provide a register pointing to the source location.
    ///
    /// # Direct Register Access
    ///
    /// The following cases can return a register without materialization:
    ///
    /// - Variable access (returns the existing register)
    /// - Constants (the memory location can be materialized into a register)
    /// - Some scalar literals (can be materialized directly into a register)
    ///
    /// # Register Allocation
    ///
    /// For other expressions that can't provide a direct register,
    /// a temporary register is allocated and the expression is evaluated into it.
    ///
    /// # Examples in Compiler Terms
    ///
    /// ```ignore
    /// // Binary operations need rvalues for both operands
    /// let left_reg = emit_scalar_rvalue(&binary_op.left);   // rvalue needed
    /// let right_reg = emit_scalar_rvalue(&binary_op.right); // rvalue needed
    ///
    /// // Assignment needs an lvalue on left, rvalue on right
    /// let target = emit_lvalue(&assign.left);        // lvalue needed
    /// let value = emit_scalar_rvalue(&assign.right); // rvalue needed
    /// ```
    pub fn emit_scalar_rvalue(&mut self, expr: &Expression, ctx: &Context) -> TypedRegister {
        match &expr.kind {
            ExpressionKind::VariableAccess(variable_ref) => {
                return self.get_variable_register(variable_ref).clone();
            }
            ExpressionKind::ConstantAccess(constant_ref) => {
                let constant_type = self.state.layout_cache.layout(&constant_ref.resolved_type);
                if constant_type.is_aggregate() {
                    let temp_target_reg = self.temp_registers.allocate(
                        VmType::new_unknown_placement(constant_type),
                        "temporary for constant access",
                    );
                    let constant_gen = self.state.constant_offsets.get(&constant_ref.id).unwrap();
                    self.builder.add_mov_32_immediate_value(
                        temp_target_reg.register(),
                        constant_gen.addr().0,
                        &expr.node,
                        "load in the constant pointer",
                    );
                    return temp_target_reg.register;
                }
            }
            _ => {}
        }
        {
            let ty = self.state.layout_cache.layout(&expr.ty);
            let temp_target_reg = self.temp_registers.allocate(
                VmType::new_unknown_placement(ty),
                "to produce a rvalue, we have to allocate a temporary variable",
            );

            self.emit_expression_into_register(
                temp_target_reg.register(),
                expr,
                "emit_rvalue",
                ctx,
            );

            temp_target_reg.register
        }
    }
}
