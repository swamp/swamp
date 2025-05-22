use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{Expression, MutRefOrImmutableExpression, WhenBinding};
use swamp_vm_types::types::Destination;
use swamp_vm_types::{MemoryLocation, MemoryOffset};

impl CodeBuilder<'_> {
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
}
