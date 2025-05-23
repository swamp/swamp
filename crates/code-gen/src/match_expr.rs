use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{Match, NormalPattern, Pattern};
use swamp_vm_types::MemoryOffset;
use swamp_vm_types::types::{Destination, VmType, u8_type};

impl CodeBuilder<'_> {
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
            enum_ptr_reg.grab_rvalue(),
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
}
