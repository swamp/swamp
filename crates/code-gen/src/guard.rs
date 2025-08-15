/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::Guard;
use swamp_vm_types::types::Place;

impl CodeBuilder<'_> {
    pub(crate) fn emit_guard(
        &mut self,
        output_destination: &Place,
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
}
