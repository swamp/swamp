/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::FlagState;
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{ArgumentExpression, ExternalFunctionDefinitionRef};
use swamp_vm_types::types::{Destination, TypedRegister};

impl CodeBuilder<'_> {
    pub(crate) fn emit_host_call(
        &mut self,
        output_destination: &Destination,
        node: &Node,
        host_fn: &ExternalFunctionDefinitionRef,
        arguments: &[ArgumentExpression],
        ctx: &Context,
    ) -> FlagState {
        // Then set up the arguments
        let argument_infos = self.emit_arguments(
            output_destination,
            node,
            &host_fn.signature,
            None,
            arguments,
            true,
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

        self.emit_post_call(argument_infos, node, "host call");

        FlagState::default()
    }

    pub(crate) fn emit_host_self_call(
        &mut self,
        return_output_destination: &Destination,
        node: &Node,
        host_fn: &ExternalFunctionDefinitionRef,
        self_frame_placed_type: &TypedRegister,
        arguments: &[ArgumentExpression],
        ctx: &Context,
    ) -> FlagState {
        let argument_infos = self.emit_arguments(
            return_output_destination,
            node,
            &host_fn.signature,
            Some(self_frame_placed_type),
            arguments,
            true,
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

        self.emit_post_call(argument_infos, node, "host_self_call");

        FlagState::default()
    }
}
