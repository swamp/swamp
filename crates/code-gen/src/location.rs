/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::Expression;
use swamp_vm_types::types::{BasicTypeKind, TypedRegister, VmType};
use swamp_vm_types::{AggregateMemoryLocation, MemoryLocation, MemoryOffset};

impl CodeBuilder<'_> {
    /// Emits code to evaluate an expression and return a memory location for the aggregate.
    ///
    /// This is specifically used when we need to access the internals of a container type
    /// (like optionals, structs, enums) rather than just getting the value itself.
    ///
    /// For example, in `when` expressions with optionals, we need a memory location for the
    /// optional container so we can:
    /// - Load the tag byte to check Some/None
    /// - Access the payload at a specific offset
    pub(crate) fn emit_expression_as_aggregate_pointer(
        &mut self,
        expression: &Expression,
        context: &Context,
    ) -> AggregateMemoryLocation {
        let base_reg = self.emit_scalar_rvalue(expression, context);
        let memory_location = MemoryLocation {
            base_ptr_reg: base_reg,
            offset: MemoryOffset(0),
            ty: VmType::new_unknown_placement(self.state.layout_cache.layout(&expression.ty)),
        };
        AggregateMemoryLocation::new(memory_location)
    }
}
