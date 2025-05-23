use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use swamp_semantic::{LocationAccessKind, SingleLocationExpression};
use swamp_vm_types::types::{Destination, VmType};

impl CodeBuilder<'_> {
    /// Emits code to compute the memory address (lvalue) of a variable access chain.
    ///
    /// In compiler terminology:
    ///
    /// - "lvalue" represents a storage location that can appear on the left side of an assignment
    /// - "address" refers to the actual memory position where a value is stored
    /// - "chain" is a sequence of accesses (e.g., `foo.bar[i].baz`)
    ///
    /// This method performs address arithmetic to compute the final memory address through:
    ///
    /// - Base address in registers
    /// - Offset calculations for struct fields
    /// - Index calculations for arrays/vectors
    /// - Pointer manipulations for collections
    ///
    /// # Address Resolution Process
    ///
    /// The method traverses the access chain, computing new addresses by:
    ///
    /// 1. Starting from a base variable's address in a register
    ///
    /// 2. For each access in the chain:
    ///    - For fields: Add field offset to base address
    ///    - For indexing: Scale index by element size and add to base
    ///    - For collections: Use intrinsic access methods to get element address
    ///
    /// The computed address is always represented as either:
    /// - A register containing a direct memory address
    /// - A (`base_register`, offset) pair for more complex addressing
    ///
    /// # Examples in Compiler Terms
    ///
    /// ```ignore
    /// let addr = emit_lvalue_address(&expr);
    /// emit_expression(addr, value);
    /// ```
    ///
    /// This address computation is crucial for:
    ///
    /// - Assignment targets
    /// - Reference operations
    /// - Pointer arithmetic
    /// - Memory access optimization
    #[allow(clippy::too_many_lines)]
    pub(crate) fn emit_lvalue_address(
        &mut self,
        location_expression: &SingleLocationExpression,
        ctx: &Context,
    ) -> Destination {
        let start_reg = self
            .variable_registers
            .get(
                &location_expression
                    .starting_variable
                    .unique_id_within_function,
            )
            .unwrap()
            .clone();

        let node = &location_expression.node;

        let chain_len = location_expression.access_chain.len();
        let accesses_count = chain_len;
        /* if true {
            chain_len.saturating_sub(1)
        } else {
            chain_len
        };
        */

        let mut current_location = Destination::Register(start_reg);

        // Loop over the consecutive accesses until we find the actual frame relative address (TypedRegister)
        for access in location_expression.access_chain.iter().take(accesses_count) {
            match &access.kind {
                LocationAccessKind::FieldIndex(_anonymous_struct_type, field_index) => {
                    let ty = current_location.vm_type().underlying();
                    let offset_item = ty.get_field_offset(*field_index).unwrap();

                    current_location = current_location.add_offset(
                        offset_item.offset,
                        VmType::new_unknown_placement(offset_item.ty.clone()),
                    );
                }
                LocationAccessKind::Subscript(slice_type, int_expr) => {
                    let element_gen_type = layout_type(&slice_type.element);
                    todo!()
                    /*
                    current_location = self.subscript_helper_from_location_to_location(
                        current_location,
                        &element_gen_type,
                        int_expr,
                        BoundsCheck::KnownSizeAtCompileTime(slice_type.fixed_size as u16),
                        &int_expr.node,
                        "subscript",
                        ctx,
                    );

                     */
                }
                LocationAccessKind::SubscriptVec(element_type, int_expression) => {
                    current_location = self.vec_subscript_helper(
                        &current_location,
                        element_type,
                        int_expression,
                        ctx,
                    );
                }
                LocationAccessKind::IntrinsicSubscript(
                    _intrinsic_function,
                    arguments_to_the_intrinsic,
                ) => {
                    let layout_item_type = layout_type(&access.ty);
                    let get_item_target_reg = self.temp_registers.allocate(
                        VmType::new_unknown_placement(layout_item_type),
                        "intrinsic subscript",
                    );
                    let collection_reg = self.emit_ptr_reg_from_detailed_location(
                        &current_location,
                        &access.node,
                        "lvalue chain",
                    );

                    // Fetching from vector, map, etc. are done using intrinsic calls

                    self.emit_collection_get(
                        node,
                        &collection_reg,
                        arguments_to_the_intrinsic,
                        get_item_target_reg.register(),
                        ctx,
                    );

                    current_location = Destination::new_reg(get_item_target_reg.register);
                }
            }
        }

        current_location
    }
}
