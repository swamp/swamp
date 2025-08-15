/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use source_map_node::Node;
use swamp_vm_isa::HeapMemoryAddress;
use swamp_vm_types::types::{BasicTypeKind, Place, TypedRegister, VmType};
use swamp_vm_types::{MemoryLocation, PointerLocation};

impl CodeBuilder<'_> {
    /// Emits a Swamp VM instruction to load a **scalar** value from a memory location defined by a
    /// **base pointer register and a compile-time offset** (base-plus-offset addressing).
    ///
    /// This function acts as a low-level instruction emitter and is typically used by other
    /// higher-level `emit_load` functions, rather than being called directly. It selects
    /// between `ld8` (for 8-bit types like `B8`, `U8`) and `ld32` (for 32-bit types like
    /// `Fixed32`, `U32`, `S32`, `InternalStringPointer`) instructions based on the target register's type.
    pub(crate) fn emit_load_scalar_from_memory_offset_instruction(
        &mut self,
        target: &TypedRegister,
        source_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        // Choose the appropriate load instruction based on the target register's type
        let underlying_scalar = target.underlying();
        match underlying_scalar.kind {
            BasicTypeKind::Fixed32
            | BasicTypeKind::U32
            | BasicTypeKind::S32
            | BasicTypeKind::StringView { .. } => {
                self.builder.add_ld32_from_pointer_with_offset_u16(
                    target,
                    &source_location.base_ptr_reg,
                    source_location.offset,
                    node,
                    &format!("{comment} (load int)"),
                );
            }
            BasicTypeKind::B8 | BasicTypeKind::U8 => {
                self.builder.add_ld8_from_pointer_with_offset(
                    target,
                    &source_location.base_ptr_reg,
                    source_location.offset,
                    node,
                    &format!("{comment} (load bool)"),
                );
            }
            _ => panic!("Unsupported primitive type in add_load_primitive: {underlying_scalar:?}", ),
        }
    }

    /// Emits a Swamp VM instruction to load a **scalar** value directly from a fixed,
    /// **absolute memory address**.
    ///
    /// This is typically only used for loading **constants** (that has been
    /// calculated before other normal functions have been started).
    /// It generates specific load instructions (`ld8` for 8-bit types, `ld32` for 32-bit types).
    /// If the type is `Empty`, no code is emitted as it has a size of zero.
    pub(crate) fn emit_load_scalar_from_absolute_address_instruction(
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

    /// Emits a Swamp VM instruction to store a **scalar** value from a `primitive_reg`
    /// to a memory location defined by a **base pointer register and a compile-time offset**.
    ///
    /// This function is a low-level instruction emitter, acting as a helper for
    /// `emit_store_to_pointer_target`. It selects between `st8` (for 8-bit types)
    /// and `st32` (for 32-bit types) instructions based on the primitive register's type.
    pub(crate) fn emit_store_scalar_to_memory_offset_instruction(
        &mut self,
        memory_location: &MemoryLocation,
        primitive_reg: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        let underlying = primitive_reg.ty.basic_type();
        match underlying.kind {
            BasicTypeKind::B8 | BasicTypeKind::U8 => {
                self.builder.add_st8_using_ptr_with_offset(
                    memory_location,
                    primitive_reg,
                    node,
                    comment,
                );
            }
            BasicTypeKind::U32 | BasicTypeKind::S32 | BasicTypeKind::Fixed32 => {
                self.builder.add_st32_using_ptr_with_offset(
                    memory_location,
                    primitive_reg,
                    node,
                    comment,
                );
            }

            _ => {
                if underlying.kind.is_reg_copy() {
                    self.builder.add_st32_using_ptr_with_offset(
                        memory_location,
                        primitive_reg,
                        node,
                        comment,
                    );
                } else {
                    panic!("must be scalar {underlying} {}", primitive_reg.ty);
                }
            }
        }
    }

    /// Computes the **effective memory address** based on a given `Destination` and
    /// places this address into a `TypedRegister`.
    ///
    /// If the `location` is already a `Register`, that register's content (expected to be an address)
    /// is returned directly. If the `location` is `Memory` (defined by a base pointer and an offset),
    /// this function generates an `add_u32_imm` instruction to add the offset to the base pointer,
    /// storing the resulting calculated address in a new temporary register.
    pub(crate) fn emit_compute_effective_address_from_location_to_register(
        &mut self,
        memory_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) -> PointerLocation {
        if memory_location.offset.0 == 0 {
            // Create a new TypedRegister with the same register but *updated* type
            // Caused bugs for things like transformers that needed the correct type.
            PointerLocation {
                ptr_reg: TypedRegister {
                    index: memory_location.base_ptr_reg.index,
                    ty: memory_location.ty.clone(), // Use the field's type, not the base register's type
                    comment: comment.to_string(),
                },
            }
        } else {
            let final_ptr_target_reg = self.temp_registers.allocate(
                memory_location.ty.clone(),
                &format!("{comment} - final_ptr_target_reg"),
            );

            self.builder.add_add_u32_imm(
                final_ptr_target_reg.register(),
                &memory_location.base_ptr_reg,
                memory_location.offset.0,
                node,
                &format!("{comment} (add to resolved new base_ptr)"),
            );
            PointerLocation {
                ptr_reg: final_ptr_target_reg.register().clone(),
            }
        }
    }

    /// Computes the **effective memory address** based on a given `Destination` and
    /// places this address into a `TypedRegister`.
    ///
    /// If the `location` is already a `Register`, that register's content (expected to be an address)
    /// is returned directly. If the `location` is `Memory` (defined by a base pointer and an offset),
    /// this function generates an `add_u32_imm` instruction to add the offset to the base pointer,
    /// storing the resulting calculated address in a new temporary register.
    pub(crate) fn emit_compute_effective_address_to_register(
        &mut self,
        location: &Place,
        node: &Node,
        comment: &str,
    ) -> TypedRegister {
        match location {
            Place::Register(reg) => reg.clone(),
            Place::Memory(memory_location) => {
                self.emit_compute_effective_address_from_location_to_register(
                    memory_location,
                    node,
                    comment,
                )
                    .ptr_reg
            }
            Place::Discard => {
                panic!("can not compute effective address from unit")
            }
        }
    }

    /// Computes the **effective memory address** based on a given `Destination` and
    /// places this address into the `target_reg`.
    pub(crate) fn emit_compute_effective_address_to_target_register(
        &mut self,
        target_reg: &TypedRegister,
        source_location: &Place,
        node: &Node,
        comment: &str,
    ) {
        match source_location {
            Place::Register(reg) => {
                self.builder.add_mov_reg(target_reg, reg, node, comment);
            }
            Place::Memory(memory_location) => {
                if memory_location.offset.0 == 0 {
                    self.builder.add_mov_reg(
                        target_reg,
                        &memory_location.base_ptr_reg,
                        node,
                        comment,
                    );
                } else {
                    self.builder.add_add_u32_imm(
                        target_reg,
                        &memory_location.base_ptr_reg,
                        memory_location.offset.0,
                        node,
                        &format!("{comment} (add to resolved new base_ptr)"),
                    );
                }
            }
            Place::Discard => {
                panic!("can not compute effective address from unit")
            }
        }
    }
}
