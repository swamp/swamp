use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use source_map_node::Node;
use swamp_semantic::Literal;
use swamp_types::Type;
use swamp_vm_types::types::{Destination, VmType, int_type, string_type};
use swamp_vm_types::{HeapMemoryAddress, StringHeader};

impl CodeBuilder<'_> {
    #[allow(clippy::too_many_lines)]
    pub fn emit_literal(
        &mut self,
        output: &Destination,
        basic_literal: &Literal,
        node: &Node,
        ctx: &Context,
    ) {
        match basic_literal {
            Literal::StringLiteral(str) => {
                self.emit_string_literal(output, node, str, ctx);
            }
            Literal::IntLiteral(int) => match output {
                Destination::Register(target_reg) => {
                    self.builder.add_mov_32_immediate_value(
                        target_reg,
                        *int as u32,
                        node,
                        "int literal",
                    );
                }
                Destination::Memory(location) => {
                    let temp_int_literal_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(int_type()),
                        "temporary for int literal",
                    );
                    self.builder.add_mov_32_immediate_value(
                        temp_int_literal_reg.register(),
                        *int as u32,
                        node,
                        "int literal",
                    );
                    self.builder.add_st32_using_ptr_with_offset(
                        location,
                        temp_int_literal_reg.register(),
                        node,
                        "copy int literal into destination memory",
                    );
                }
                Destination::Unit => {
                    panic!("int can not materialize into nothing")
                }
            },
            Literal::FloatLiteral(fixed_point) => match output {
                Destination::Register(target_reg) => {
                    self.builder.add_mov_32_immediate_value(
                        target_reg,
                        fixed_point.inner() as u32,
                        node,
                        "float literal",
                    );
                }
                Destination::Memory(location) => {
                    let temp_fixed_point_temp_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(int_type()),
                        "temporary for float literal",
                    );
                    self.builder.add_mov_32_immediate_value(
                        temp_fixed_point_temp_reg.register(),
                        fixed_point.inner() as u32,
                        node,
                        "float literal",
                    );
                    self.builder.add_st32_using_ptr_with_offset(
                        location,
                        temp_fixed_point_temp_reg.register(),
                        node,
                        "copy float literal into destination memory",
                    );
                }
                Destination::Unit => {
                    panic!("int can not materialize into nothing")
                }
            },
            Literal::NoneLiteral => {
                let union_info = output.ty().unwrap_info().unwrap();

                match output {
                    Destination::Register(target_reg) => {
                        self.builder
                            .add_mov8_immediate(target_reg, 0, node, "none literal");
                    }
                    Destination::Memory(location) => {
                        let temp_none_literal_reg = self.temp_registers.allocate(
                            VmType::new_contained_in_register(int_type()),
                            "temporary for none literal",
                        );
                        self.builder.add_mov8_immediate(
                            temp_none_literal_reg.register(),
                            0,
                            node,
                            "none literal",
                        );

                        self.builder.add_st8_using_ptr_with_offset(
                            location,
                            temp_none_literal_reg.register(),
                            node,
                            "copy none literal into destination memory",
                        );
                    }
                    Destination::Unit => {
                        panic!("none can not materialize into nothing")
                    }
                }
            }
            Literal::BoolLiteral(truthy) => match output {
                Destination::Register(target_reg) => {
                    self.builder.add_mov8_immediate(
                        target_reg,
                        u8::from(*truthy),
                        node,
                        "bool literal",
                    );
                }
                Destination::Memory(location) => {
                    let temp_bool_literal_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(int_type()),
                        "temporary for bool literal",
                    );
                    self.builder.add_mov8_immediate(
                        temp_bool_literal_reg.register(),
                        u8::from(*truthy),
                        node,
                        "bool literal",
                    );

                    self.builder.add_st8_using_ptr_with_offset(
                        location,
                        temp_bool_literal_reg.register(),
                        node,
                        "copy bool literal into destination memory",
                    );
                }
                Destination::Unit => {
                    panic!("int can not materialize into nothing")
                }
            },

            Literal::EnumVariantLiteral(enum_type, enum_variant, enum_variant_payload) => {
                // A enum variant literal can not be represented as a register, not even a pointer to it, it needs materialization into memory
                self.emit_enum_variant_to_memory_location(
                    &output.grab_aggregate_memory_location(),
                    enum_type,
                    enum_variant,
                    enum_variant_payload,
                    node,
                    ctx,
                );
            }
            Literal::TupleLiteral(types, expressions) => {
                // A tuple literal can not be represented as a register, not even a pointer to it, it needs materialization into memory
                self.emit_tuple_literal_into_memory(
                    &output.grab_aggregate_memory_location(),
                    types,
                    expressions,
                    ctx,
                    node,
                );
            }
            Literal::Slice(slice_type, expressions) => {
                // A tuple literal can not be represented as a register, not even a pointer to it, it needs materialization into memory
                let Type::InternalInitializerList(element_type) = slice_type else {
                    panic!("must be slice")
                };
                let element_gen_type = layout_type(element_type);
                self.emit_slice_literal_into_target_lvalue_memory_location(
                    &output.grab_aggregate_memory_location(),
                    &element_gen_type,
                    expressions,
                    ctx,
                );
            }
            Literal::SlicePair(slice_pair_type, pairs) => {
                todo!() //self.emit_slice_pair_literal(slice_pair_type, pairs, node, ctx);
            }
        }
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
}
