/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{
    BinaryInstruction, ConstantMemoryAddress, CountU16, FrameMemoryAddress,
    FrameMemoryAddressIndirectPointer, FrameMemorySize, InstructionPosition, MemoryAddress,
    MemorySize,
};

#[derive(Debug)]
pub struct PatchPosition(pub InstructionPosition);

pub struct InstructionBuilder {
    pub instructions: Vec<BinaryInstruction>,
    pub comments: Vec<String>,
}

impl InstructionBuilder {}

impl InstructionBuilder {}

impl Default for InstructionBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl InstructionBuilder {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            instructions: Vec::new(),
            comments: Vec::new(),
        }
    }

    #[must_use]
    pub fn position(&self) -> InstructionPosition {
        InstructionPosition(self.instructions.len() as u16)
    }

    pub fn add_jmp_if_equal_placeholder(&mut self, comment: &str) -> PatchPosition {
        let position = self.position();

        self.add_instruction(OpCode::Bz, &[0], comment);

        PatchPosition(position)
    }

    pub fn add_jmp_if_not_equal_placeholder(&mut self, comment: &str) -> PatchPosition {
        let position = self.position();

        self.add_instruction(OpCode::Bnz, &[0], comment);

        PatchPosition(position)
    }

    pub fn add_vec_subscript(
        &mut self,
        target: FrameMemoryAddress,
        self_addr: FrameMemoryAddress,
        index: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::VecSubscript,
            &[target.0, self_addr.0, index.0],
            comment,
        );
    }

    pub fn add_vec_subscript_mut(
        &mut self,
        target: FrameMemoryAddress,
        self_addr: FrameMemoryAddress,
        index: FrameMemoryAddress,
        value_addr: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::VecSubscriptMut,
            &[target.0, self_addr.0, index.0, value_addr.0],
            comment,
        );
    }

    pub fn add_vec_iter_next_placeholder(
        &mut self,
        iterator_target: FrameMemoryAddress,
        closure_variable: FrameMemoryAddress,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.add_instruction(
            OpCode::VecIterNext,
            &[iterator_target.0, closure_variable.0, 0],
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_vec_iter_next_pair_placeholder(
        &mut self,
        iterator_target: FrameMemoryAddress,
        closure_variable: FrameMemoryAddress,
        closure_variable_b: FrameMemoryAddress,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.add_instruction(
            OpCode::VecIterNextPair,
            &[
                iterator_target.0,
                closure_variable.0,
                closure_variable_b.0,
                0,
            ],
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_eq_u8_immediate(
        &mut self,
        source_addr: FrameMemoryAddress,
        immediate: u8,
        comment: &str,
    ) {
        self.add_instruction(OpCode::Eq8Imm, &[source_addr.0, immediate as u16], comment);
    }

    pub fn add_eq_32(
        &mut self,
        addr_a: FrameMemoryAddress,
        addr_b: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::Eq32, &[addr_a.0, addr_b.0], comment);
    }

    pub fn add_call_placeholder(&mut self, comment: &str) -> PatchPosition {
        let position = self.position();
        self.add_instruction(OpCode::Call, &[0], comment);
        PatchPosition(position)
    }

    pub fn add_jump_placeholder(&mut self, comment: &str) -> PatchPosition {
        let position = self.position();

        self.add_instruction(OpCode::Jmp, &[0], comment);

        PatchPosition(position)
    }

    pub fn add_enter(&mut self, size: FrameMemorySize, comment: &str) {
        self.add_instruction(OpCode::Enter, &[size.0], comment);
    }

    // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
    pub fn add_mov(
        &mut self,
        target: FrameMemoryAddress,
        source: FrameMemoryAddress,
        size: MemorySize,
        comment: &str,
    ) {
        self.add_instruction(OpCode::Mov, &[target.0, source.0, size.0], comment);
    }

    // for overlap moves
    pub fn add_movlp(
        &mut self,
        target: FrameMemoryAddress,
        source: FrameMemoryAddress,
        size: MemorySize,
        comment: &str,
    ) {
        self.add_instruction(OpCode::MovLp, &[target.0, source.0, size.0], comment);
    }

    pub fn add_ret(&mut self, comment: &str) {
        self.add_instruction(OpCode::Ret, &[], comment);
    }

    pub fn add_hlt(&mut self, comment: &str) {
        self.add_instruction(OpCode::Hlt, &[], comment);
    }

    pub fn add_call(&mut self, function_ip: &InstructionPosition, comment: &str) {
        self.add_instruction(OpCode::Call, &[function_ip.0], comment);
    }

    pub fn add_host_call(
        &mut self,
        host_function_id: u16,
        arguments_size: MemorySize,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::HostCall,
            &[host_function_id, arguments_size.0],
            comment,
        );
    }

    /// # Panics
    ///
    pub fn patch_jump(
        &mut self,
        patch_position: PatchPosition,
        target_position: &InstructionPosition,
    ) {
        const JMP_IF_NOT: u8 = OpCode::Bz as u8;
        const JMP_IF: u8 = OpCode::Bnz as u8;
        const JMP: u8 = OpCode::Jmp as u8;

        const VEC_ITER_NEXT: u8 = OpCode::VecIterNext as u8;
        const VEC_ITER_NEXT_PAIR: u8 = OpCode::VecIterNextPair as u8;
        const MAP_ITER_NEXT: u8 = OpCode::MapIterNext as u8;
        const MAP_ITER_NEXT_PAIR: u8 = OpCode::MapIterNextPair as u8;

        const RANGE_ITER_NEXT: u8 = OpCode::RangeIterNext as u8;
        const RANGE_ITER_NEXT_PAIR: u8 = OpCode::RangeIterNextPair as u8;

        let instruction = &mut self.instructions[patch_position.0.0 as usize];

        match instruction.opcode {
            JMP_IF_NOT => {
                instruction.operands[0] = target_position.0 as u16 - 1;
            }
            JMP_IF => {
                instruction.operands[0] = target_position.0 as u16 - 1;
            }
            JMP => {
                instruction.operands[0] = target_position.0 as u16 - 1;
            }

            VEC_ITER_NEXT => {
                instruction.operands[2] = target_position.0 as u16 - 1;
            }

            MAP_ITER_NEXT => {
                instruction.operands[2] = target_position.0 as u16 - 1;
            }

            RANGE_ITER_NEXT => {
                instruction.operands[2] = target_position.0 as u16 - 1;
            }

            VEC_ITER_NEXT_PAIR => {
                instruction.operands[3] = target_position.0 as u16 - 1;
            }

            MAP_ITER_NEXT_PAIR => {
                instruction.operands[3] = target_position.0 as u16 - 1;
            }

            RANGE_ITER_NEXT_PAIR => {
                instruction.operands[3] = target_position.0 as u16 - 1;
            }

            _ => panic!("Attempted to patch a non-jump instruction at position {patch_position:?}"),
        }
    }

    // It takes ownership of the patch position
    pub fn patch_jump_here(&mut self, jump_position: PatchPosition) {
        self.patch_jump(jump_position, &self.position());
    }

    /// # Panics
    ///
    pub fn patch_call(&mut self, patch_position: PatchPosition, ip: &InstructionPosition) {
        const CALL: u8 = OpCode::Call as u8;

        let instruction = &mut self.instructions[patch_position.0.0 as usize];

        match instruction.opcode {
            CALL => {
                instruction.operands[0] = ip.0 as u16 - 1;
            }
            _ => panic!("Attempted to patch a non-call instruction at position {patch_position:?}"),
        }
    }

    pub fn add_jmp(&mut self, ip: InstructionPosition, comment: &str) {
        self.add_instruction(OpCode::Jmp, &[ip.0 - 1], comment);
    }

    pub fn add_map_iter_init(
        &mut self,
        iterator_target: FrameMemoryAddress,
        pointer_to_map: FrameMemoryAddressIndirectPointer,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::MapIterInit,
            &[iterator_target.0, pointer_to_map.0.0],
            comment,
        );
    }

    pub fn add_map_iter_next_placeholder(
        &mut self,
        iterator_target: FrameMemoryAddress,
        closure_variable: FrameMemoryAddress,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.add_instruction(
            OpCode::MapIterNext,
            &[iterator_target.0, closure_variable.0, 0],
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_map_iter_next_pair_placeholder(
        &mut self,
        iterator_target: FrameMemoryAddress,
        closure_variable: FrameMemoryAddress,
        closure_variable_b: FrameMemoryAddress,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.add_instruction(
            OpCode::MapIterNextPair,
            &[
                iterator_target.0,
                closure_variable.0,
                closure_variable_b.0,
                0,
            ],
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_range_iter_next_placeholder(
        &mut self,
        iterator_target: FrameMemoryAddress,
        closure_variable: FrameMemoryAddress,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.add_instruction(
            OpCode::RangeIterNext,
            &[iterator_target.0, closure_variable.0, 0],
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_range_iter_next_pair_placeholder(
        &mut self,
        iterator_target: FrameMemoryAddress,
        closure_variable: FrameMemoryAddress,
        closure_variable_b: FrameMemoryAddress,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.add_instruction(
            OpCode::RangeIterNextPair,
            &[
                iterator_target.0,
                closure_variable.0,
                closure_variable_b.0,
                0,
            ],
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_string_from_constant_slice(
        &mut self,
        target_string: FrameMemoryAddress,
        constant_addr: ConstantMemoryAddress,
        byte_count: MemorySize,
        comment: &str,
    ) {
        let (lower_bits, upper_bits) = Self::convert_to_lower_and_upper(constant_addr.0);

        self.add_instruction(
            OpCode::StringFromConstantSlice,
            &[target_string.0, lower_bits, upper_bits, byte_count.0],
            comment,
        );
    }

    pub fn add_string_append(
        &mut self,
        dst_offset: FrameMemoryAddress,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::StringAppend,
            &[dst_offset.0, lhs_offset.0, rhs_offset.0],
            comment,
        );
    }

    pub fn add_string_len(
        &mut self,
        len_target: FrameMemoryAddress,
        indirect: FrameMemoryAddressIndirectPointer,
        comment: &str,
    ) {
        self.add_instruction(OpCode::StringLen, &[len_target.0, indirect.0.0], comment);
    }

    pub fn add_vec_len(
        &mut self,
        len_target: FrameMemoryAddress,
        self_addr: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::VecLen, &[len_target.0, self_addr.0], comment);
    }

    pub fn add_vec_from_slice(
        &mut self,
        target: FrameMemoryAddress,
        source_slice: FrameMemoryAddress,
        element_size: MemorySize,
        element_count: CountU16,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::VecFromSlice,
            &[target.0, source_slice.0, element_size.0, element_count.0],
            comment,
        );
    }

    pub fn add_vec_iter_init(
        &mut self,
        iterator_target: FrameMemoryAddress,
        pointer_to_vec: FrameMemoryAddressIndirectPointer,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::VecIterInit,
            &[iterator_target.0, pointer_to_vec.0.0],
            comment,
        );
    }

    pub fn add_vec_iter_next(
        &mut self,
        iterator_target: FrameMemoryAddress,
        closure_variable: FrameMemoryAddress,
        instruction_position: InstructionPosition,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::VecIterNext,
            &[
                iterator_target.0,
                closure_variable.0,
                instruction_position.0,
            ],
            comment,
        );
    }

    pub fn add_vec_iter_next_pair(
        &mut self,
        iterator_target: FrameMemoryAddress,
        closure_variable_key: FrameMemoryAddress,
        closure_variable_value: FrameMemoryAddress,
        instruction_position: InstructionPosition,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::VecIterNextPair,
            &[
                iterator_target.0,
                closure_variable_key.0,
                closure_variable_value.0,
                instruction_position.0,
            ],
            comment,
        );
    }

    fn convert_to_lower_and_upper(data: u32) -> (u16, u16) {
        let lower_bits = (data & 0xFFFF) as u16;
        let upper_bits = (data >> 16) as u16;

        (lower_bits, upper_bits)
    }

    pub fn add_ld32(&mut self, dst_offset: FrameMemoryAddress, value: i32, comment: &str) {
        let (lower_bits, upper_bits) = Self::convert_to_lower_and_upper(value as u32);

        self.add_instruction(
            OpCode::Ld32,
            &[dst_offset.0, lower_bits, upper_bits],
            comment,
        );
    }

    pub fn add_ld_constant(
        &mut self,
        target_addr: FrameMemoryAddress,
        constant_addr: ConstantMemoryAddress,
        size: MemorySize,
        comment: &str,
    ) {
        let value_u32 = constant_addr.0;

        let lower_bits = (value_u32 & 0xFFFF) as u16;
        let upper_bits = (value_u32 >> 16) as u16;

        self.add_instruction(
            OpCode::LdConst,
            &[target_addr.0, lower_bits, upper_bits, size.0],
            comment,
        );
    }

    pub fn add_ld8(&mut self, dst_offset: FrameMemoryAddress, value: u8, comment: &str) {
        self.add_instruction(OpCode::Ld8, &[dst_offset.0, value as u16], comment);
    }

    pub fn add_add_i32(
        &mut self,
        dst_offset: FrameMemoryAddress,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::AddI32,
            &[dst_offset.0, lhs_offset.0, rhs_offset.0],
            comment,
        );
    }

    pub fn add_mod_i32(
        &mut self,
        dst_offset: FrameMemoryAddress,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::ModI32,
            &[dst_offset.0, lhs_offset.0, rhs_offset.0],
            comment,
        );
    }

    pub fn add_sub_i32(
        &mut self,
        dst_offset: FrameMemoryAddress,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::SubI32,
            &[dst_offset.0, lhs_offset.0, rhs_offset.0],
            comment,
        );
    }

    pub fn add_add_f32(
        &mut self,
        dst_offset: FrameMemoryAddress,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::AddF32,
            &[dst_offset.0, lhs_offset.0, rhs_offset.0],
            comment,
        );
    }

    pub fn add_mul_i32(
        &mut self,
        dst_offset: FrameMemoryAddress,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::MulI32,
            &[dst_offset.0, lhs_offset.0, rhs_offset.0],
            comment,
        );
    }

    pub fn add_neg_i32(
        &mut self,
        target: FrameMemoryAddress,
        source: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::NegI32, &[target.0, source.0], comment);
    }

    pub fn add_neg_f32(
        &mut self,
        target: FrameMemoryAddress,
        source: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::NegF32, &[target.0, source.0], comment);
    }

    pub fn add_jmp_if(
        &mut self,
        condition_offset: FrameMemoryAddress,
        jmp_target: &InstructionPosition,
        comment: &str,
    ) {
        self.add_instruction(OpCode::Bnz, &[condition_offset.0, jmp_target.0], comment);
    }

    pub fn add_jmp_if_not(
        &mut self,
        condition_offset: MemoryAddress,
        jmp_target: InstructionPosition,
        comment: &str,
    ) {
        self.add_instruction(OpCode::Bz, &[condition_offset.0, jmp_target.0], comment);
    }

    pub fn add_lt_i32(
        &mut self,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::LtI32, &[lhs_offset.0, rhs_offset.0], comment);
    }

    pub fn add_le_i32(
        &mut self,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::LeI32, &[lhs_offset.0, rhs_offset.0], comment);
    }

    pub fn add_gt_i32(
        &mut self,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::GtI32, &[lhs_offset.0, rhs_offset.0], comment);
    }

    pub fn add_ge_i32(
        &mut self,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::GeI32, &[lhs_offset.0, rhs_offset.0], comment);
    }

    pub fn add_tst8(&mut self, addr: FrameMemoryAddress, comment: &str) {
        self.add_instruction(OpCode::Tst8, &[addr.0], comment);
    }

    // Collection specific
    pub fn add_map_new_from_slice(
        &mut self,
        map_target_addr: FrameMemoryAddress,
        slice_source_addr: FrameMemoryAddress,
        key_size: MemorySize,
        value_size: MemorySize,
        count: CountU16,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::MapNewFromPairs,
            &[
                map_target_addr.0,
                slice_source_addr.0,
                key_size.0,
                value_size.0,
                count.0,
            ],
            comment,
        );
    }

    pub fn add_map_remove(
        &mut self,
        map_target_addr: FrameMemoryAddress,
        key_addr: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::MapRemove, &[map_target_addr.0, key_addr.0], comment);
    }

    fn add_instruction(&mut self, op_code: OpCode, operands: &[u16], comment: &str) {
        let mut array: [u16; 5] = [0; 5];
        assert!(operands.len() <= 5);
        let len = operands.len();
        array[..len].copy_from_slice(&operands[..len]);
        self.instructions.push(BinaryInstruction {
            opcode: op_code as u8,
            operands: array,
        });
        self.comments.push(comment.to_string());
    }
    pub fn add_ld_u16(&mut self, dest: FrameMemoryAddress, data: u16, comment: &str) {
        self.add_instruction(OpCode::Ld16, &[dest.0, data], comment);
    }

    pub fn add_int_rnd(
        &mut self,
        dest: FrameMemoryAddress,
        self_int: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::IntToRnd, &[dest.0, self_int.0], comment);
    }

    /*

        pub fn add_lt_u16(
        &mut self,
        dest: FrameMemoryAddress,
        a: FrameMemoryAddress,
        b: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::LtU16, &[dest.0, a.0, b.0], comment);
    }


    pub fn add_stx(
        &mut self,
        indirect_target: FrameMemoryAddress,
        offset: MemoryOffset,
        source_address: FrameMemoryAddress,
        size: MemorySize,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::Stx,
            &[indirect_target.0, offset.0, source_address.0, size.0],
            comment,
        );
    }
    pub fn add_alloc(&mut self, target: FrameMemoryAddress, size: MemorySize, comment: &str) {
        self.add_instruction(OpCode::Alloc, &[target.0, size.0], comment);
    }


        pub fn add_load_frame_address(
        &mut self,
        dest: FrameMemoryAddress,
        addr: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_ld_u16(dest, addr.0, comment);
    }

    */
}
