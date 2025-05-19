/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_node::Node;
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::types::{BasicTypeKind, TypedRegister};
pub use swamp_vm_types::{
    BinaryInstruction, FrameMemoryAddress, FrameMemoryRegion, FrameMemorySize,
    HEAP_PTR_ON_FRAME_SIZE, HeapMemoryOffset, HeapMemoryRegion, InstructionPosition,
    InstructionPositionOffset, MemoryOffset, MemorySize, Meta, PatchPosition, RANGE_HEADER_SIZE,
    RANGE_ITERATOR_SIZE, ZFlagPolarity,
};
use swamp_vm_types::{HeapMemoryAddress, ProgramCounterDelta};

/// Keeps track of all the instructions, and the corresponding meta information (comments and node).
pub struct InstructionBuilderState {
    pub instructions: Vec<BinaryInstruction>,
    pub meta: Vec<Meta>,
}

pub fn u16_to_u8_pair(v: u16) -> (u8, u8) {
    let bytes = v.to_le_bytes();
    (bytes[0], bytes[1])
}

fn u32_to_bytes(a: u32) -> (u8, u8, u8, u8) {
    let bytes = a.to_le_bytes();
    (bytes[0], bytes[1], bytes[2], bytes[3])
}

impl Default for InstructionBuilderState {
    fn default() -> Self {
        Self::new()
    }
}
impl InstructionBuilderState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            meta: Vec::new(),
        }
    }

    #[must_use]
    pub fn position(&self) -> InstructionPosition {
        InstructionPosition(self.instructions.len() as u32)
    }

    pub fn patch_enter(
        &mut self,
        patch_position: PatchPosition,
        frame_memory_size: FrameMemorySize,
    ) {
        const ENTER: u8 = OpCode::Enter as u8;

        let instruction = &mut self.instructions[patch_position.0.0 as usize];

        match instruction.opcode {
            ENTER => {
                let bytes = frame_memory_size.0.to_le_bytes();

                instruction.operands[0] = bytes[0];
                instruction.operands[1] = bytes[1];
            }
            _ => {
                panic!("Attempted to patch a non-enter instruction at position {patch_position:?}")
            }
        }
    }

    /// # Panics
    ///
    pub fn patch_call(&mut self, patch_position: PatchPosition, ip: &InstructionPosition) {
        const CALL: u8 = OpCode::Call as u8;

        let instruction = &mut self.instructions[patch_position.0.0 as usize];

        match instruction.opcode {
            CALL => {
                let bytes = u32_to_bytes(ip.0);

                instruction.operands[0] = bytes.0;
                instruction.operands[1] = bytes.1;
                instruction.operands[2] = bytes.2;
                instruction.operands[3] = bytes.3;
            }
            _ => panic!("Attempted to patch a non-call instruction at position {patch_position:?}"),
        }
    }

    pub fn add_ret(&mut self, node: &Node, comment: &str) {
        self.add_instruction(OpCode::Ret, &[], node, comment);
    }

    pub fn add_hlt(&mut self, node: &Node, comment: &str) {
        self.add_instruction(OpCode::Hlt, &[], node, comment);
    }

    fn add_instruction(&mut self, op_code: OpCode, operands: &[u8], node: &Node, comment: &str) {
        let mut array: [u8; 8] = [0; 8];
        assert!(operands.len() <= 8);
        let len = operands.len();
        array[..len].copy_from_slice(&operands[..len]);
        self.instructions.push(BinaryInstruction {
            opcode: op_code as u8,
            operands: array,
        });
        let meta = Meta {
            comment: comment.to_string(),
            node: node.clone(),
        };

        self.meta.push(meta);
    }
}

pub struct InstructionBuilder<'a> {
    pub state: &'a mut InstructionBuilderState,
    temp_reg: u8,
}

impl<'a> InstructionBuilder<'a> {
    #[must_use]
    pub const fn new(state: &'a mut InstructionBuilderState) -> Self {
        Self {
            state,
            temp_reg: 32,
        }
    }
}

impl InstructionBuilder<'_> {
    /// # Panics
    /// if the register doesn't hold a primitive
    pub fn add_load_primitive(
        &mut self,
        target: &TypedRegister,
        base: &TypedRegister,
        offset: MemoryOffset,
        node: &Node,
        comment: &str,
    ) {
        // Choose the appropriate load instruction based on the target register's type
        match target.underlying().kind {
            BasicTypeKind::Fixed32 | BasicTypeKind::U32 | BasicTypeKind::S32 => {
                self.add_ld32_from_pointer_with_offset_u16(
                    target,
                    base,
                    offset,
                    node,
                    &format!("{comment} (load int)"),
                );
            }
            BasicTypeKind::B8 | BasicTypeKind::U8 => {
                self.add_ld8_from_pointer_with_offset_u16(
                    target,
                    base,
                    offset,
                    node,
                    &format!("{comment} (load bool)"),
                );
            }
            _ => panic!(
                "Unsupported primitive type in add_load_primitive: {:?}",
                target.ty
            ),
        }
    }

    pub fn add_not_t(&mut self, node: &Node, comment: &str) {
        self.state.add_instruction(OpCode::NotT, &[], node, comment);
    }

    #[must_use]
    pub fn position(&self) -> InstructionPosition {
        InstructionPosition(self.state.instructions.len() as u32)
    }

    pub fn add_enter_placeholder(&mut self, node: &Node, comment: &str) -> PatchPosition {
        let patch_position = PatchPosition(self.position());
        self.state
            .add_instruction(OpCode::Enter, &[0, 0], node, comment);

        patch_position
    }

    pub fn patch_enter(&mut self, size: FrameMemorySize, patch_position: PatchPosition) {
        self.state.patch_enter(patch_position, size);
    }

    pub fn add_jmp_if_equal_placeholder(&mut self, node: &Node, comment: &str) -> PatchPosition {
        let position = self.position();

        self.state
            .add_instruction(OpCode::BTrue, &[0, 0], node, comment);

        PatchPosition(position)
    }

    pub fn add_jmp_if_true_placeholder(&mut self, node: &Node, comment: &str) -> PatchPosition {
        self.add_jmp_if_equal_placeholder(node, comment)
    }

    pub fn add_jmp_if_not_equal_placeholder(
        &mut self,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();

        self.state
            .add_instruction(OpCode::BFalse, &[0, 0], node, comment);

        PatchPosition(position)
    }

    pub fn add_jmp_if_not_true_placeholder(&mut self, node: &Node, comment: &str) -> PatchPosition {
        self.add_jmp_if_not_equal_placeholder(node, comment)
    }

    pub fn add_jmp_if_not_equal_polarity_placeholder(
        &mut self,
        polarity: &ZFlagPolarity,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        match polarity {
            ZFlagPolarity::TrueWhenSet => self.add_jmp_if_not_equal_placeholder(node, comment),
            ZFlagPolarity::TrueWhenClear => self.add_jmp_if_equal_placeholder(node, comment),
        }
    }

    pub fn add_vec_swap(
        &mut self,
        vec_self_addr: &TypedRegister,
        int_index_a: &TypedRegister,
        int_index_b: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            vec_self_addr.ty().kind,
            BasicTypeKind::InternalVecView(_)
        ));
        assert_eq!(int_index_a.size(), int_index_b.size());
        self.state.add_instruction(
            OpCode::VecSwap,
            &[
                vec_self_addr.addressing(),
                int_index_a.addressing(),
                int_index_b.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_subscript(
        &mut self,
        target: &TypedRegister,
        self_addr: &TypedRegister,
        index: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        /* TODO: Bring this back // assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));

         */
        self.state.add_instruction(
            OpCode::VecGet,
            &[
                target.addressing(),
                self_addr.addressing(),
                index.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_get(
        &mut self,
        target: &TypedRegister,
        self_addr: &TypedRegister,
        index: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecView(_)
        ));
        self.state.add_instruction(
            OpCode::VecGet,
            &[
                target.addressing(),
                self_addr.addressing(),
                index.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_get_range(
        &mut self,
        target: &TypedRegister,
        vec_self_addr: &TypedRegister,
        range_header: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            vec_self_addr.ty().kind,
            BasicTypeKind::InternalVecView(_)
        ));

        self.state.add_instruction(
            OpCode::VecGetRange,
            &[
                target.addressing(),
                vec_self_addr.addressing(),
                range_header.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_set(
        &mut self,
        self_addr: &TypedRegister,
        index: &TypedRegister,
        value_addr: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecView(_)
        ));

        self.state.add_instruction(
            OpCode::VecSet,
            &[
                self_addr.addressing(),
                index.addressing(),
                value_addr.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_push(
        &mut self,
        self_addr: &TypedRegister,
        element_item: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        /*
        assert!(
            matches!(
                self_addr.underlying().kind,
                BasicTypeKind::InternalVecView(_)
            ),
            "what is this {:?}",
            self_addr.ty()
        );


         */
        self.state.add_instruction(
            OpCode::VecPush,
            &[self_addr.addressing(), element_item.addressing()],
            node,
            comment,
        );
    }

    pub fn add_vec_pop(
        &mut self,
        target_addr: &TypedRegister,
        self_addr: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecView(_)
        ));
        self.state.add_instruction(
            OpCode::VecPop,
            &[target_addr.addressing(), self_addr.addressing()],
            node,
            comment,
        );
    }

    pub fn add_vec_remove_index(
        &mut self,
        self_addr: &TypedRegister,
        element_item: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        /* TODO: Bring back //
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));

         */
        self.state.add_instruction(
            OpCode::VecRemoveIndex,
            &[self_addr.addressing(), element_item.addressing()],
            node,
            comment,
        );
    }

    pub fn add_vec_remove_index_get_value(
        &mut self,
        target_addr: &TypedRegister,
        self_addr: &TypedRegister,
        element_item: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecView(_)
        ));
        self.state.add_instruction(
            OpCode::VecRemoveIndexGetValue,
            &[
                target_addr.addressing(),
                self_addr.addressing(),
                element_item.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_iter_next_placeholder(
        &mut self,
        iterator_target: &TypedRegister,
        closure_variable: &TypedRegister,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        assert!(matches!(
            iterator_target.ty().kind,
            BasicTypeKind::InternalVecIterator
        ));
        let position = self.position();
        self.state.add_instruction(
            OpCode::VecIterNext,
            &[
                iterator_target.addressing(),
                closure_variable.addressing(),
                0,
            ],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_vec_iter_next_pair_placeholder(
        &mut self,
        iterator_target: &TypedRegister,
        closure_variable: &TypedRegister,
        closure_variable_b: &TypedRegister,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        assert!(matches!(
            iterator_target.ty().kind,
            BasicTypeKind::InternalVecIterator
        ));
        let position = self.position();
        self.state.add_instruction(
            OpCode::VecIterNextPair,
            &[
                iterator_target.addressing(),
                closure_variable.addressing(),
                closure_variable_b.addressing(),
                0,
            ],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_eq_u8_immediate(
        &mut self,
        source_addr: &TypedRegister,
        immediate: u8,
        node: &Node,
        comment: &str,
    ) {
        // TODO: BRING THIS BACK // assert!(source_addr.size().0 >= 1);
        self.state.add_instruction(
            OpCode::Eq8Imm,
            &[source_addr.addressing(), immediate],
            node,
            comment,
        );
    }

    pub fn add_call_placeholder(&mut self, node: &Node, comment: &str) -> PatchPosition {
        let position = self.position();
        self.state
            .add_instruction(OpCode::Call, &[0], node, comment);
        PatchPosition(position)
    }

    pub fn add_jump_placeholder(&mut self, node: &Node, comment: &str) -> PatchPosition {
        let position = self.position();

        self.state
            .add_instruction(OpCode::B, &[0, 0], node, comment);

        PatchPosition(position)
    }

    pub fn add_lea(
        &mut self,
        target_heap: &TypedRegister,
        frame_address_to_convert: FrameMemoryAddress,
        node: &Node,
        comment: &str,
    ) {
        let pairs = u16_to_u8_pair(frame_address_to_convert.0);
        self.state.add_instruction(
            OpCode::LdPtrFromEffectiveAddress,
            &[target_heap.addressing(), pairs.0, pairs.1],
            node,
            comment,
        );
    }

    pub fn add_lea_base_ptr_index_imm_element_size(
        &mut self,
        dest_reg: &TypedRegister,
        base_ptr: &TypedRegister,
        base_ptr_offset: MemoryOffset,
        index: &TypedRegister,
        size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        let pairs = u16_to_u8_pair(size.0);
        let base_ptr_offset_bytes = u16_to_u8_pair(base_ptr_offset.0);
        self.state.add_instruction(
            OpCode::LoadEffectiveAddressIndexMultiplier,
            &[
                dest_reg.addressing(),
                base_ptr.addressing(),
                base_ptr_offset_bytes.0,
                base_ptr_offset_bytes.1,
                index.addressing(),
                pairs.0,
                pairs.1,
            ],
            node,
            comment,
        );
    }

    pub fn add_ld_regs_from_frame(
        &mut self,
        target_reg: &TypedRegister,
        stored_in_frame: FrameMemoryAddress,
        count: u8,
        node: &Node,
        comment: &str,
    ) {
        let address_bytes = stored_in_frame.0.to_le_bytes();
        self.state.add_instruction(
            OpCode::LdRegFromFrame,
            &[
                target_reg.addressing(),
                address_bytes[0],
                address_bytes[1],
                count,
            ],
            node,
            comment,
        );
    }

    pub fn add_st_regs_to_frame(
        &mut self,
        frame_mem: FrameMemoryAddress,
        source_reg: &TypedRegister,
        count: u8,
        node: &Node,
        comment: &str,
    ) {
        let address_bytes = frame_mem.0.to_le_bytes();
        self.state.add_instruction(
            OpCode::StRegToFrame,
            &[
                address_bytes[0],
                address_bytes[1],
                source_reg.addressing(),
                count,
            ],
            node,
            comment,
        );
    }

    pub fn add_block_copy_with_offset(
        &mut self,
        target_base_ptr_reg: &TypedRegister,
        target_offset: MemoryOffset,
        source_base_ptr_reg: &TypedRegister,
        source_offset: MemoryOffset,
        memory_size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        let target_offset_bytes = u16_to_u8_pair(target_offset.0);
        let source_offset_bytes = u16_to_u8_pair(source_offset.0);
        let size_bytes = u16_to_u8_pair(memory_size.0);

        self.state.add_instruction(
            OpCode::BlockCopyWithOffsets,
            &[
                target_base_ptr_reg.addressing(),
                target_offset_bytes.0,
                target_offset_bytes.1,
                source_base_ptr_reg.addressing(),
                source_offset_bytes.0,
                source_offset_bytes.1,
                size_bytes.0,
                size_bytes.1,
            ],
            node,
            comment,
        );
    }

    pub fn add_block_copy(
        &mut self,
        target_base_ptr_reg: &TypedRegister,
        source_base_ptr_reg: &TypedRegister,
        memory_size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        let size_bytes = u16_to_u8_pair(memory_size.0);

        self.state.add_instruction(
            OpCode::BlockCopy,
            &[
                target_base_ptr_reg.addressing(),
                source_base_ptr_reg.addressing(),
                size_bytes.0,
                size_bytes.1,
            ],
            node,
            comment,
        );
    }

    /*
       // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
       pub fn add_mov_for_assignment(
           &mut self,
           target: &TypedRegister,
           source: &TypedRegister,
           node: &Node,
           comment: &str,
       ) {
           let decorated_reg = TypedRegister::new_vm_type(self.temp_reg, VmType::Immediate(Immediate::U32(source.size().0 as u32)));

           self.add_ld32(&decorated_reg, source.size().0 as u32, node, "size of source");

           assert_eq!(
               target.underlying().total_size,
               source.underlying().total_size,
               "problem with move {target:?} {source:?}"
           );
           self.add_mov_mem(target, source, decorated_reg, node, comment);
       }

    */

    /*

    // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
    pub fn add_mov_mem(
        &mut self,
        target: &TypedRegister,
        source: &TypedRegister,
        size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        assert_ne!(size.0, 0);

        let (lower_bits, upper_bits) = Self::u16_to_octets(size.0);
        self.state.add_instruction(
            OpCode::BlockCopy,
            &[
                target.addressing(),
                source.addressing(),
                lower_bits,
                upper_bits,
            ],
            node,
            comment,
        );
    }

     */

    /*
    // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
    pub fn add_mov_mem(
        &mut self,
        target: &TypedRegister,
        source: &TypedRegister,
        size: TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::MovMem,
            &[target.addressing(), source.addressing(), size.addressing()],
            node,
            comment,
        );
    }

     */

    pub fn add_panic(&mut self, str: &TypedRegister, node: &Node, comment: &str) {
        self.state
            .add_instruction(OpCode::Panic, &[str.addressing()], node, comment);
    }

    pub fn add_call(&mut self, function_ip: &InstructionPosition, node: &Node, comment: &str) {
        let ip_bytes = function_ip.0.to_le_bytes();
        self.state.add_instruction(
            OpCode::Call,
            &[ip_bytes[0], ip_bytes[1], ip_bytes[2], ip_bytes[3]],
            node,
            comment,
        );
    }

    pub fn add_host_call(
        &mut self,
        host_function_id: u16,
        arguments_count: u8,
        node: &Node,
        comment: &str,
    ) {
        let ip_bytes = Self::u16_to_octets(host_function_id);
        self.state.add_instruction(
            OpCode::HostCall,
            &[ip_bytes.0, ip_bytes.1, arguments_count],
            node,
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
        const JMP_IF_NOT: u8 = OpCode::BTrue as u8;
        const JMP_IF: u8 = OpCode::BFalse as u8;
        const JMP: u8 = OpCode::B as u8;

        const VEC_ITER_NEXT: u8 = OpCode::VecIterNext as u8;
        const VEC_ITER_NEXT_PAIR: u8 = OpCode::VecIterNextPair as u8;
        const MAP_ITER_NEXT: u8 = OpCode::MapIterNext as u8;
        const MAP_ITER_NEXT_PAIR: u8 = OpCode::MapIterNextPair as u8;

        const RANGE_ITER_NEXT: u8 = OpCode::RangeIterNext as u8;

        //const UNWRAP_JMP_NONE: u8 = OpCode::UnwrapJmpNone as u8;
        //const UNWRAP_JMP_SOME: u8 = OpCode::UnwrapJmpSome as u8;

        let instruction = &mut self.state.instructions[patch_position.0.0 as usize];
        let delta = *target_position - patch_position.0;
        let raw = delta.0 - 1; // when running, the PC has already advanced one step
        let delta_bytes = raw.to_le_bytes();

        match instruction.opcode {
            JMP_IF_NOT | JMP_IF | JMP => {
                instruction.operands[0] = delta_bytes[0];
                instruction.operands[1] = delta_bytes[1];
            }

            VEC_ITER_NEXT | MAP_ITER_NEXT | RANGE_ITER_NEXT => {
                instruction.operands[2] = delta_bytes[0];
                instruction.operands[3] = delta_bytes[1];
            }

            VEC_ITER_NEXT_PAIR | MAP_ITER_NEXT_PAIR => {
                instruction.operands[3] = delta_bytes[0];
                instruction.operands[4] = delta_bytes[1];
            }

            _ => panic!("Attempted to patch a non-jump instruction at position {patch_position:?}"),
        }
    }

    // It takes ownership of the patch position
    pub fn patch_jump_here(&mut self, jump_position: PatchPosition) {
        self.patch_jump(jump_position, &self.position());
    }

    pub fn add_jmp(&mut self, pc: InstructionPosition, node: &Node, comment: &str) {
        let delta_bytes = self.calculate_pc_delta_bytes(pc);
        self.state
            .add_instruction(OpCode::B, &[delta_bytes[0], delta_bytes[1]], node, comment);
    }

    fn calculate_pc_delta(&self, pc: InstructionPosition) -> ProgramCounterDelta {
        ProgramCounterDelta(((pc.0 as i32) - (self.position().0 as i32)) as i16)
    }
    fn calculate_pc_delta_bytes(&self, pc: InstructionPosition) -> [u8; 2] {
        let delta = self.calculate_pc_delta(pc);

        delta.0.to_le_bytes()
    }

    pub fn add_frame_memory_clear(
        &mut self,
        frame_region: FrameMemoryRegion,
        node: &Node,
        comment: &str,
    ) {
        let addr_bytes = u16_to_u8_pair(frame_region.addr.0);
        let size_bytes = u16_to_u8_pair(frame_region.size.0);
        self.state.add_instruction(
            OpCode::FrameMemClr,
            &[addr_bytes.0, addr_bytes.1, size_bytes.0, size_bytes.1],
            node,
            comment,
        );
    }

    // Slices

    pub fn add_slice_from_heap(
        &mut self,
        slice_dst: &TypedRegister,
        heap_region: &TypedRegister,
        element_size: &TypedRegister,
        element_count: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back //assert_ne!(slice_dst.size().0, 0);
        // TODO: Bring this back //assert_ne!(element_size.size().0, 0);

        self.state.add_instruction(
            OpCode::SliceFromHeap,
            &[
                slice_dst.addressing(),
                heap_region.addressing(),
                element_size.addressing(),
                element_count.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_slice_pair_from_heap(
        &mut self,
        slice_dst: &TypedRegister,
        heap_region: &TypedRegister,
        key_size: &TypedRegister,
        value_size: &TypedRegister,
        element_count: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert_ne!(slice_dst.size().0, 0);
        assert_ne!(key_size.size().0, 0);
        assert_ne!(value_size.size().0, 0);

        self.state.add_instruction(
            OpCode::SlicePairFromHeap,
            &[
                slice_dst.addressing(),
                heap_region.addressing(),
                key_size.addressing(),
                value_size.addressing(),
                element_count.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_map_iter_init(
        &mut self,
        iterator_target: &TypedRegister,
        pointer_to_map_header: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::MapIterInit,
            &[
                iterator_target.addressing(),
                pointer_to_map_header.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_map_iter_next_placeholder(
        &mut self,
        iterator_target: &TypedRegister,
        closure_variable: &TypedRegister,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.state.add_instruction(
            OpCode::MapIterNext,
            &[
                iterator_target.addressing(),
                closure_variable.addressing(),
                0,
            ],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_map_iter_next_pair_placeholder(
        &mut self,
        iterator_target: &TypedRegister,
        closure_variable: &TypedRegister,
        closure_variable_b: &TypedRegister,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.state.add_instruction(
            OpCode::MapIterNextPair,
            &[
                iterator_target.addressing(),
                closure_variable.addressing(),
                closure_variable_b.addressing(),
                0,
            ],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_range_iter_next_placeholder(
        &mut self,
        iterator_target: &TypedRegister,
        closure_variable: &TypedRegister,
        node: &Node,

        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.state.add_instruction(
            OpCode::RangeIterNext,
            &[
                iterator_target.addressing(),
                closure_variable.addressing(),
                0,
            ],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_string_append(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::StringAppend,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_string_cmp(
        &mut self,
        a: &TypedRegister,
        b: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::StringCmp,
            &[a.addressing(), b.addressing()],
            node,
            comment,
        );
    }

    pub fn add_vec_clear(&mut self, mut_self_addr: &TypedRegister, node: &Node, comment: &str) {
        self.state.add_instruction(
            OpCode::VecClear,
            &[mut_self_addr.addressing()],
            node,
            comment,
        );
    }

    pub fn add_vec_create(
        &mut self,
        mut_self_addr: &TypedRegister,
        element_byte_size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        let bytes = Self::u16_to_octets(element_byte_size.0);
        // assert_ne!(element_byte_size.0, 0); // TODO: Bring this back
        self.state.add_instruction(
            OpCode::VecCreate,
            &[mut_self_addr.addressing(), bytes.0, bytes.1],
            node,
            comment,
        );
    }

    pub fn add_vec_from_slice(
        &mut self,
        target: &TypedRegister,
        source_slice_header: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::VecFromSlice,
            &[target.addressing(), source_slice_header.addressing()],
            node,
            comment,
        );
    }

    pub fn add_range_iter_init(
        &mut self,
        iterator_target: &TypedRegister,
        range_source_header: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back // assert_eq!(iterator_target.size(), RANGE_ITERATOR_SIZE);
        // TODO: Bring this back // assert_eq!(range_source_header.size(), RANGE_HEADER_SIZE);

        self.state.add_instruction(
            OpCode::RangeIterInit,
            &[
                iterator_target.addressing(),
                range_source_header.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_iter_init(
        &mut self,
        iterator_target: &TypedRegister,
        pointer_to_vec_header: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::VecIterInit,
            &[
                iterator_target.addressing(),
                pointer_to_vec_header.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_iter_next(
        &mut self,
        iterator_target: &TypedRegister,
        closure_variable: &TypedRegister,
        instruction_position: InstructionPosition,
        node: &Node,
        comment: &str,
    ) {
        let bytes = self.calculate_pc_delta_bytes(instruction_position);
        self.state.add_instruction(
            OpCode::VecIterNext,
            &[
                iterator_target.addressing(),
                closure_variable.addressing(),
                bytes[0],
                bytes[1],
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_iter_next_pair(
        &mut self,
        iterator_target: &TypedRegister,
        closure_variable_key: &TypedRegister,
        closure_variable_value: &TypedRegister,
        instruction_position: InstructionPosition,
        node: &Node,
        comment: &str,
    ) {
        let bytes = self.calculate_pc_delta_bytes(instruction_position);
        self.state.add_instruction(
            OpCode::VecIterNextPair,
            &[
                iterator_target.addressing(),
                closure_variable_key.addressing(),
                closure_variable_value.addressing(),
                bytes[0],
                bytes[1],
            ],
            node,
            comment,
        );
    }

    fn convert_to_lower_and_upper(data: u32) -> (u8, u8, u8, u8) {
        data.to_le_bytes().into()
    }

    fn u16_to_octets(data: u16) -> (u8, u8) {
        data.to_le_bytes().into()
    }

    pub fn add_st32_using_ptr_with_offset(
        &mut self,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        u32_reg: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        //assert_eq!(u32_reg.ty().underlying().total_size.0, 4);
        let bytes = u16_to_u8_pair(offset.0);
        self.state.add_instruction(
            OpCode::St32UsingPtrWithOffset,
            &[base_ptr_reg.addressing(), bytes.0, bytes.1, u32_reg.index],
            node,
            comment,
        );
    }

    pub fn add_st16_using_ptr_with_offset(
        &mut self,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        u16_reg: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        //assert_eq!(u16_reg.ty().underlying().total_size.0, 2);
        let bytes = u16_to_u8_pair(offset.0);
        self.state.add_instruction(
            OpCode::St16UsingPtrWithOffset,
            &[base_ptr_reg.addressing(), bytes.0, bytes.1, u16_reg.index],
            node,
            comment,
        );
    }

    pub fn add_st8_using_ptr_with_offset(
        &mut self,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        u8_reg: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back. // assert_eq!(u8_reg.ty().underlying().total_size.0, 1);
        let bytes = u16_to_u8_pair(offset.0);
        self.state.add_instruction(
            OpCode::St8UsingPtrWithOffset,
            &[base_ptr_reg.addressing(), bytes.0, bytes.1, u8_reg.index],
            node,
            comment,
        );
    }

    pub fn add_mov_16_immediate_value(
        &mut self,
        dst_offset: &TypedRegister,
        value: u16,
        node: &Node,
        comment: &str,
    ) {
        let bytes = Self::u16_to_octets(value);

        self.state.add_instruction(
            OpCode::Mov16FromImmediateValue,
            &[dst_offset.addressing(), bytes.0, bytes.1],
            node,
            comment,
        );
    }

    pub fn add_mov_32_immediate_value(
        &mut self,
        dst_offset: &TypedRegister,
        value: u32,
        node: &Node,
        comment: &str,
    ) {
        let bytes = Self::convert_to_lower_and_upper(value);

        self.state.add_instruction(
            OpCode::Mov32FromImmediateValue,
            &[dst_offset.addressing(), bytes.0, bytes.1, bytes.2, bytes.3],
            node,
            comment,
        );
    }

    pub fn add_ld32_from_absolute_memory_address(
        &mut self,
        dst_reg: &TypedRegister,
        absolute_mem_addr: &HeapMemoryAddress,
        node: &Node,
        comment: &str,
    ) {
        let bytes = u32_to_bytes(absolute_mem_addr.0);

        self.state.add_instruction(
            OpCode::Ld32FromAbsoluteAddress,
            &[dst_reg.addressing(), bytes.0, bytes.1, bytes.2, bytes.3],
            node,
            comment,
        );
    }

    pub fn add_ld16_from_pointer_with_offset_u16(
        &mut self,
        dst_reg: &TypedRegister,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        node: &Node,
        comment: &str,
    ) {
        let bytes = u16_to_u8_pair(offset.0);

        self.state.add_instruction(
            OpCode::Ld16FromPointerWithOffset,
            &[
                dst_reg.addressing(),
                base_ptr_reg.addressing(),
                bytes.0,
                bytes.1,
            ],
            node,
            comment,
        );
    }

    pub fn add_ld32_from_pointer_with_offset_u16(
        &mut self,
        dst_reg: &TypedRegister,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        node: &Node,
        comment: &str,
    ) {
        let bytes = u16_to_u8_pair(offset.0);

        self.state.add_instruction(
            OpCode::Ld32FromPointerWithOffset,
            &[
                dst_reg.addressing(),
                base_ptr_reg.addressing(),
                bytes.0,
                bytes.1,
            ],
            node,
            comment,
        );
    }

    pub fn add_ld8_from_absolute_memory_address(
        &mut self,
        dst_reg: &TypedRegister,
        absolute_mem_addr: &HeapMemoryAddress,
        node: &Node,
        comment: &str,
    ) {
        let bytes = u32_to_bytes(absolute_mem_addr.0);

        self.state.add_instruction(
            OpCode::Ld8FromAbsoluteAddress,
            &[dst_reg.addressing(), bytes.0, bytes.1, bytes.2, bytes.3],
            node,
            comment,
        );
    }

    pub fn add_ld8_from_pointer_with_offset_u16(
        &mut self,
        dst_reg: &TypedRegister,
        base_ptr_reg: &TypedRegister,
        offset: MemoryOffset,
        node: &Node,
        comment: &str,
    ) {
        let bytes = u16_to_u8_pair(offset.0);

        self.state.add_instruction(
            OpCode::Ld8FromPointerWithOffset,
            &[
                dst_reg.addressing(),
                base_ptr_reg.addressing(),
                bytes.0,
                bytes.1,
            ],
            node,
            comment,
        );
    }

    pub fn add_mov_reg(
        &mut self,
        dst_offset: &TypedRegister,
        src_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        if dst_offset.index == src_offset.index {
            return;
        }
        self.state.add_instruction(
            OpCode::MovReg,
            &[dst_offset.addressing(), src_offset.addressing()],
            node,
            comment,
        );
    }

    pub fn add_mov8_immediate(
        &mut self,
        dst_offset: &TypedRegister,
        value: u8,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::Mov8FromImmediateValue,
            &[dst_offset.addressing(), value],
            node,
            comment,
        );
    }

    pub fn add_add_u32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back //assert!(dst_offset.ty().is_int());
        // TODO: Bring this back //assert!(lhs_offset.ty().is_int());
        // TODO: Bring this back //assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::AddU32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_add_u32_imm(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_immediate: u32,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back //assert!(dst_offset.ty().is_int());
        // TODO: Bring this back //assert!(lhs_offset.ty().is_int());
        // TODO: Bring this back //assert!(rhs_offset.ty().is_int());
        let immediate_bytes = u32_to_bytes(rhs_immediate);
        self.state.add_instruction(
            OpCode::AddU32Imm,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                immediate_bytes.0,
                immediate_bytes.1,
                immediate_bytes.2,
                immediate_bytes.3,
            ],
            node,
            comment,
        );
    }

    pub fn add_mod_i32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back // assert!(dst_offset.ty().is_int());
        // TODO: Bring this back // assert!(lhs_offset.ty().is_int());
        // TODO: Bring this back // assert!(rhs_offset.ty().is_int());

        self.state.add_instruction(
            OpCode::ModI32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_div_i32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back // assert!(dst_offset.ty().is_int());
        // TODO: Bring this back // assert!(lhs_offset.ty().is_int());
        // TODO: Bring this back // assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::DivI32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_sub_u32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back // assert!(dst_offset.ty().is_int());
        // TODO: Bring this back // assert!(lhs_offset.ty().is_int());
        // TODO: Bring this back // assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::SubU32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_mul_i32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back // assert!(dst_offset.ty().is_int());
        // TODO: Bring this back // assert!(lhs_offset.ty().is_int());
        // TODO: Bring this back // assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::MulU32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_neg_i32(
        &mut self,
        target: &TypedRegister,
        source: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back //assert!(target.ty().is_int());
        // TODO: Bring this back //assert!(source.ty().is_int());
        self.state.add_instruction(
            OpCode::NegI32,
            &[target.addressing(), source.addressing()],
            node,
            comment,
        );
    }

    pub fn add_mod_f32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_float());
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::ModF32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_sub_f32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: bring this back // assert!(dst_offset.ty().is_float());
        // TODO: bring this back //assert!(lhs_offset.ty().is_float());
        // TODO: bring this back //assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::SubU32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }
    pub fn add_mul_f32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: bring this back //assert!(dst_offset.ty().is_float());
        // TODO: bring this back //assert!(lhs_offset.ty().is_float());
        // TODO: bring this back //assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::MulF32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }
    pub fn add_div_f32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back //assert!(dst_offset.ty().is_float());
        // TODO: Bring this back //assert!(lhs_offset.ty().is_float());
        // TODO: Bring this back //assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::DivF32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_add_f32(
        &mut self,
        dst_offset: &TypedRegister,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: bring this back //assert!(dst_offset.ty().is_float());
        // TODO: bring this back //assert!(lhs_offset.ty().is_float());
        // TODO: bring this back //assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::AddU32,
            &[
                dst_offset.addressing(),
                lhs_offset.addressing(),
                rhs_offset.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_neg_f32(
        &mut self,
        target: &TypedRegister,
        source: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back // assert!(target.ty().is_float());
        // TODO: Bring this back // assert!(source.ty().is_float());
        self.state.add_instruction(
            OpCode::NegI32,
            &[target.addressing(), source.addressing()],
            node,
            comment,
        );
    }

    pub fn add_lt_i32(
        &mut self,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring Back //assert!(lhs_offset.ty().is_int());
        // TODO: Bring Back //assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::LtI32,
            &[lhs_offset.addressing(), rhs_offset.addressing()],
            node,
            comment,
        );
    }

    pub fn add_lt_u32(
        &mut self,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring Back //assert!(lhs_offset.ty().is_int());
        // TODO: Bring Back //assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::LtU32,
            &[lhs_offset.addressing(), rhs_offset.addressing()],
            node,
            comment,
        );
    }

    pub fn add_le_i32(
        &mut self,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back // assert!(lhs_offset.ty().is_int());
        // TODO: Bring this back // assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::LeI32,
            &[lhs_offset.addressing(), rhs_offset.addressing()],
            node,
            comment,
        );
    }

    pub fn add_gt_i32(
        &mut self,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back. //assert!(lhs_offset.ty().is_int());
        // TODO: Bring this back. //assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::GtI32,
            &[lhs_offset.addressing(), rhs_offset.addressing()],
            node,
            comment,
        );
    }

    pub fn add_ge_i32(
        &mut self,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: bring this back //assert!(lhs_offset.ty().is_int());
        // TODO: bring this back //assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::GeI32,
            &[lhs_offset.addressing(), rhs_offset.addressing()],
            node,
            comment,
        );
    }

    pub fn add_ge_u32(
        &mut self,
        lhs_offset: &TypedRegister,
        rhs_offset: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: bring this back //assert!(lhs_offset.ty().is_int());
        // TODO: bring this back //assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::GeU32,
            &[lhs_offset.addressing(), rhs_offset.addressing()],
            node,
            comment,
        );
    }

    pub fn add_tst_u8(&mut self, addr: &TypedRegister, node: &Node, comment: &str) {
        self.state.add_instruction(
            OpCode::MovToTFlagFromReg,
            &[addr.addressing()],
            node,
            comment,
        );
    }

    pub fn add_stz(&mut self, target: &TypedRegister, node: &Node, comment: &str) {
        //assert_eq!(target.underlying().total_size.0, 1);
        self.state.add_instruction(
            OpCode::MovFromTFlagToReg,
            &[target.addressing()],
            node,
            comment,
        );
    }

    pub fn add_stnz(&mut self, target: &TypedRegister, node: &Node, comment: &str) {
        // assert_eq!(target.underlying().total_size.0, 1);
        self.state.add_instruction(
            OpCode::MovFromNotTFlagToReg,
            &[target.addressing()],
            node,
            comment,
        );
    }

    pub fn add_trap(&mut self, trap_code: u8, node: &Node, comment: &str) {
        self.state
            .add_instruction(OpCode::Trap, &[trap_code], node, comment);
    }

    pub fn add_cmp_reg(
        &mut self,
        source_a: &TypedRegister,
        source_b: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::CmpReg,
            &[source_a.addressing(), source_b.addressing()],
            node,
            comment,
        );
    }

    pub fn add_block_cmp(
        &mut self,
        first_ptr: &TypedRegister,
        second_ptr: &TypedRegister,
        size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        let bytes = u16_to_u8_pair(size.0);
        self.state.add_instruction(
            OpCode::CmpBlock,
            &[
                first_ptr.addressing(),
                second_ptr.addressing(),
                bytes.0,
                bytes.1,
            ],
            node,
            comment,
        );
    }

    // Collection specific
    pub fn add_map_new_from_slice(
        &mut self,
        map_target_addr: &TypedRegister,
        slice_source_addr: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        //assert_ne!(key_size.0, 0); // TODO: Bring this back
        //assert_ne!(value_size.0, 0); // TODO: Bring this back
        // assert_ne!(element_size.0, 0); // TODO: Bring this back

        self.state.add_instruction(
            OpCode::MapNewFromPairs,
            &[map_target_addr.addressing(), slice_source_addr.addressing()],
            node,
            comment,
        );
    }

    pub fn add_map_has(
        &mut self,
        self_addr: &TypedRegister,
        key_addr: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_addr.ty().kind, BasicTypeKind::InternalMapPointer(_, _));
        self.state.add_instruction(
            OpCode::MapHas,
            &[self_addr.addressing(), key_addr.addressing()],
            node,
            comment,
        );
    }

    pub fn add_map_remove(
        &mut self,
        self_addr: &TypedRegister,
        key_addr: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_addr.ty().kind, BasicTypeKind::InternalMapPointer(_, _));
        self.state.add_instruction(
            OpCode::MapRemove,
            &[self_addr.addressing(), key_addr.addressing()],
            node,
            comment,
        );
    }

    pub fn add_map_fetch(
        &mut self,
        target_addr: &TypedRegister,
        self_addr: &TypedRegister,
        key: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_addr.ty().kind, BasicTypeKind::InternalMapPointer(_, _));
        self.state.add_instruction(
            OpCode::MapFetch,
            &[
                target_addr.addressing(),
                self_addr.addressing(),
                key.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_map_set(
        &mut self,
        self_addr: &TypedRegister,
        key: &TypedRegister,
        value: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_addr.ty().kind, BasicTypeKind::InternalMapPointer(_, _));

        self.state.add_instruction(
            OpCode::MapSet,
            &[self_addr.addressing(), key.addressing(), value.addressing()],
            node,
            comment,
        );
    }

    /*
    pub fn add_ld_u16(&mut self, dest: &TypedRegister, data: u16, node: &Node, comment: &str) {
        self.state
            .add_instruction(OpCode::Ld16, &[dest.addressing(), data], node, comment);
    }

     */

    pub fn add_alloc(
        &mut self,
        target_ptr_reg: &TypedRegister,
        size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        /* TODO: Bring this back assert!(matches!(
            target_ptr_reg.ty().kind,
            BasicTypeKind::MutablePointer(_)
        ));

         */
        // TODO: Bring this back //assert_eq!(target_ptr_reg.ty().total_size, HEAP_PTR_ON_FRAME_SIZE);
        // assert_ne!(size.0, 0); TODO: Bring this back
        let size_bytes = Self::u16_to_octets(size.0);
        self.state.add_instruction(
            OpCode::Alloc,
            &[target_ptr_reg.addressing(), size_bytes.0, size_bytes.1],
            node,
            comment,
        );
    }

    pub fn add_int_rnd(
        &mut self,
        dest: &TypedRegister,
        self_int: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_int());
        assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntToRnd,
            &[dest.addressing(), self_int.addressing()],
            node,
            comment,
        );
    }

    pub fn add_int_min(
        &mut self,
        dest: &TypedRegister,
        self_int: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back //assert!(dest.ty().is_int());
        // TODO: Bring this back //assert!(self_int.ty().is_int());

        self.state.add_instruction(
            OpCode::IntMin,
            &[dest.addressing(), self_int.addressing()],
            node,
            comment,
        );
    }

    pub fn add_int_max(
        &mut self,
        dest: &TypedRegister,
        self_int: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back //assert!(dest.ty().is_int());
        // TODO: Bring this back //assert!(self_int.ty().is_int());

        self.state.add_instruction(
            OpCode::IntMax,
            &[dest.addressing(), self_int.addressing()],
            node,
            comment,
        );
    }

    pub fn add_int_clamp(
        &mut self,
        dest: &TypedRegister,
        self_int: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_int());
        assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntClamp,
            &[dest.addressing(), self_int.addressing()],
            node,
            comment,
        );
    }

    pub fn add_int_abs(
        &mut self,
        dest: &TypedRegister,
        self_int: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: Bring this back //  assert!(dest.ty().is_int());
        // TODO: Bring this back //  assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntAbs,
            &[dest.addressing(), self_int.addressing()],
            node,
            comment,
        );
    }

    pub fn add_int_to_float(
        &mut self,
        dest: &TypedRegister,
        self_int: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: bring this back //assert!(dest.ty().is_float());
        // TODO: bring this back //assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntToFloat,
            &[dest.addressing(), self_int.addressing()],
            node,
            comment,
        );
    }

    pub fn add_int_to_string(
        &mut self,
        dest: &TypedRegister,
        self_int: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_str());
        assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntToString,
            &[dest.addressing(), self_int.addressing()],
            node,
            comment,
        );
    }

    pub fn bool_to_string(
        &mut self,
        dest_str: &TypedRegister,
        self_bool: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_str.ty().is_str());
        assert!(self_bool.ty().is_bool());
        self.state.add_instruction(
            OpCode::BoolToString,
            &[dest_str.addressing(), self_bool.addressing()],
            node,
            comment,
        );
    }

    pub fn float_to_string(
        &mut self,
        dest_str: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_str.ty().is_str());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatToString,
            &[dest_str.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_round(
        &mut self,
        dest_int: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_int.ty().is_int());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatRound,
            &[dest_int.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_floor(
        &mut self,
        dest_int: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: bring this back //assert!(dest_int.ty().is_int());
        // TODO: bring this back //assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatFloor,
            &[dest_int.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_sqrt(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        // TODO: bring this back //assert!(dest_float.ty().is_float());
        // TODO: bring this back //assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatSqrt,
            &[dest_float.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_sign(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatSign,
            &[dest_float.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_abs(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatAbs,
            &[dest_float.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_prnd(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatPseudoRandom,
            &[dest_float.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_sin(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatSin,
            &[dest_float.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_cos(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatCos,
            &[dest_float.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_acos(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatAcos,
            &[dest_float.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_asin(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatAsin,
            &[dest_float.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_atan2(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatAtan2,
            &[dest_float.addressing(), self_float.addressing()],
            node,
            comment,
        );
    }

    pub fn add_float_min(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        other: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatMin,
            &[
                dest_float.addressing(),
                self_float.addressing(),
                other.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_float_max(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        max_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        assert!(max_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatMax,
            &[
                dest_float.addressing(),
                self_float.addressing(),
                max_float.addressing(),
            ],
            node,
            comment,
        );
    }

    pub fn add_float_clamp(
        &mut self,
        dest_float: &TypedRegister,
        self_float: &TypedRegister,
        min_float: &TypedRegister,
        max_float: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        assert!(min_float.ty().is_float());
        assert!(max_float.ty().is_float());

        self.state.add_instruction(
            OpCode::FloatClamp,
            &[
                dest_float.addressing(),
                min_float.addressing(),
                self_float.addressing(),
                max_float.addressing(),
            ],
            node,
            comment,
        );
    }
}
