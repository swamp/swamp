/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use source_map_node::Node;
use std::ops::Deref;
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, CompleteFunctionInfo, FramePlacedType, FunctionInfo,
    FunctionInfoKind, HeapPlacedType, OffsetMemoryItem,
};
use swamp_vm_types::{
    BinaryInstruction, FrameMemoryAddress, FrameMemorySize, HEAP_PTR_ON_FRAME_SIZE,
    HeapMemoryAddress, HeapMemoryOffset, InstructionPosition, InstructionPositionOffset,
    MemorySize, Meta, RANGE_HEADER_SIZE, RANGE_ITERATOR_SIZE, ZFlagPolarity,
};
use tracing::info;

#[derive(Debug)]
pub struct PatchPosition(pub InstructionPosition);

pub struct InstructionBuilderState {
    pub instructions: Vec<BinaryInstruction>,
    pub functions: SeqMap<usize, CompleteFunctionInfo>,
    pub constants: SeqMap<usize, CompleteFunctionInfo>,
    pub meta: Vec<Meta>,
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
            functions: SeqMap::default(),
            constants: SeqMap::default(),
            meta: Vec::new(),
        }
    }

    #[must_use]
    pub fn position(&self) -> InstructionPosition {
        InstructionPosition(self.instructions.len() as u16)
    }
    // Could be a constant function
    // should probably be called function_like
    pub fn add_function(
        &mut self,
        function_info: FunctionInfo,
        node: &Node,
        comment: &str,
    ) -> InstructionBuilder {
        self.add_function_helper(function_info, node, comment)
    }

    pub fn add_function_helper(
        &mut self,
        function_info: FunctionInfo,
        node: &Node,
        comment: &str,
    ) -> InstructionBuilder {
        let complete_info = CompleteFunctionInfo {
            ip: self.position(),
            size: InstructionPositionOffset(0),
            info: function_info.clone(),
        };
        if let FunctionInfoKind::Constant(constant_id) = function_info.kind {
            self.constants.insert(constant_id, complete_info).unwrap();
        } else if let FunctionInfoKind::Normal(normal_id) = function_info.kind {
            if self.functions.contains_key(&normal_id) {
                // TODO: improve monomorph
                info!("skipping");
            } else {
                self.functions.insert(normal_id, complete_info).unwrap();
            }
        }

        let mut function_instruction_builder = InstructionBuilder::new(self);

        function_instruction_builder.enter(function_info.frame_memory.size(), node, comment);

        function_instruction_builder
    }

    pub fn add_constant_function(
        &mut self,
        constant_info: FunctionInfo,
        node: &Node,
        comment: &str,
    ) -> InstructionBuilder {
        let FunctionInfoKind::Constant(_id) = constant_info.kind else {
            panic!("must be constant")
        };

        self.add_function_helper(constant_info, node, comment)
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

    pub fn add_ret(&mut self, node: &Node, comment: &str) {
        self.add_instruction(OpCode::Ret, &[], node, comment);
    }

    pub fn add_hlt(&mut self, node: &Node, comment: &str) {
        self.add_instruction(OpCode::Hlt, &[], node, comment);
    }

    fn add_instruction(&mut self, op_code: OpCode, operands: &[u16], node: &Node, comment: &str) {
        let mut array: [u16; 5] = [0; 5];
        assert!(operands.len() <= 5);
        let len = operands.len();
        array[..len].copy_from_slice(&operands[..len]);
        self.instructions.push(BinaryInstruction {
            opcode: op_code as u8,
            padding: 0,
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
}

impl<'a> InstructionBuilder<'a> {
    #[must_use]
    pub const fn new(state: &'a mut InstructionBuilderState) -> Self {
        Self { state }
    }
}

impl InstructionBuilder<'_> {
    pub fn add_not_z(&mut self, node: &Node, comment: &str) {
        self.state.add_instruction(OpCode::NotZ, &[], node, comment);
    }
    pub fn enter(&mut self, size: FrameMemorySize, node: &Node, comment: &str) {
        self.state
            .add_instruction(OpCode::Enter, &[size.0], node, comment);
    }

    #[must_use]
    pub fn position(&self) -> InstructionPosition {
        InstructionPosition(self.state.instructions.len() as u16)
    }

    pub fn add_jmp_if_equal_placeholder(&mut self, node: &Node, comment: &str) -> PatchPosition {
        let position = self.position();

        self.state.add_instruction(OpCode::Bz, &[0], node, comment);

        PatchPosition(position)
    }

    pub fn add_jmp_if_not_equal_placeholder(
        &mut self,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();

        self.state.add_instruction(OpCode::Bnz, &[0], node, comment);

        PatchPosition(position)
    }

    pub fn add_jmp_if_not_equal_polarity_placeholder(
        &mut self,
        polarity: &ZFlagPolarity,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        match polarity {
            ZFlagPolarity::Normal => self.add_jmp_if_not_equal_placeholder(node, comment),
            ZFlagPolarity::Inverted => self.add_jmp_if_equal_placeholder(node, comment),
        }
    }

    pub fn add_unwrap_jmp_some_placeholder(
        &mut self,
        target: &FramePlacedType,
        check_optional: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();

        self.state.add_instruction(
            OpCode::UnwrapJmpSome,
            &[target.addr().0, check_optional.addr().0, 0],
            node,
            comment,
        );

        PatchPosition(position)
    }

    // If the value at option_offset is Some, unwrap and write to result_offset, then continue.
    // If it is None, write the value at none_value_offset to result_offset, and jump to jump_ip (the end of the chain).
    pub fn add_unwrap_jmp_none_placeholder(
        &mut self,
        target: &FramePlacedType,
        check_optional: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();

        self.state.add_instruction(
            OpCode::UnwrapJmpNone,
            &[target.addr().0, check_optional.addr().0, 0],
            node,
            comment,
        );

        PatchPosition(position)
    }

    pub fn add_vec_swap(
        &mut self,
        vec_self_addr: &FramePlacedType,
        int_index_a: &FramePlacedType,
        int_index_b: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            vec_self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));
        assert_eq!(int_index_a.size(), int_index_b.size());
        self.state.add_instruction(
            OpCode::VecSwap,
            &[
                vec_self_addr.addr().0,
                int_index_a.addr().0,
                int_index_b.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_subscript(
        &mut self,
        target: &FramePlacedType,
        self_addr: &FramePlacedType,
        index: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));
        self.state.add_instruction(
            OpCode::VecGet,
            &[target.addr().0, self_addr.addr().0, index.addr().0],
            node,
            comment,
        );
    }

    pub fn add_vec_get(
        &mut self,
        target: &FramePlacedType,
        self_addr: &FramePlacedType,
        index: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));
        self.state.add_instruction(
            OpCode::VecGet,
            &[target.addr().0, self_addr.addr().0, index.addr().0],
            node,
            comment,
        );
    }

    pub fn add_vec_get_range(
        &mut self,
        target: &FramePlacedType,
        vec_self_addr: &FramePlacedType,
        range_header: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            vec_self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));

        self.state.add_instruction(
            OpCode::VecGetRange,
            &[
                target.addr().0,
                vec_self_addr.addr().0,
                range_header.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_set(
        &mut self,
        self_addr: &FramePlacedType,
        index: &FramePlacedType,
        value_addr: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));

        self.state.add_instruction(
            OpCode::VecSet,
            &[self_addr.addr().0, index.addr().0, value_addr.addr().0],
            node,
            comment,
        );
    }

    pub fn add_vec_len(
        &mut self,
        target: &FramePlacedType,
        self_vec: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_vec.ty().kind, BasicTypeKind::InternalVecPointer(_));
        self.state.add_instruction(
            OpCode::VecLen,
            &[target.addr().0, self_vec.addr().0],
            node,
            comment,
        );
    }

    pub fn add_vec_push(
        &mut self,
        self_addr: &FramePlacedType,
        element_item: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(
            matches!(
                self_addr.underlying().kind,
                BasicTypeKind::InternalVecPointer(_)
            ),
            "what is this {:?}",
            self_addr.ty()
        );

        self.state.add_instruction(
            OpCode::VecPush,
            &[self_addr.addr().0, element_item.addr().0],
            node,
            comment,
        );
    }

    pub fn add_vec_pop(
        &mut self,
        target_addr: &FramePlacedType,
        self_addr: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));
        self.state.add_instruction(
            OpCode::VecPop,
            &[target_addr.addr().0, self_addr.addr().0],
            node,
            comment,
        );
    }

    pub fn add_vec_remove_index(
        &mut self,
        self_addr: &FramePlacedType,
        element_item: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));
        self.state.add_instruction(
            OpCode::VecRemoveIndex,
            &[self_addr.addr().0, element_item.addr().0],
            node,
            comment,
        );
    }

    pub fn add_vec_remove_index_get_value(
        &mut self,
        target_addr: &FramePlacedType,
        self_addr: &FramePlacedType,
        element_item: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(
            self_addr.ty().kind,
            BasicTypeKind::InternalVecPointer(_)
        ));
        self.state.add_instruction(
            OpCode::VecRemoveIndexGetValue,
            &[
                target_addr.addr().0,
                self_addr.addr().0,
                element_item.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_iter_next_placeholder(
        &mut self,
        iterator_target: &FramePlacedType,
        closure_variable: &FramePlacedType,
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
            &[iterator_target.addr().0, closure_variable.addr().0, 0],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_vec_iter_next_pair_placeholder(
        &mut self,
        iterator_target: &FramePlacedType,
        closure_variable: &FramePlacedType,
        closure_variable_b: &FramePlacedType,
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
                iterator_target.addr().0,
                closure_variable.addr().0,
                closure_variable_b.addr().0,
                0,
            ],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_eq_u8_immediate(
        &mut self,
        source_addr: &FramePlacedType,
        immediate: u8,
        node: &Node,
        comment: &str,
    ) {
        assert!(source_addr.size().0 >= 1);
        self.state.add_instruction(
            OpCode::Eq8Imm,
            &[source_addr.addr().0, immediate as u16],
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

        self.state.add_instruction(OpCode::Jmp, &[0], node, comment);

        PatchPosition(position)
    }

    pub fn add_lea(
        &mut self,
        target_heap: &FramePlacedType,
        frame_address_to_convert: FrameMemoryAddress,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::Lea,
            &[target_heap.addr().0, frame_address_to_convert.0],
            node,
            comment,
        );
    }

    // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
    pub fn add_mov(
        &mut self,
        target: &FramePlacedType,
        source: &FramePlacedType,
        size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        assert_ne!(size.0, 0);
        self.state.add_instruction(
            OpCode::Mov,
            &[target.addr().0, source.addr().0, size.0],
            node,
            comment,
        );
    }

    // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
    pub fn add_mov_for_assignment(
        &mut self,
        target: &FramePlacedType,
        source: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert_eq!(
            target.underlying().total_size,
            source.underlying().total_size,
            "problem with move {target:?} {source:?}"
        );
        self.add_mov(target, source, source.size(), node, comment);
    }

    // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
    pub fn add_mov_mem(
        &mut self,
        target: &FramePlacedType,
        source: HeapMemoryAddress,
        size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        assert_ne!(size.0, 0);

        let (lower_bits, upper_bits) = Self::convert_to_lower_and_upper(source.0);
        self.state.add_instruction(
            OpCode::MovMem,
            &[target.addr().0, lower_bits, upper_bits, size.0],
            node,
            comment,
        );
    }

    // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
    pub fn add_mov_mem_for_assignment(
        &mut self,
        target: &FramePlacedType,
        source: &HeapPlacedType,
        node: &Node,
        comment: &str,
    ) {
        self.add_mov_mem(target, source.addr(), source.size(), node, comment);
    }

    pub fn add_panic(&mut self, str: &FramePlacedType, node: &Node, comment: &str) {
        self.state
            .add_instruction(OpCode::Panic, &[str.addr().0], node, comment);
    }

    pub fn add_call(&mut self, function_ip: &InstructionPosition, node: &Node, comment: &str) {
        self.state
            .add_instruction(OpCode::Call, &[function_ip.0], node, comment);
    }

    pub fn add_host_call(
        &mut self,
        host_function_id: u16,
        arguments_size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::HostCall,
            &[host_function_id, arguments_size.0],
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
        const JMP_IF_NOT: u8 = OpCode::Bz as u8;
        const JMP_IF: u8 = OpCode::Bnz as u8;
        const JMP: u8 = OpCode::Jmp as u8;

        const VEC_ITER_NEXT: u8 = OpCode::VecIterNext as u8;
        const VEC_ITER_NEXT_PAIR: u8 = OpCode::VecIterNextPair as u8;
        const MAP_ITER_NEXT: u8 = OpCode::MapIterNext as u8;
        const MAP_ITER_NEXT_PAIR: u8 = OpCode::MapIterNextPair as u8;

        const RANGE_ITER_NEXT: u8 = OpCode::RangeIterNext as u8;

        const UNWRAP_JMP_NONE: u8 = OpCode::UnwrapJmpNone as u8;
        const UNWRAP_JMP_SOME: u8 = OpCode::UnwrapJmpSome as u8;

        let instruction = &mut self.state.instructions[patch_position.0.0 as usize];

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

            UNWRAP_JMP_NONE => {
                instruction.operands[2] = target_position.0 as u16 - 1;
            }
            UNWRAP_JMP_SOME => {
                instruction.operands[2] = target_position.0 as u16 - 1;
            }

            _ => panic!("Attempted to patch a non-jump instruction at position {patch_position:?}"),
        }
    }

    // It takes ownership of the patch position
    pub fn patch_jump_here(&mut self, jump_position: PatchPosition) {
        self.patch_jump(jump_position, &self.position());
    }

    pub fn add_jmp(&mut self, ip: InstructionPosition, node: &Node, comment: &str) {
        self.state
            .add_instruction(OpCode::Jmp, &[ip.0 - 1], node, comment);
    }

    // Slices

    pub fn add_slice_from_heap(
        &mut self,
        slice_dst: &FramePlacedType,
        heap_region: FrameMemoryAddress,
        value_type: &BasicType,
        element_count: u16,
        node: &Node,
        comment: &str,
    ) {
        assert_ne!(slice_dst.size().0, 0);
        assert_ne!(value_type.total_size.0, 0);
        self.state.add_instruction(
            OpCode::SliceFromHeap,
            &[
                slice_dst.addr().0,
                heap_region.0,
                value_type.total_size.0,
                element_count,
            ],
            node,
            comment,
        );
    }

    pub fn add_slice_pair_from_heap(
        &mut self,
        slice_dst: &FramePlacedType,
        heap_region: FrameMemoryAddress,
        key_type: &BasicType,
        value_type: &BasicType,
        element_count: u16,
        node: &Node,
        comment: &str,
    ) {
        assert_ne!(slice_dst.size().0, 0);
        assert_ne!(key_type.total_size.0, 0);
        assert_ne!(value_type.total_size.0, 0);

        self.state.add_instruction(
            OpCode::SlicePairFromHeap,
            &[
                slice_dst.addr().0,
                heap_region.0,
                key_type.total_size.0,
                value_type.total_size.0,
                element_count,
            ],
            node,
            comment,
        );
    }

    pub fn add_map_iter_init(
        &mut self,
        iterator_target: &FramePlacedType,
        pointer_to_map_header: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::MapIterInit,
            &[iterator_target.addr().0, pointer_to_map_header.addr().0],
            node,
            comment,
        );
    }

    pub fn add_map_iter_next_placeholder(
        &mut self,
        iterator_target: &FramePlacedType,
        closure_variable: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.state.add_instruction(
            OpCode::MapIterNext,
            &[iterator_target.addr().0, closure_variable.addr().0, 0],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_map_iter_next_pair_placeholder(
        &mut self,
        iterator_target: &FramePlacedType,
        closure_variable: &FramePlacedType,
        closure_variable_b: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.state.add_instruction(
            OpCode::MapIterNextPair,
            &[
                iterator_target.addr().0,
                closure_variable.addr().0,
                closure_variable_b.addr().0,
                0,
            ],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_range_iter_next_placeholder(
        &mut self,
        iterator_target: &FramePlacedType,
        closure_variable: &FramePlacedType,
        node: &Node,

        comment: &str,
    ) -> PatchPosition {
        let position = self.position();
        self.state.add_instruction(
            OpCode::RangeIterNext,
            &[iterator_target.addr().0, closure_variable.addr().0, 0],
            node,
            comment,
        );
        PatchPosition(position)
    }

    pub fn add_string_append(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::StringAppend,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_clear(&mut self, mut_self_addr: &FramePlacedType, node: &Node, comment: &str) {
        self.state
            .add_instruction(OpCode::VecClear, &[mut_self_addr.addr().0], node, comment);
    }

    pub fn add_vec_create(
        &mut self,
        mut_self_addr: &FramePlacedType,
        element_byte_size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        // assert_ne!(element_byte_size.0, 0); // TODO: Bring this back
        self.state.add_instruction(
            OpCode::VecCreate,
            &[mut_self_addr.addr().0, element_byte_size.0],
            node,
            comment,
        );
    }

    pub fn add_vec_from_slice(
        &mut self,
        target: &FramePlacedType,
        source_slice_header: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::VecFromSlice,
            &[target.addr().0, source_slice_header.addr().0],
            node,
            comment,
        );
    }

    pub fn add_range_iter_init(
        &mut self,
        iterator_target: &FramePlacedType,
        range_source_header: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert_eq!(iterator_target.size(), RANGE_ITERATOR_SIZE);
        assert_eq!(range_source_header.size(), RANGE_HEADER_SIZE);

        self.state.add_instruction(
            OpCode::RangeIterInit,
            &[iterator_target.addr().0, range_source_header.addr().0],
            node,
            comment,
        );
    }

    pub fn add_vec_iter_init(
        &mut self,
        iterator_target: &FramePlacedType,
        pointer_to_vec_header: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::VecIterInit,
            &[iterator_target.addr().0, pointer_to_vec_header.addr().0],
            node,
            comment,
        );
    }

    pub fn add_vec_iter_next(
        &mut self,
        iterator_target: &FramePlacedType,
        closure_variable: &FramePlacedType,
        instruction_position: InstructionPosition,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::VecIterNext,
            &[
                iterator_target.addr().0,
                closure_variable.addr().0,
                instruction_position.0,
            ],
            node,
            comment,
        );
    }

    pub fn add_vec_iter_next_pair(
        &mut self,
        iterator_target: &FramePlacedType,
        closure_variable_key: &FramePlacedType,
        closure_variable_value: &FramePlacedType,
        instruction_position: InstructionPosition,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::VecIterNextPair,
            &[
                iterator_target.addr().0,
                closure_variable_key.addr().0,
                closure_variable_value.addr().0,
                instruction_position.0,
            ],
            node,
            comment,
        );
    }

    const fn convert_to_lower_and_upper(data: u32) -> (u16, u16) {
        let lower_bits = (data & 0xFFFF) as u16;
        let upper_bits = (data >> 16) as u16;

        (lower_bits, upper_bits)
    }

    pub fn add_ldi32(
        &mut self,
        dst_offset: &FramePlacedType,
        value: i32,
        node: &Node,
        comment: &str,
    ) {
        let (lower_bits, upper_bits) = Self::convert_to_lower_and_upper(value as u32);

        self.state.add_instruction(
            OpCode::Ld32,
            &[dst_offset.addr().0, lower_bits, upper_bits],
            node,
            comment,
        );
    }

    pub fn add_ld32(
        &mut self,
        dst_offset: &FramePlacedType,
        value: u32,
        node: &Node,
        comment: &str,
    ) {
        let (lower_bits, upper_bits) = Self::convert_to_lower_and_upper(value);

        self.state.add_instruction(
            OpCode::Ld32,
            &[dst_offset.addr().0, lower_bits, upper_bits],
            node,
            comment,
        );
    }

    pub fn add_mov32(
        &mut self,
        dst_offset: &FramePlacedType,
        src_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        self.state.add_instruction(
            OpCode::Mov32,
            &[dst_offset.addr().0, src_offset.addr().0],
            node,
            comment,
        );
    }

    pub fn add_ld8(&mut self, dst_offset: &FramePlacedType, value: u8, node: &Node, comment: &str) {
        self.state.add_instruction(
            OpCode::Ld8,
            &[dst_offset.addr().0, value as u16],
            node,
            comment,
        );
    }

    pub fn add_add_i32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_int());
        assert!(lhs_offset.ty().is_int());
        assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::AddI32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_mod_i32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_int());
        assert!(lhs_offset.ty().is_int());
        assert!(rhs_offset.ty().is_int());

        self.state.add_instruction(
            OpCode::ModI32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_div_i32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_int());
        assert!(lhs_offset.ty().is_int());
        assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::DivI32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_sub_i32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_int());
        assert!(lhs_offset.ty().is_int());
        assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::SubI32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_mul_i32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_int());
        assert!(lhs_offset.ty().is_int());
        assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::MulI32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_neg_i32(
        &mut self,
        target: &FramePlacedType,
        source: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(target.ty().is_int());
        assert!(source.ty().is_int());
        self.state.add_instruction(
            OpCode::NegI32,
            &[target.addr().0, source.addr().0],
            node,
            comment,
        );
    }

    pub fn add_mod_f32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_float());
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::ModF32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_sub_f32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_float());
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::SubF32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }
    pub fn add_mul_f32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_float());
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::MulF32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }
    pub fn add_div_f32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_float());
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::DivF32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_add_f32(
        &mut self,
        dst_offset: &FramePlacedType,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dst_offset.ty().is_float());
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::AddF32,
            &[
                dst_offset.addr().0,
                lhs_offset.addr().0,
                rhs_offset.addr().0,
            ],
            node,
            comment,
        );
    }

    pub fn add_neg_f32(
        &mut self,
        target: &FramePlacedType,
        source: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(target.ty().is_float());
        assert!(source.ty().is_float());
        self.state.add_instruction(
            OpCode::NegF32,
            &[target.addr().0, source.addr().0],
            node,
            comment,
        );
    }

    pub fn add_lt_f32(
        &mut self,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());
        self.state.add_instruction(
            OpCode::LtF32,
            &[lhs_offset.addr().0, rhs_offset.addr().0],
            node,
            comment,
        );
    }

    pub fn add_le_f32(
        &mut self,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());

        self.state.add_instruction(
            OpCode::LeF32,
            &[lhs_offset.addr().0, rhs_offset.addr().0],
            node,
            comment,
        );
    }

    pub fn add_gt_f32(
        &mut self,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());

        self.state.add_instruction(
            OpCode::GtF32,
            &[lhs_offset.addr().0, rhs_offset.addr().0],
            node,
            comment,
        );
    }

    pub fn add_ge_f32(
        &mut self,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(lhs_offset.ty().is_float());
        assert!(rhs_offset.ty().is_float());

        self.state.add_instruction(
            OpCode::GeF32,
            &[lhs_offset.addr().0, rhs_offset.addr().0],
            node,
            comment,
        );
    }

    pub fn add_lt_i32(
        &mut self,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(lhs_offset.ty().is_int());
        assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::LtI32,
            &[lhs_offset.addr().0, rhs_offset.addr().0],
            node,
            comment,
        );
    }

    pub fn add_le_i32(
        &mut self,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(lhs_offset.ty().is_int());
        assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::LeI32,
            &[lhs_offset.addr().0, rhs_offset.addr().0],
            node,
            comment,
        );
    }

    pub fn add_gt_i32(
        &mut self,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(lhs_offset.ty().is_int());
        assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::GtI32,
            &[lhs_offset.addr().0, rhs_offset.addr().0],
            node,
            comment,
        );
    }

    pub fn add_ge_i32(
        &mut self,
        lhs_offset: &FramePlacedType,
        rhs_offset: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(lhs_offset.ty().is_int());
        assert!(rhs_offset.ty().is_int());
        self.state.add_instruction(
            OpCode::GeI32,
            &[lhs_offset.addr().0, rhs_offset.addr().0],
            node,
            comment,
        );
    }

    pub fn add_tst8(&mut self, addr: &FramePlacedType, node: &Node, comment: &str) {
        assert!(addr.size().0 >= 1);
        self.state
            .add_instruction(OpCode::Tst8, &[addr.addr().0], node, comment);
    }

    pub fn add_stz(&mut self, target: &FramePlacedType, node: &Node, comment: &str) {
        assert_eq!(target.underlying().total_size.0, 1);
        self.state
            .add_instruction(OpCode::Stz, &[target.addr().0], node, comment);
    }

    pub fn add_stnz(&mut self, target: &FramePlacedType, node: &Node, comment: &str) {
        assert_eq!(target.underlying().total_size.0, 1);
        self.state
            .add_instruction(OpCode::Stnz, &[target.addr().0], node, comment);
    }

    pub fn add_cmp8(
        &mut self,
        a: &FramePlacedType,
        b: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert_eq!(a.underlying().total_size.0, 1);
        assert_eq!(b.underlying().total_size.0, 1);
        self.state
            .add_instruction(OpCode::Cmp8, &[a.addr().0, b.addr().0], node, comment);
    }

    pub fn add_cmp32(
        &mut self,
        a: &FramePlacedType,
        b: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert_eq!(a.underlying().total_size.0, 4);
        assert_eq!(b.underlying().total_size.0, 4);

        self.state
            .add_instruction(OpCode::Cmp32, &[a.addr().0, b.addr().0], node, comment);
    }

    pub fn add_cmp(
        &mut self,
        source_a: &FramePlacedType,
        source_b: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert_eq!(source_a.size(), source_b.size());
        self.state.add_instruction(
            OpCode::Cmp,
            &[source_a.addr().0, source_b.addr().0, source_a.size().0],
            node,
            comment,
        );
    }

    // Collection specific
    pub fn add_map_new_from_slice(
        &mut self,
        map_target_addr: &FramePlacedType,
        slice_source_addr: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        //assert_ne!(key_size.0, 0); // TODO: Bring this back
        //assert_ne!(value_size.0, 0); // TODO: Bring this back
        // assert_ne!(element_size.0, 0); // TODO: Bring this back

        self.state.add_instruction(
            OpCode::MapNewFromPairs,
            &[map_target_addr.addr().0, slice_source_addr.addr().0],
            node,
            comment,
        );
    }

    pub fn add_map_has(
        &mut self,
        self_addr: &FramePlacedType,
        key_addr: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_addr.ty().kind, BasicTypeKind::InternalMapPointer(_, _));
        self.state.add_instruction(
            OpCode::MapHas,
            &[self_addr.addr().0, key_addr.addr().0],
            node,
            comment,
        );
    }

    pub fn add_map_len(
        &mut self,
        target: &FramePlacedType,
        self_addr: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_addr.ty().kind, BasicTypeKind::InternalMapPointer(_, _));
        self.state.add_instruction(
            OpCode::MapLen,
            &[target.addr().0, self_addr.addr().0],
            node,
            comment,
        );
    }

    pub fn add_map_remove(
        &mut self,
        self_addr: &FramePlacedType,
        key_addr: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_addr.ty().kind, BasicTypeKind::InternalMapPointer(_, _));
        self.state.add_instruction(
            OpCode::MapRemove,
            &[self_addr.addr().0, key_addr.addr().0],
            node,
            comment,
        );
    }

    pub fn add_map_fetch(
        &mut self,
        target_addr: &FramePlacedType,
        self_addr: &FramePlacedType,
        key: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_addr.ty().kind, BasicTypeKind::InternalMapPointer(_, _));
        self.state.add_instruction(
            OpCode::MapFetch,
            &[target_addr.addr().0, self_addr.addr().0, key.addr().0],
            node,
            comment,
        );
    }

    pub fn add_map_set(
        &mut self,
        self_addr: &FramePlacedType,
        key: &FramePlacedType,
        value: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        matches!(self_addr.ty().kind, BasicTypeKind::InternalMapPointer(_, _));

        self.state.add_instruction(
            OpCode::MapSet,
            &[self_addr.addr().0, key.addr().0, value.addr().0],
            node,
            comment,
        );
    }

    pub fn add_ld_u16(&mut self, dest: &FramePlacedType, data: u16, node: &Node, comment: &str) {
        self.state
            .add_instruction(OpCode::Ld16, &[dest.addr().0, data], node, comment);
    }

    pub fn add_alloc(
        &mut self,
        target: &FramePlacedType,
        size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(target.ty().kind, BasicTypeKind::MutablePointer(_)));
        assert_eq!(target.ty().total_size, HEAP_PTR_ON_FRAME_SIZE);
        // assert_ne!(size.0, 0); TODO: Bring this back

        self.state
            .add_instruction(OpCode::Alloc, &[target.addr().0, size.0], node, comment);
    }

    pub fn add_stx(
        &mut self,
        dest: &FramePlacedType,
        offset: HeapMemoryOffset,
        source: &FramePlacedType,
        size: FrameMemorySize,
        node: &Node,
        comment: &str,
    ) {
        assert!(matches!(dest.ty().kind, BasicTypeKind::MutablePointer(_)));
        assert_eq!(dest.ty().total_size, HEAP_PTR_ON_FRAME_SIZE);
        assert_ne!(size.0, 0);

        let (offset_lower, offset_upper) = Self::convert_to_lower_and_upper(offset.0);
        self.state.add_instruction(
            OpCode::Stx,
            &[
                dest.addr().0,
                offset_lower,
                offset_upper,
                source.addr().0,
                size.0,
            ],
            node,
            comment,
        );
    }

    pub fn add_stx_for_assignment(
        &mut self,
        dest: &FramePlacedType,
        offset: HeapMemoryOffset,
        source: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        self.add_stx(
            dest,
            offset,
            source,
            FrameMemorySize(source.size().0),
            node,
            comment,
        );
    }

    pub fn add_int_rnd(
        &mut self,
        dest: &FramePlacedType,
        self_int: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_int());
        assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntToRnd,
            &[dest.addr().0, self_int.addr().0],
            node,
            comment,
        );
    }

    pub fn add_int_min(
        &mut self,
        dest: &FramePlacedType,
        self_int: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_int());
        assert!(self_int.ty().is_int());

        self.state.add_instruction(
            OpCode::IntMin,
            &[dest.addr().0, self_int.addr().0],
            node,
            comment,
        );
    }

    pub fn add_int_max(
        &mut self,
        dest: &FramePlacedType,
        self_int: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_int());
        assert!(self_int.ty().is_int());

        self.state.add_instruction(
            OpCode::IntMax,
            &[dest.addr().0, self_int.addr().0],
            node,
            comment,
        );
    }

    pub fn add_int_clamp(
        &mut self,
        dest: &FramePlacedType,
        self_int: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_int());
        assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntClamp,
            &[dest.addr().0, self_int.addr().0],
            node,
            comment,
        );
    }

    pub fn add_int_abs(
        &mut self,
        dest: &FramePlacedType,
        self_int: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_int());
        assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntAbs,
            &[dest.addr().0, self_int.addr().0],
            node,
            comment,
        );
    }

    pub fn add_int_to_float(
        &mut self,
        dest: &FramePlacedType,
        self_int: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_float());
        assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntToFloat,
            &[dest.addr().0, self_int.addr().0],
            node,
            comment,
        );
    }

    pub fn add_int_to_string(
        &mut self,
        dest: &FramePlacedType,
        self_int: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest.ty().is_str());
        assert!(self_int.ty().is_int());
        self.state.add_instruction(
            OpCode::IntToString,
            &[dest.addr().0, self_int.addr().0],
            node,
            comment,
        );
    }

    pub fn bool_to_string(
        &mut self,
        dest_str: &FramePlacedType,
        self_bool: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_str.ty().is_str());
        assert!(self_bool.ty().is_bool());
        self.state.add_instruction(
            OpCode::BoolToString,
            &[dest_str.addr().0, self_bool.addr().0],
            node,
            comment,
        );
    }

    pub fn float_to_string(
        &mut self,
        dest_str: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_str.ty().is_str());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatToString,
            &[dest_str.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_round(
        &mut self,
        dest_int: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_int.ty().is_int());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatRound,
            &[dest_int.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_floor(
        &mut self,
        dest_int: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_int.ty().is_int());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatFloor,
            &[dest_int.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_sqrt(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatSqrt,
            &[dest_float.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_sign(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatSign,
            &[dest_float.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_abs(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatAbs,
            &[dest_float.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_prnd(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatPseudoRandom,
            &[dest_float.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_sin(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatSin,
            &[dest_float.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_cos(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatCos,
            &[dest_float.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_acos(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatAcos,
            &[dest_float.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_asin(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatAsin,
            &[dest_float.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_atan2(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatAtan2,
            &[dest_float.addr().0, self_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_min(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        other: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatMin,
            &[dest_float.addr().0, self_float.addr().0, other.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_max(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        max_float: &FramePlacedType,
        node: &Node,
        comment: &str,
    ) {
        assert!(dest_float.ty().is_float());
        assert!(self_float.ty().is_float());
        assert!(max_float.ty().is_float());
        self.state.add_instruction(
            OpCode::FloatMax,
            &[dest_float.addr().0, self_float.addr().0, max_float.addr().0],
            node,
            comment,
        );
    }

    pub fn add_float_clamp(
        &mut self,
        dest_float: &FramePlacedType,
        self_float: &FramePlacedType,
        min_float: &FramePlacedType,
        max_float: &FramePlacedType,
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
                dest_float.addr().0,
                min_float.addr().0,
                self_float.addr().0,
                max_float.addr().0,
            ],
            node,
            comment,
        );
    }
}
