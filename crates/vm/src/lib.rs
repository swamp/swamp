/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::host::{HostArgs, HostFunctionCallback};
use seq_map::SeqMap;
use std::{alloc, ptr};
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{BinaryInstruction, InstructionPosition};

pub mod host;
mod map;
mod map_open;
mod string;
mod vec;

type Handler0 = fn(&mut Vm);
type Handler1 = fn(&mut Vm, u16);
type Handler2 = fn(&mut Vm, u16, u16);
type Handler3 = fn(&mut Vm, u16, u16, u16);
type Handler4 = fn(&mut Vm, u16, u16, u16, u16);
type Handler5 = fn(&mut Vm, u16, u16, u16, u16, u16);

#[derive(Copy, Clone)]
enum HandlerType {
    Args0(Handler0),
    Args1(Handler1),
    Args2(Handler2),
    Args3(Handler3),
    Args4(Handler4),
    Args5(Handler5),
}

pub struct Flags {
    z: bool,
}

pub struct Vm {
    // Memory
    stack_memory: *mut u8,
    stack_memory_size: usize,

    heap_memory: *mut u8,
    heap_memory_size: usize,

    // Memory regions (offsets)
    heap_alloc_offset: usize, // Current allocation point
    stack_offset: usize,      // Current stack position
    frame_offset: usize,      // Current frame position
    constant_memory_size: usize,

    // Execution state
    ip: usize,                            // Instruction pointer
    instructions: Vec<BinaryInstruction>, // Bytecode
    execution_complete: bool,             // Flag for completion

    // Function call management
    call_stack: Vec<CallFrame>, // Track function calls

    // Host function integration
    host_functions: SeqMap<u16, HostFunctionCallback>,

    handlers: [HandlerType; 256],

    // TODO: Error state
    debug_call_depth: usize,

    pub flags: Flags,
    last_frame_size: u16,
}

impl Vm {
    #[must_use]
    pub fn instructions(&self) -> &[BinaryInstruction] {
        &self.instructions
    }
}

impl Vm {
    pub fn reset(&mut self) {
        self.stack_offset = 0;
        self.frame_offset = self.stack_offset;
        self.ip = 0;
        self.execution_complete = false;
        self.call_stack.clear();
    }
}

impl Drop for Vm {
    fn drop(&mut self) {
        unsafe {
            // Free the memory that was allocated in new()
            let layout = alloc::Layout::from_size_align(self.stack_memory_size, ALIGNMENT).unwrap();
            alloc::dealloc(self.stack_memory, layout);
            let layout = alloc::Layout::from_size_align(self.heap_memory_size, ALIGNMENT).unwrap();
            alloc::dealloc(self.heap_memory, layout);
        }
    }
}

impl Vm {
    pub fn stack_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.stack_ptr(), self.stack_memory_size) }
    }

    pub fn frame_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.frame_ptr(), self.stack_memory_size) }
    }
    pub fn heap_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.heap_memory, self.heap_memory_size) }
    }

    pub fn constant_size(&self) -> usize {
        self.constant_memory_size
    }
}

const ALIGNMENT: usize = 8;
const ALIGNMENT_REST: usize = ALIGNMENT - 1;
const ALIGNMENT_MASK: usize = !ALIGNMENT_REST;

pub struct VmSetup {
    pub frame_memory_size: usize,
    pub heap_memory_size: usize,
    pub constant_memory: Vec<u8>,
}

impl Vm {
    pub fn new(instructions: Vec<BinaryInstruction>, setup: VmSetup) -> Self {
        let frame_memory = unsafe {
            alloc::alloc(
                alloc::Layout::from_size_align(setup.frame_memory_size, ALIGNMENT).unwrap(),
            )
        };
        let heap_memory = unsafe {
            alloc::alloc(alloc::Layout::from_size_align(setup.heap_memory_size, ALIGNMENT).unwrap())
        };

        let mut vm = Self {
            stack_memory: frame_memory,                 // Raw memory pointer
            stack_memory_size: setup.frame_memory_size, // Total memory size
            constant_memory_size: setup.constant_memory.len(),
            heap_memory,
            heap_memory_size: setup.heap_memory_size,
            heap_alloc_offset: setup.constant_memory.len(),
            stack_offset: 0,
            frame_offset: 0,
            ip: 0,
            instructions,
            execution_complete: false,
            call_stack: vec![],
            host_functions: SeqMap::default(),
            handlers: [const { HandlerType::Args0(Self::execute_unimplemented) }; 256],
            debug_call_depth: 0,
            last_frame_size: 0,
            flags: Flags { z: false },
        };

        vm.handlers[OpCode::Alloc as usize] = HandlerType::Args4(Self::execute_alloc);

        // Load immediate
        vm.handlers[OpCode::Ld8 as usize] = HandlerType::Args2(Self::execute_ld8);
        vm.handlers[OpCode::Ld16 as usize] = HandlerType::Args2(Self::execute_ld16);
        vm.handlers[OpCode::Ld32 as usize] = HandlerType::Args3(Self::execute_ld32);

        // Copy data in frame memory
        vm.handlers[OpCode::Mov as usize] = HandlerType::Args3(Self::execute_mov);
        vm.handlers[OpCode::MovLp as usize] = HandlerType::Args3(Self::execute_mov_lp);
        vm.handlers[OpCode::MovMem as usize] = HandlerType::Args4(Self::execute_mov_mem);
        vm.handlers[OpCode::Mov32 as usize] = HandlerType::Args4(Self::execute_mov_32);
        vm.handlers[OpCode::Stx as usize] = HandlerType::Args4(Self::execute_stx);
        vm.handlers[OpCode::Ldx as usize] = HandlerType::Args4(Self::execute_ldx);

        // Comparisons - Int
        vm.handlers[OpCode::LtI32 as usize] = HandlerType::Args2(Self::execute_lt_i32);
        vm.handlers[OpCode::LeI32 as usize] = HandlerType::Args2(Self::execute_le_i32);
        vm.handlers[OpCode::GtI32 as usize] = HandlerType::Args2(Self::execute_gt_i32);
        vm.handlers[OpCode::GeI32 as usize] = HandlerType::Args2(Self::execute_ge_i32);

        // Comparisons - Fixed
        vm.handlers[OpCode::LtF32 as usize] = HandlerType::Args2(Self::execute_ltf_i32);
        vm.handlers[OpCode::LeF32 as usize] = HandlerType::Args2(Self::execute_lef_i32);
        vm.handlers[OpCode::GtF32 as usize] = HandlerType::Args2(Self::execute_gtf_i32);
        vm.handlers[OpCode::GeF32 as usize] = HandlerType::Args2(Self::execute_gef_i32);

        vm.handlers[OpCode::Cmp as usize] = HandlerType::Args2(Self::execute_cmp);
        vm.handlers[OpCode::Cmp8 as usize] = HandlerType::Args2(Self::execute_cmp8);
        vm.handlers[OpCode::Cmp32 as usize] = HandlerType::Args2(Self::execute_cmp32);

        vm.handlers[OpCode::Eq8Imm as usize] = HandlerType::Args2(Self::execute_eq_8_imm);
        vm.handlers[OpCode::Tst8 as usize] = HandlerType::Args1(Self::execute_tst8);

        vm.handlers[OpCode::NotZ as usize] = HandlerType::Args2(Self::execute_notz); // needed for normalized Z
        vm.handlers[OpCode::Stz as usize] = HandlerType::Args2(Self::execute_stz);
        vm.handlers[OpCode::Stnz as usize] = HandlerType::Args2(Self::execute_stnz);

        vm.handlers[OpCode::Cmp32 as usize] = HandlerType::Args2(Self::execute_cmp32);

        // Logical Operations

        // Conditional jumps
        vm.handlers[OpCode::Bnz as usize] = HandlerType::Args1(Self::execute_bnz);
        vm.handlers[OpCode::Bz as usize] = HandlerType::Args1(Self::execute_bz);

        // Unconditional jump
        vm.handlers[OpCode::Jmp as usize] = HandlerType::Args1(Self::execute_jmp);

        // Operators - Int
        vm.handlers[OpCode::NegI32 as usize] = HandlerType::Args2(Self::execute_neg_i32);
        vm.handlers[OpCode::AddI32 as usize] = HandlerType::Args3(Self::execute_add_i32);
        vm.handlers[OpCode::MulI32 as usize] = HandlerType::Args3(Self::execute_mul_i32);
        vm.handlers[OpCode::SubI32 as usize] = HandlerType::Args2(Self::execute_sub_i32);
        vm.handlers[OpCode::ModI32 as usize] = HandlerType::Args2(Self::execute_mod_i32);
        vm.handlers[OpCode::DivI32 as usize] = HandlerType::Args2(Self::execute_div_i32);

        // Operators - Fixed
        vm.handlers[OpCode::NegF32 as usize] = HandlerType::Args2(Self::execute_neg_f32);
        vm.handlers[OpCode::AddI32 as usize] = HandlerType::Args3(Self::execute_add_f32);
        vm.handlers[OpCode::MulI32 as usize] = HandlerType::Args3(Self::execute_mul_f32);
        vm.handlers[OpCode::SubI32 as usize] = HandlerType::Args2(Self::execute_sub_f32);
        vm.handlers[OpCode::ModI32 as usize] = HandlerType::Args2(Self::execute_mod_f32);
        vm.handlers[OpCode::DivI32 as usize] = HandlerType::Args2(Self::execute_div_f32);

        // Call, enter, ret
        vm.handlers[OpCode::Call as usize] = HandlerType::Args1(Self::execute_call);
        vm.handlers[OpCode::Enter as usize] = HandlerType::Args1(Self::execute_enter);
        vm.handlers[OpCode::Ret as usize] = HandlerType::Args0(Self::execute_ret);

        vm.handlers[OpCode::HostCall as usize] = HandlerType::Args2(Self::execute_host_call);

        // Halt - return to host
        vm.handlers[OpCode::Hlt as usize] = HandlerType::Args0(Self::execute_hlt);

        // String
        vm.handlers[OpCode::StringFromSlice as usize] =
            HandlerType::Args4(Self::execute_string_from_constant_slice);
        vm.handlers[OpCode::StringAppend as usize] =
            HandlerType::Args3(Self::execute_string_append);

        // Int
        vm.handlers[OpCode::IntMin as usize] = HandlerType::Args2(Self::execute_int_min);
        vm.handlers[OpCode::IntMax as usize] = HandlerType::Args2(Self::execute_int_max);
        vm.handlers[OpCode::IntClamp as usize] = HandlerType::Args2(Self::execute_int_clamp);

        vm.handlers[OpCode::IntAbs as usize] = HandlerType::Args2(Self::execute_int_abs);
        vm.handlers[OpCode::IntToRnd as usize] = HandlerType::Args2(Self::execute_int_prnd);
        vm.handlers[OpCode::IntToString as usize] = HandlerType::Args2(Self::execute_int_to_string);
        vm.handlers[OpCode::IntToFloat as usize] = HandlerType::Args2(Self::execute_int_to_float);

        // Fixed
        vm.handlers[OpCode::FloatRound as usize] = HandlerType::Args2(Self::execute_float_round);
        vm.handlers[OpCode::FloatFloor as usize] = HandlerType::Args2(Self::execute_float_floor);
        vm.handlers[OpCode::FloatSqrt as usize] = HandlerType::Args2(Self::execute_float_sqrt);
        vm.handlers[OpCode::FloatSign as usize] = HandlerType::Args2(Self::execute_float_sign);
        vm.handlers[OpCode::FloatAbs as usize] = HandlerType::Args2(Self::execute_float_abs);
        vm.handlers[OpCode::FloatPseudoRandom as usize] =
            HandlerType::Args2(Self::execute_float_prnd);
        vm.handlers[OpCode::FloatSin as usize] = HandlerType::Args2(Self::execute_float_sin);
        vm.handlers[OpCode::FloatCos as usize] = HandlerType::Args2(Self::execute_float_cos);
        vm.handlers[OpCode::FloatAsin as usize] = HandlerType::Args2(Self::execute_float_asin);
        vm.handlers[OpCode::FloatAcos as usize] = HandlerType::Args2(Self::execute_float_acos);
        vm.handlers[OpCode::FloatAtan2 as usize] = HandlerType::Args2(Self::execute_float_atan2);
        vm.handlers[OpCode::FloatMin as usize] = HandlerType::Args2(Self::execute_float_min);
        vm.handlers[OpCode::FloatMax as usize] = HandlerType::Args2(Self::execute_float_max);
        vm.handlers[OpCode::FloatClamp as usize] = HandlerType::Args2(Self::execute_float_clamp);
        vm.handlers[OpCode::FloatToString as usize] =
            HandlerType::Args2(Self::execute_float_to_string);

        // Vec
        vm.handlers[OpCode::VecFromSlice as usize] =
            HandlerType::Args4(Self::execute_vec_from_slice);
        vm.handlers[OpCode::VecIterInit as usize] = HandlerType::Args2(Self::execute_vec_iter_init);
        vm.handlers[OpCode::VecIterNext as usize] = HandlerType::Args3(Self::execute_vec_iter_next);
        vm.handlers[OpCode::VecIterNextPair as usize] =
            HandlerType::Args2(Self::execute_vec_iter_next_pair);
        vm.handlers[OpCode::VecPush as usize] =
            HandlerType::Args2(Self::execute_vec_iter_next_pair);

        assert_eq!(vm.handlers.len(), OpCode::HostCall as usize);

        // Load indirect

        // Optional: Zero out the memory for safety?
        unsafe {
            ptr::write_bytes(frame_memory, 0, setup.frame_memory_size);
            ptr::write_bytes(heap_memory, 0, setup.heap_memory_size);
            ptr::copy_nonoverlapping(
                setup.constant_memory.as_ptr(),
                heap_memory,
                setup.constant_memory.len(),
            );
        }

        vm
    }

    /// # Panics
    /// if function already has been added
    pub fn add_host_function<F>(&mut self, id: u16, callback: F)
    where
        F: 'static + FnMut(HostArgs),
    {
        self.host_functions
            .insert(id, Box::new(callback))
            .expect("should work to insert ");
    }

    #[must_use]
    pub fn frame_offset(&self) -> usize {
        self.frame_offset
    }

    // Read a value at a specific offset from memory
    #[must_use]
    pub fn get_i32(&self, offset: usize) -> i32 {
        unsafe { *(self.ptr_at_i32(offset) as *const i32) }
    }

    pub fn load_bytecode(&mut self, instructions: Vec<BinaryInstruction>) {
        self.instructions = instructions;
        self.ip = 0;
        self.execution_complete = false;
    }

    #[inline]
    fn execute_ld32(&mut self, dst_offset: u16, lower_bits: u16, upper_bits: u16) {
        let value = ((upper_bits as u32) << 16) | (lower_bits as u32);

        let dst_ptr = self.ptr_at_u32(self.frame_offset + dst_offset as usize) as *mut u32;
        unsafe {
            *dst_ptr = value;
        }
    }

    /*
    #[inline]
    fn execute_ld32ptr(&mut self, base_offset: u16, offset: u16, lower_bits: u16, upper_bits: u16) {
        let value = ((upper_bits as u32) << 16) | (lower_bits as u32);

        let ptr_ptr = self.ptr_at_u16(self.frame_offset + base_offset as usize) as *const u16;
        let ptr = unsafe { *ptr_ptr };

        let addr = ptr + offset;

        let dst_ptr = self.ptr_at_u32(addr as usize) as *mut u32;
        unsafe {
            *dst_ptr = value;
        }
    }
    fn execute_ldx(&mut self, dst_offset: u16, base_offset: u16, offset: u16, size: u16) {
        let ptr_ptr = self.ptr_at_u16(self.frame_offset + base_offset as usize) as *const u16;
        let ptr = unsafe { *ptr_ptr };

        let src_addr = ptr + offset;

        let src_ptr = self.ptr_at_u8(src_addr as usize);
        let dst_ptr = self.ptr_at_u8(self.frame_offset + dst_offset as usize);

        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, size as usize);
        }
    }

    fn execute_stx(&mut self, base_offset: u16, offset: u16, src_offset: u16, size: u16) {
        let ptr_ptr = self.ptr_at_u16(self.frame_offset + base_offset as usize) as *const u16;
        let ptr = unsafe { *ptr_ptr };

        let dst_addr = ptr + offset;

        let src_ptr = self.ptr_at_u8(self.frame_offset + src_offset as usize);
        let dst_ptr = self.ptr_at_u8(dst_addr as usize);

        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, size as usize);
        }
    }

    #[inline]
    fn execute_st32x(&mut self, base_offset: u16, offset: u16, lower_bits: u16, upper_bits: u16) {
        let value = ((upper_bits as u32) << 16) | (lower_bits as u32);

        let ptr_ptr = self.ptr_at_u16(self.frame_offset + base_offset as usize) as *const u16;
        let ptr = unsafe { *ptr_ptr };

        let dst_addr = ptr + offset;

        let dst_ptr = self.ptr_at_u32(dst_addr as usize) as *mut u32;
        unsafe {
            *dst_ptr = value;
        }
    }
    */

    #[inline]
    fn execute_ld16(&mut self, dst_offset: u16, data: u16) {
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize) as *mut u16;
        unsafe {
            *dst_ptr = data;
        }
    }

    /*
    #[inline]
    fn execute_alloc(&mut self, dst_offset: u16, memory_size: u16) {
        let data_ptr = self.heap_allocate(memory_size as usize);
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize) as *mut u16;
        unsafe {
            *dst_ptr = data_ptr;
        }
    }
    */

    #[inline]
    fn execute_ld8(&mut self, dst_offset: u16, octet: u16) {
        let dst_ptr = self.frame_ptr_bool_at(dst_offset);
        unsafe {
            *dst_ptr = octet as u8;
        }
    }

    #[inline]
    fn execute_add_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.ptr_at_i32(self.frame_offset + lhs_offset as usize) as *const i32;
        let rhs_ptr = self.ptr_at_i32(self.frame_offset + rhs_offset as usize) as *const i32;
        let dst_ptr = self.ptr_at_i32(self.frame_offset + dst_offset as usize) as *mut i32;

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = lhs + rhs;
        }
    }

    #[inline]
    fn execute_mul_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.ptr_at_i32(self.frame_offset + lhs_offset as usize) as *const i32;
        let rhs_ptr = self.ptr_at_i32(self.frame_offset + rhs_offset as usize) as *const i32;
        let dst_ptr = self.ptr_at_i32(self.frame_offset + dst_offset as usize) as *mut i32;

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = lhs * rhs;
        }
    }

    #[inline]
    fn execute_neg_i32(&mut self, dst_offset: u16, lhs_offset: u16) {
        let lhs_ptr = self.ptr_at_i32(self.frame_offset + lhs_offset as usize) as *const i32;
        let dst_ptr = self.ptr_at_i32(self.frame_offset + dst_offset as usize);

        unsafe {
            let lhs = *lhs_ptr;
            *dst_ptr = -lhs;
        }
    }

    #[inline]
    fn execute_lt_i32(&mut self, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame_ptr_i32_const_at(lhs_offset);
        let rhs_ptr = self.frame_ptr_i32_const_at(rhs_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            self.flags.z = lhs < rhs;
        }
    }

    #[inline]
    fn execute_gt_i32(&mut self, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame_ptr_i32_const_at(lhs_offset);
        let rhs_ptr = self.frame_ptr_i32_const_at(rhs_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            self.flags.z = lhs > rhs;
        }
    }

    #[inline]
    fn execute_neg_f32(&mut self, dst_offset: u16, lhs_offset: u16) {
        let lhs_ptr = self.ptr_at_i32(self.frame_offset + lhs_offset as usize) as *const i32;
        let dst_ptr = self.ptr_at_i32(self.frame_offset + dst_offset as usize);

        unsafe {
            let lhs = *lhs_ptr;
            *dst_ptr = -lhs;
        }
    }

    #[inline]
    fn execute_eq_8_imm(&mut self, lhs_offset: u16, rhs_u8_in_u16: u16) {
        let lhs_u8 = self.frame_u8_at(lhs_offset);
        self.flags.z = lhs_u8 < rhs_u8_in_u16 as u8;
    }

    #[inline]
    fn execute_tst8(&mut self, lhs_offset: u16) {
        let lhs_u8 = self.frame_u8_at(lhs_offset);
        self.flags.z = lhs_u8 == 1;
    }

    #[inline]
    fn execute_bnz(&mut self, absolute_ip: u16) {
        if !self.flags.z {
            self.ip = absolute_ip as usize;
        }
    }

    #[inline]
    fn execute_bz(&mut self, absolute_ip: u16) {
        if self.flags.z {
            self.ip = absolute_ip as usize;
        }
    }

    #[inline]
    fn execute_jmp(&mut self, absolute_ip: u16) {
        self.ip = absolute_ip as usize;
    }

    #[inline]
    fn execute_hlt(&mut self) {
        self.execution_complete = true;
    }

    fn execute_unimplemented(&mut self) {
        let unknown_opcode = OpCode::from(self.instructions[self.ip].opcode);
        panic!("unknown OPCODE! {unknown_opcode} {unknown_opcode:?}");
    }

    #[inline]
    fn execute_mov(&mut self, dst_offset: u16, src_offset: u16, size: u16) {
        let src_ptr = self.ptr_at_u16(self.frame_offset + src_offset as usize);
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize);

        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, size as usize);
        }
    }

    #[inline]
    fn execute_mov_mem(
        &mut self,
        dst_offset: u16,
        const_lower: u16,
        const_upper: u16,
        memory_size: u16,
    ) {
        let const_offset = ((const_upper as u32) << 16) | (const_lower as u32);
        let dst_ptr = self.frame_ptr_at(dst_offset);
        let src_ptr = self.heap_ptr_immut_at(const_offset as usize);

        unsafe {
            ptr::copy_nonoverlapping(src_ptr, dst_ptr, memory_size as usize);
        }
    }

    #[inline]
    fn execute_mov_lp(&mut self, dst_offset: u16, src_offset: u16, size: u16) {
        let src_ptr = self.ptr_at_u16(self.frame_offset + src_offset as usize);
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize);

        unsafe {
            std::ptr::copy(src_ptr, dst_ptr, size as usize);
        }
    }

    // Helper to convert offset to pointer

    #[inline(always)]
    fn ptr_at_i32(&self, offset: usize) -> *mut i32 {
        // Ensure alignment
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {}", offset);
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(offset) as *mut i32 }
    }

    #[inline(always)]
    fn ptr_at_u32(&self, offset: usize) -> *mut u32 {
        // Ensure alignment
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {}", offset);
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(offset) as *mut u32 }
    }

    #[inline(always)]
    fn ptr_at_u16(&self, offset: usize) -> *mut u16 {
        // Ensure alignment
        debug_assert_eq!(offset % 2, 0, "Unaligned u16 access at offset {}", offset);
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(offset) as *mut u16 }
    }

    #[inline(always)]
    fn ptr_at_u8(&self, offset: usize) -> *mut u8 {
        // Inline ptr_at functionality
        unsafe { self.stack_memory.add(offset) }
    }

    #[inline(always)]
    fn heap_ptr_at(&self, offset: usize) -> *mut u8 {
        unsafe { self.heap_memory.add(offset) }
    }

    fn heap_ptr_immut_at(&self, offset: usize) -> *const u8 {
        unsafe { self.heap_memory.add(offset) }
    }

    // Helper to get current frame pointer
    fn frame_ptr(&self) -> *mut u8 {
        self.ptr_at_u8(self.frame_offset)
    }

    fn stack_ptr(&self) -> *mut u8 {
        self.ptr_at_u8(self.stack_offset)
    }

    #[inline(always)]
    fn frame_ptr_i32_at(&self, offset: u16) -> *mut i32 {
        self.ptr_at_i32(self.frame_offset + offset as usize)
    }

    #[inline(always)]
    fn frame_ptr_i32_const_at(&self, offset: u16) -> *const i32 {
        self.ptr_at_i32(self.frame_offset + offset as usize)
            .cast_const()
    }

    #[inline(always)]
    fn frame_u8_at(&self, offset: u16) -> u8 {
        unsafe { *self.ptr_at_u8(self.frame_offset + offset as usize) }
    }

    #[inline(always)]
    fn frame_u32_at(&self, offset: u16) -> u32 {
        unsafe { *self.ptr_at_u32(self.frame_offset + offset as usize) }
    }

    #[inline(always)]
    fn frame_ptr_bool_at(&self, offset: u16) -> *mut u8 {
        self.ptr_at_u8(self.frame_offset + offset as usize)
    }

    #[inline(always)]
    fn frame_ptr_at(&self, offset: u16) -> *mut u8 {
        self.ptr_at_u8(self.frame_offset + offset as usize)
    }

    #[inline(always)]
    fn frame_ptr_indirect_heap_immut_at(&self, frame_offset: u16) -> *const u8 {
        let heap_offset = self.frame_u32_at(frame_offset);
        self.heap_ptr_immut_at(heap_offset as usize)
    }

    fn frame_ptr_indirect_heap_offset_at(&self, frame_offset: u16) -> u32 {
        self.frame_u32_at(frame_offset)
    }

    #[inline(always)]
    fn frame_ptr_bool_const_at(&self, offset: u16) -> bool {
        unsafe { *self.ptr_at_u8(self.frame_offset + offset as usize) != 0 }
    }

    #[inline(always)]
    fn heap_allocate(&mut self, size: usize) -> u32 {
        let aligned_size = (size + ALIGNMENT_REST) & ALIGNMENT_MASK;
        let aligned_offset = (self.heap_alloc_offset + ALIGNMENT_REST) & ALIGNMENT_MASK;

        eprintln!(
            "heap_allocate original_size:{size}, aligned_size: {aligned_size} offset: {aligned_offset:08X} ({aligned_offset})"
        );

        debug_assert!(
            aligned_offset + aligned_size <= self.heap_memory_size,
            "Out of memory"
        );

        self.heap_alloc_offset = aligned_offset + aligned_size;

        aligned_offset as u32
    }

    #[inline(always)]
    fn heap_allocate_with_data(&mut self, octets: &[u8]) {
        let offset = self.heap_allocate(octets.len());
        {
            unsafe {
                ptr::copy_nonoverlapping(
                    octets.as_ptr(),
                    self.heap_ptr_at(offset as usize),
                    octets.len(),
                );
            }
        }
    }

    pub fn debug_opcode(&self, opcode: u8, operands: &[u16; 5]) {
        eprintln!(
            "{:8} [{}]",
            OpCode::from(opcode),
            match self.handlers[opcode as usize] {
                HandlerType::Args0(_) => String::new(),
                HandlerType::Args1(_) => format!("{:04x}", operands[0]),
                HandlerType::Args2(_) => format!("{:04x}, {:04x}", operands[0], operands[1]),
                HandlerType::Args3(_) => format!(
                    "{:04x}, {:04x}, {:04x}",
                    operands[0], operands[1], operands[2]
                ),
                HandlerType::Args4(_) => format!(
                    "{:04x}, {:04x}, {:04x}, {:04x}",
                    operands[0], operands[1], operands[2], operands[3]
                ),
                HandlerType::Args5(_) => format!(
                    "{:04x}, {:04x}, {:04x}, {:04x}, {:04x}",
                    operands[0], operands[1], operands[2], operands[3], operands[4],
                ),
            }
        );
    }

    fn debug_instructions(&self) {
        for (ip, instruction) in self.instructions.iter().enumerate() {
            eprint!("|> {ip:04x}: ");
            let operands = instruction.operands;
            self.debug_opcode(instruction.opcode, &operands);
        }
    }

    pub fn execute(&mut self) {
        self.execute_from_ip(&InstructionPosition(0));
    }

    pub fn execute_from_ip(&mut self, ip: &InstructionPosition) {
        self.ip = ip.0 as usize;
        self.execute_internal();
    }

    pub fn execute_internal(&mut self) {
        self.execution_complete = false;
        self.flags.z = false;
        self.call_stack.clear();

        #[cfg(feature = "debug_vm")]
        {
            eprintln!("program:");
            self.debug_instructions();
            eprintln!("start executing");
        }

        self.call_stack.push(CallFrame {
            return_address: 1,
            previous_frame_offset: 0,
            frame_size: 0,
        });

        while !self.execution_complete {
            let instruction = &self.instructions[self.ip];
            let opcode = instruction.opcode;

            #[cfg(feature = "debug_vm")]
            {
                let operands = instruction.operands;
                eprint!("> {:04x}: ", self.ip);
                self.debug_opcode(opcode, &operands);

                //    let s = hexify::format_hex(&self.frame_memory()[..16]);
                //  eprintln!("mem: {s}");
            }

            match self.handlers[opcode as usize] {
                HandlerType::Args0(handler) => handler(self),
                HandlerType::Args1(handler) => handler(self, instruction.operands[0]),
                HandlerType::Args2(handler) => {
                    handler(self, instruction.operands[0], instruction.operands[1]);
                }
                HandlerType::Args3(handler) => handler(
                    self,
                    instruction.operands[0],
                    instruction.operands[1],
                    instruction.operands[2],
                ),
                HandlerType::Args4(handler) => handler(
                    self,
                    instruction.operands[0],
                    instruction.operands[1],
                    instruction.operands[2],
                    instruction.operands[3],
                ),
                HandlerType::Args5(handler) => handler(
                    self,
                    instruction.operands[0],
                    instruction.operands[1],
                    instruction.operands[2],
                    instruction.operands[3],
                    instruction.operands[4],
                ),
            };

            self.ip += 1;
        }
    }

    fn execute_call(&mut self, target: u16) {
        let return_info = CallFrame {
            return_address: self.ip + 1,              // Instruction to return to
            previous_frame_offset: self.frame_offset, // Previous frame position
            frame_size: 0,                            // Will be filled by ENTER
        };

        self.call_stack.push(return_info);

        self.ip = target as usize;
    }

    #[inline]
    fn execute_host_call(&mut self, function_id: u16, bytes_to_copy_from_frame_ptr: u16) {
        let callback: &mut Box<dyn FnMut(HostArgs)> =
            self.host_functions.get_mut(&function_id).unwrap();
        //        let offset = self.frame_offset + self.last_frame_size as usize;
        //      let num_bytes = bytes_to_copy_from_frame_ptr as usize;

        unsafe {
            let host_args = HostArgs::new(
                self.stack_memory
                    .add(self.frame_offset + self.last_frame_size as usize),
                self.stack_memory_size - self.frame_offset,
                self.heap_memory,
                self.heap_memory_size,
            );
            callback(host_args);
        }
    }

    #[inline]
    fn execute_enter(&mut self, aligned_size: u16) {
        //let aligned_size = (frame_size as usize + ALIGNMENT_REST) & ALIGNMENT_MASK; // 8-byte alignment

        let frame = self.call_stack.last_mut().unwrap();
        frame.frame_size = aligned_size as usize;
        self.last_frame_size = aligned_size;

        // the functions frame of reference should be the stack offset
        self.frame_offset = self.stack_offset;

        // and we push the stack with the space of the local variables
        self.stack_offset += aligned_size as usize;
    }

    #[inline]
    fn execute_ret(&mut self) {
        let frame = self.call_stack.pop().unwrap();

        // Bring back the frame to the old frame
        self.frame_offset = frame.previous_frame_offset;

        // "pop" the space for the local variables of the stack
        self.stack_offset -= frame.frame_size;

        // going back to the old instruction
        self.ip = frame.return_address;
        self.ip -= 1; // Adjust for automatic increment

        // NOTE: Any return value is always at frame_offset + 0
    }
}

pub struct CallFrame {
    return_address: usize,        // Instruction to return to
    previous_frame_offset: usize, // Previous frame position
    frame_size: usize,            // Size of this frame
}
