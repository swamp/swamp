/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
extern crate core;

use crate::frame::FrameMemory;
use crate::heap::HeapMemory;
use crate::host::{HostArgs, HostFunctionCallback};
use fixed32::Fp;
use seq_map::SeqMap;
use std::{ptr, slice};
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{BinaryInstruction, InstructionPosition};

pub mod frame;
pub mod heap;
pub mod host;
mod map;
mod map_open;
mod range;
mod slice_region;
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

#[derive(Debug, Default)]
pub struct Debug {
    pub opcodes_executed: u16,
    pub call_depth: usize,
    pub max_call_depth: usize,
}

pub struct CallFrame {
    return_address: usize,        // Instruction to return to
    previous_frame_offset: usize, // Previous frame position
    frame_size: usize,            // Size of this frame
}

pub struct Vm {
    // Memory
    frame: FrameMemory,
    heap: HeapMemory,

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
    pub flags: Flags,
    last_frame_size: u16,
    pub debug: Debug,
}

impl Vm {
    #[must_use]
    pub const fn frame(&self) -> &FrameMemory {
        &self.frame
    }

    #[must_use]
    pub const fn heap(&self) -> &HeapMemory {
        &self.heap
    }
}

const ALIGNMENT: usize = 8;
const ALIGNMENT_REST: usize = ALIGNMENT - 1;
const ALIGNMENT_MASK: usize = !ALIGNMENT_REST;

pub struct VmSetup {
    pub stack_memory_size: usize,
    pub heap_memory_size: usize,
    pub constant_memory: Vec<u8>,
}

/*
stack_memory_size: setup.stack_memory_size, // Total memory size
           constant_memory_size: setup.constant_memory.len(),
           heap_memory,
           heap_memory_size: setup.heap_memory_size,
           heap_alloc_offset: setup.constant_memory.len(),
           stack_offset: 0,
           frame_offset: 0,
*/

impl Vm {
    #[allow(clippy::too_many_lines)]
    pub fn new(instructions: Vec<BinaryInstruction>, setup: VmSetup) -> Self {
        let frame_memory = FrameMemory::new(setup.stack_memory_size);
        let heap_memory = HeapMemory::new(setup.heap_memory_size, &setup.constant_memory);

        assert!(
            setup.constant_memory.len() < setup.heap_memory_size / 2,
            "too much constant memory"
        );

        let mut vm = Self {
            frame: frame_memory, // Raw memory pointer
            heap: heap_memory,
            ip: 0,
            instructions,
            execution_complete: false,
            call_stack: vec![],
            host_functions: SeqMap::default(),
            handlers: [const { HandlerType::Args0(Self::execute_unimplemented) }; 256],
            last_frame_size: 0,
            flags: Flags { z: false },
            debug: Debug {
                opcodes_executed: 0,
                call_depth: 0,
                max_call_depth: 0,
            },
        };

        vm.handlers[OpCode::Alloc as usize] = HandlerType::Args2(Self::execute_alloc);

        // Load immediate
        vm.handlers[OpCode::Ld8 as usize] = HandlerType::Args2(Self::execute_ld8);
        vm.handlers[OpCode::Ld16 as usize] = HandlerType::Args2(Self::execute_ld16);
        vm.handlers[OpCode::Ld32 as usize] = HandlerType::Args3(Self::execute_ld32);

        // Copy data in frame memory
        vm.handlers[OpCode::Mov as usize] = HandlerType::Args3(Self::execute_mov);
        vm.handlers[OpCode::MovLp as usize] = HandlerType::Args3(Self::execute_mov_lp);
        vm.handlers[OpCode::Mov32 as usize] = HandlerType::Args2(Self::execute_mov32);

        // Copy to and from heap
        vm.handlers[OpCode::Stx as usize] = HandlerType::Args5(Self::execute_stx);
        vm.handlers[OpCode::MovMem as usize] = HandlerType::Args4(Self::execute_mov_mem);

        // Comparisons - Int
        vm.handlers[OpCode::LtI32 as usize] = HandlerType::Args2(Self::execute_lt_i32);
        vm.handlers[OpCode::LeI32 as usize] = HandlerType::Args2(Self::execute_le_i32);
        vm.handlers[OpCode::GtI32 as usize] = HandlerType::Args2(Self::execute_gt_i32);
        vm.handlers[OpCode::GeI32 as usize] = HandlerType::Args2(Self::execute_ge_i32);

        // Comparisons - Float (Fixed Point)
        vm.handlers[OpCode::LtF32 as usize] = HandlerType::Args2(Self::execute_lt_i32);
        vm.handlers[OpCode::LeF32 as usize] = HandlerType::Args2(Self::execute_le_i32);
        vm.handlers[OpCode::GtF32 as usize] = HandlerType::Args2(Self::execute_gt_i32);
        vm.handlers[OpCode::GeF32 as usize] = HandlerType::Args2(Self::execute_ge_i32);

        // Comparison
        vm.handlers[OpCode::Cmp as usize] = HandlerType::Args3(Self::execute_cmp);
        vm.handlers[OpCode::Cmp8 as usize] = HandlerType::Args2(Self::execute_cmp_8);
        vm.handlers[OpCode::Cmp32 as usize] = HandlerType::Args2(Self::execute_cmp_32);

        vm.handlers[OpCode::Eq8Imm as usize] = HandlerType::Args2(Self::execute_eq_8_imm);

        // Z flag
        vm.handlers[OpCode::Tst8 as usize] = HandlerType::Args1(Self::execute_tst8);

        vm.handlers[OpCode::NotZ as usize] = HandlerType::Args0(Self::execute_notz); // needed for normalized Z
        vm.handlers[OpCode::Stz as usize] = HandlerType::Args1(Self::execute_stz);
        vm.handlers[OpCode::Stnz as usize] = HandlerType::Args1(Self::execute_stnz);

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
        vm.handlers[OpCode::SubI32 as usize] = HandlerType::Args3(Self::execute_sub_i32);
        vm.handlers[OpCode::ModI32 as usize] = HandlerType::Args3(Self::execute_mod_i32);
        vm.handlers[OpCode::DivI32 as usize] = HandlerType::Args3(Self::execute_div_i32);

        // Operators - Float (Fixed Point)
        vm.handlers[OpCode::NegF32 as usize] = HandlerType::Args2(Self::execute_neg_i32);
        vm.handlers[OpCode::AddI32 as usize] = HandlerType::Args3(Self::execute_add_i32);
        vm.handlers[OpCode::SubI32 as usize] = HandlerType::Args3(Self::execute_sub_i32);
        vm.handlers[OpCode::ModI32 as usize] = HandlerType::Args3(Self::execute_mod_i32);

        vm.handlers[OpCode::DivF32 as usize] = HandlerType::Args3(Self::execute_div_f32);
        vm.handlers[OpCode::MulF32 as usize] = HandlerType::Args3(Self::execute_mul_f32);

        // Call, enter, ret
        vm.handlers[OpCode::Call as usize] = HandlerType::Args1(Self::execute_call);
        vm.handlers[OpCode::Enter as usize] = HandlerType::Args1(Self::execute_enter);
        vm.handlers[OpCode::Ret as usize] = HandlerType::Args0(Self::execute_ret);

        vm.handlers[OpCode::HostCall as usize] = HandlerType::Args2(Self::execute_host_call);

        // Halt - return to host
        vm.handlers[OpCode::Hlt as usize] = HandlerType::Args0(Self::execute_hlt);

        // String
        /* TODO: BRING THIS BACK
        vm.handlers[OpCode::StringAppend as usize] =
            HandlerType::Args3(Self::execute_string_append);

         */

        // Int
        vm.handlers[OpCode::IntToRnd as usize] = HandlerType::Args2(Self::execute_prnd_i32);
        vm.handlers[OpCode::IntMin as usize] = HandlerType::Args3(Self::execute_min_i32);
        vm.handlers[OpCode::IntMax as usize] = HandlerType::Args3(Self::execute_max_i32);
        vm.handlers[OpCode::IntClamp as usize] = HandlerType::Args4(Self::execute_clamp_i32);

        vm.handlers[OpCode::IntAbs as usize] = HandlerType::Args2(Self::execute_abs_i32);

        vm.handlers[OpCode::IntToString as usize] = HandlerType::Args2(Self::execute_i32_to_string);
        vm.handlers[OpCode::IntToFloat as usize] = HandlerType::Args2(Self::execute_i32_to_f32);

        // Float (Fixed Point)
        vm.handlers[OpCode::FloatPseudoRandom as usize] =
            HandlerType::Args2(Self::execute_prnd_i32);
        vm.handlers[OpCode::FloatMin as usize] = HandlerType::Args3(Self::execute_min_i32);
        vm.handlers[OpCode::FloatMax as usize] = HandlerType::Args3(Self::execute_max_i32);
        vm.handlers[OpCode::FloatClamp as usize] = HandlerType::Args4(Self::execute_clamp_i32);

        vm.handlers[OpCode::FloatRound as usize] = HandlerType::Args2(Self::execute_f32_round);
        vm.handlers[OpCode::FloatFloor as usize] = HandlerType::Args2(Self::execute_f32_floor);
        vm.handlers[OpCode::FloatSqrt as usize] = HandlerType::Args2(Self::execute_f32_sqrt);
        vm.handlers[OpCode::FloatSign as usize] = HandlerType::Args2(Self::execute_f32_sign);
        vm.handlers[OpCode::FloatAbs as usize] = HandlerType::Args2(Self::execute_abs_i32);
        vm.handlers[OpCode::FloatSin as usize] = HandlerType::Args2(Self::execute_f32_sin);
        vm.handlers[OpCode::FloatCos as usize] = HandlerType::Args2(Self::execute_f32_cos);
        vm.handlers[OpCode::FloatAsin as usize] = HandlerType::Args2(Self::execute_f32_asin);
        vm.handlers[OpCode::FloatAcos as usize] = HandlerType::Args2(Self::execute_f32_acos);
        vm.handlers[OpCode::FloatAtan2 as usize] = HandlerType::Args3(Self::execute_f32_atan2);
        vm.handlers[OpCode::FloatToString as usize] =
            HandlerType::Args2(Self::execute_f32_to_string);

        // Collections ==========

        // Slices
        vm.handlers[OpCode::SliceFromHeap as usize] =
            HandlerType::Args4(Self::execute_slice_from_heap);
        vm.handlers[OpCode::SlicePairFromHeap as usize] =
            HandlerType::Args5(Self::execute_slice_pair_from_heap);

        // Range
        vm.handlers[OpCode::RangeIterInit as usize] =
            HandlerType::Args2(Self::execute_range_iter_init);
        vm.handlers[OpCode::RangeIterNext as usize] =
            HandlerType::Args3(Self::execute_range_iter_next);

        // Vec
        vm.handlers[OpCode::VecFromSlice as usize] =
            HandlerType::Args2(Self::execute_vec_from_slice);
        vm.handlers[OpCode::VecIterInit as usize] = HandlerType::Args2(Self::execute_vec_iter_init);
        vm.handlers[OpCode::VecIterNext as usize] = HandlerType::Args3(Self::execute_vec_iter_next);
        vm.handlers[OpCode::VecIterNextPair as usize] =
            HandlerType::Args4(Self::execute_vec_iter_next_pair);
        vm.handlers[OpCode::VecPush as usize] = HandlerType::Args2(Self::execute_vec_push);
        vm.handlers[OpCode::VecLen as usize] = HandlerType::Args2(Self::execute_vec_len);
        vm.handlers[OpCode::VecGet as usize] = HandlerType::Args3(Self::execute_vec_get);
        vm.handlers[OpCode::VecSet as usize] = HandlerType::Args3(Self::execute_vec_set);

        vm.handlers[OpCode::MapNewFromPairs as usize] =
            HandlerType::Args2(Self::execute_map_open_addressing_from_slice);
        vm.handlers[OpCode::MapFetch as usize] =
            HandlerType::Args3(Self::execute_map_open_addressing_get);
        vm.handlers[OpCode::MapSet as usize] =
            HandlerType::Args3(Self::execute_map_open_addressing_set);
        vm.handlers[OpCode::MapHas as usize] =
            HandlerType::Args2(Self::execute_map_open_addressing_has);

        // Map
        /* TODO: BRING THESE BACK
        vm.handlers[OpCode::MapIterInit as usize] = HandlerType::Args2(Self::execute_map_iter_init);
        vm.handlers[OpCode::MapIterNext as usize] = HandlerType::Args3(Self::execute_map_iter_next);
        vm.handlers[OpCode::MapIterNextPair as usize] =
            HandlerType::Args2(Self::execute_map_iter_next_pair);
        vm.handlers[OpCode::MapSet as usize] = HandlerType::Args2(Self::execute_map_iter_next_pair);
        vm.handlers[OpCode::MapLen as usize] = HandlerType::Args2(Self::execute_map_len);
         */

        // Other ==========
        // Unwrap
        vm.handlers[OpCode::UnwrapJmpNone as usize] =
            HandlerType::Args2(Self::execute_unwrap_jmp_none);
        vm.handlers[OpCode::UnwrapJmpSome as usize] =
            HandlerType::Args2(Self::execute_unwrap_jmp_some);

        //assert_eq!(vm.handlers.len(), OpCode::HostCall as usize);

        // Optional: Zero out the memory for safety?
        unsafe {}

        vm
    }

    pub fn execute_internal(&mut self) {
        self.execution_complete = false;
        self.flags.z = false;
        self.call_stack.clear();
        self.frame.reset_offset();

        #[cfg(feature = "debug_vm")]
        {
            eprintln!(
                "start executing --------- frame {:X} heap: {:X}",
                self.frame.frame_offset, self.heap.heap_alloc_offset
            );
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
                eprint!("> {:04X}: ", self.ip);
                self.debug_opcode(opcode, &operands);
                self.debug.opcodes_executed += 1;

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
            }

            self.ip += 1;
        }
    }

    pub fn execute(&mut self) {
        self.execute_from_ip(&InstructionPosition(0));
    }

    pub fn execute_from_ip(&mut self, ip: &InstructionPosition) {
        self.ip = ip.0 as usize;
        self.execute_internal();
    }

    fn execute_unimplemented(&mut self) {
        let unknown_opcode = OpCode::from(self.instructions[self.ip].opcode);
        eprintln!("error: opcode not implemented: {unknown_opcode} {unknown_opcode:?}");
        eprintln!("VM runtime halted.");
        self.debug_output();
        panic!("unknown OPCODE! {unknown_opcode} {unknown_opcode:?}");
    }

    pub fn stack_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.frame.stack_ptr(), self.frame.stack_memory_size) }
    }

    pub fn frame_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.frame.frame_ptr(), self.frame.stack_memory_size) }
    }

    pub fn heap_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.heap.get_heap_ptr(0), self.heap.heap_memory_size) }
    }

    pub fn constant_size(&self) -> usize {
        self.heap.constant_memory_size
    }
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn instructions(&self) -> &[BinaryInstruction] {
        &self.instructions
    }
    pub fn reset(&mut self) {
        self.frame.reset();
        self.heap.reset_allocator();

        self.ip = 0;
        self.execution_complete = false;
        self.call_stack.clear();
    }

    pub fn reset_stack_and_heap_to_constant_limit(&mut self) {
        self.heap.reset_allocator();
        self.frame.reset();

        self.execution_complete = false;
        self.call_stack.clear();
        self.ip = 0;

        #[cfg(feature = "debug_vm")]
        {
            self.reset_debug();
        }
    }

    pub fn reset_debug(&mut self) {
        self.debug = Debug::default();
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
        self.frame.frame_offset
    }

    pub fn load_bytecode(&mut self, instructions: Vec<BinaryInstruction>) {
        self.instructions = instructions;
        self.ip = 0;
        self.execution_complete = false;
    }

    #[inline]
    fn execute_ld32(&mut self, dst_offset: u16, lower_bits: u16, upper_bits: u16) {
        let value = ((upper_bits as u32) << 16) | (lower_bits as u32);

        let dst_ptr = self.frame.get_frame_ptr_as_u32(dst_offset);
        unsafe {
            *dst_ptr = value;
        }
    }

    #[inline]
    pub fn execute_unwrap_jmp_some(&mut self, optional_addr: u16, jmp_offset: u16) {
        let tag_value = self.frame.read_frame_u8(optional_addr);
        if tag_value != 0 {
            self.ip = jmp_offset as usize;
        }
    }

    #[inline]
    pub fn execute_unwrap_jmp_none(&mut self, optional_addr: u16, jmp_offset: u16) {
        let tag_value = self.frame.read_frame_u8(optional_addr);
        if tag_value == 0 {
            self.ip = jmp_offset as usize;
        }
    }

    #[inline]
    fn execute_ld16(&mut self, dst_offset: u16, data: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_u16(dst_offset);
        unsafe {
            *dst_ptr = data;
        }
    }

    #[inline]
    fn execute_alloc(&mut self, dst_offset: u16, memory_size: u16) {
        let data_ptr = self.heap.heap_allocate(memory_size as usize);
        let dst_ptr = self.frame.get_frame_ptr_as_u32(dst_offset);
        unsafe {
            *dst_ptr = data_ptr;
        }
    }

    #[inline]
    fn execute_ld8(&mut self, dst_offset: u16, octet: u16) {
        let dst_ptr = self.frame.get_frame_ptr(dst_offset);
        unsafe {
            *dst_ptr = octet as u8;
        }
    }

    // Fixed Point special methods
    #[inline]
    fn execute_mul_f32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = (Fp::from_raw(lhs) * Fp::from_raw(rhs)).inner();
        }
    }

    #[inline]
    fn execute_div_f32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = (Fp::from_raw(lhs) / Fp::from_raw(rhs)).inner();
        }
    }

    #[inline]
    fn execute_f32_round(&mut self, dst_offset: u16, val_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        unsafe {
            *dst_ptr = Fp::from_raw(*val_ptr).round().into();
        }
    }

    #[inline]
    fn execute_f32_floor(&mut self, dst_offset: u16, val_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        unsafe {
            *dst_ptr = Fp::from_raw(*val_ptr).floor().into();
        }
    }

    #[inline]
    fn execute_f32_sqrt(&mut self, dst_offset: u16, val_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        unsafe {
            *dst_ptr = Fp::from_raw(*val_ptr).sqrt().inner();
        }
    }

    #[inline]
    fn execute_f32_sin(&mut self, dst_offset: u16, val_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        unsafe {
            *dst_ptr = Fp::from_raw(*val_ptr).sin().inner();
        }
    }

    #[inline]
    fn execute_f32_asin(&mut self, dst_offset: u16, val_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        unsafe {
            *dst_ptr = Fp::from_raw(*val_ptr).asin().inner();
        }
    }

    #[inline]
    fn execute_f32_cos(&mut self, dst_offset: u16, val_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        unsafe {
            *dst_ptr = Fp::from_raw(*val_ptr).cos().inner();
        }
    }

    #[inline]
    fn execute_f32_acos(&mut self, dst_offset: u16, val_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        unsafe {
            *dst_ptr = Fp::from_raw(*val_ptr).acos().inner();
        }
    }

    #[inline]
    fn execute_f32_atan2(&mut self, dst_offset: u16, val_offset: u16, y_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        // TODO: Implement atan2 in fixed32
        todo!()
    }

    #[inline]
    fn execute_f32_to_string(&mut self, dst_string: u16, val_offset: u16) {
        let dst_ptr = self
            .frame
            .get_heap_u32_ptr_via_frame(dst_string, &self.heap);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        let fp = unsafe { Fp::from_raw(*val_ptr) };

        unsafe {
            *dst_ptr = self.create_string(&fp.to_string());
        }
    }

    #[inline]
    fn execute_f32_sign(&mut self, dst_offset: u16, val_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        unsafe {
            let v = *val_ptr;
            *dst_ptr = Fp::from(if v < 0 {
                -1
            } else if v > 0 {
                1
            } else {
                0
            })
            .inner();

            // TODO: signum() is/was incorrect in Fixed32 crate
            //*dst_ptr = Fp::from_raw(*val_ptr).signum().inner();
        }
    }

    #[inline]
    fn execute_neg_i32(&mut self, dst_offset: u16, lhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            *dst_ptr = -lhs;
        }
    }

    #[inline]
    fn execute_add_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset) as *mut i32;

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = lhs + rhs;

            #[cfg(feature = "debug_vm")]
            {
                unsafe {
                    eprintln!("add {} = {} + {}", *dst_ptr, lhs, rhs);
                }
            }
        }
    }

    #[inline]
    fn execute_mul_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = (Fp::from_raw(lhs) * Fp::from_raw(rhs)).inner();
        }
    }

    #[inline]
    fn execute_sub_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = lhs - rhs;

            #[cfg(feature = "debug_vm")]
            {
                unsafe {
                    eprintln!("sub {} = {} - {}", *dst_ptr, lhs, rhs);
                }
            }
        }
    }

    #[inline]
    fn execute_mod_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = ((lhs % rhs) + rhs) % rhs; // Swamp uses strict modulo instead of remainder
        }

        #[cfg(feature = "debug_vm")]
        {
            unsafe {
                eprintln!("mod {} = {} % {}", *dst_ptr, *lhs_ptr, *rhs_ptr);
            }
        }
    }

    #[inline]
    fn execute_div_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = (Fp::from_raw(lhs) / Fp::from_raw(rhs)).inner();
        }
    }

    #[inline]
    fn execute_lt_i32(&mut self, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            self.flags.z = lhs < rhs;
        }
    }

    #[inline]
    fn execute_le_i32(&mut self, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            self.flags.z = lhs <= rhs;
        }
    }

    #[inline]
    fn execute_gt_i32(&mut self, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            self.flags.z = lhs > rhs;
        }
    }

    #[inline]
    fn execute_ge_i32(&mut self, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            self.flags.z = lhs >= rhs;
        }
    }

    #[inline]
    fn execute_prnd_i32(&mut self, dst_offset: u16, src_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let src_ptr = self.frame.get_frame_const_ptr_as_i32(src_offset);

        unsafe {
            *dst_ptr = squirrel_prng::squirrel_noise5(*src_ptr as u32, 0) as i32;
        }
    }

    #[inline]
    fn execute_i32_to_string(&mut self, dst_string: u16, val_offset: u16) {
        let dst_ptr = self
            .frame
            .get_heap_u32_ptr_via_frame(dst_string, &self.heap);
        let val_ptr = self.frame.get_frame_const_ptr_as_i32(val_offset);

        unsafe {
            *dst_ptr = self.create_string(&(*val_ptr).to_string());
        }
    }

    #[inline]
    fn execute_i32_to_f32(&mut self, float_dest: u16, int_source: u16) {
        let source_ptr = self.frame.get_frame_const_ptr_as_i32(int_source);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(float_dest);

        unsafe {
            *dst_ptr = Fp::from((*source_ptr) as i16).inner();
        }
    }

    #[inline]
    fn execute_abs_i32(&mut self, dst_offset: u16, src_offset: u16) {
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);
        let src_ptr = self.frame.get_frame_const_ptr_as_i32(src_offset);

        unsafe {
            let lhs = *src_ptr;

            *dst_ptr = if lhs < 0 { -lhs } else { lhs }
        }
    }

    #[inline]
    fn execute_min_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;

            if lhs < rhs {
                *dst_ptr = lhs;
            } else {
                *dst_ptr = rhs;
            }
        }
    }

    #[inline]
    fn execute_max_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_i32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_i32(rhs_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;

            if lhs > rhs {
                *dst_ptr = lhs;
            } else {
                *dst_ptr = rhs;
            }
        }
    }

    #[inline]
    fn execute_clamp_i32(
        &mut self,
        dst_offset: u16,
        v_offset: u16,
        min_offset: u16,
        max_offset: u16,
    ) {
        let v_ptr = self.frame.get_frame_const_ptr_as_i32(v_offset);
        let min_ptr = self.frame.get_frame_const_ptr_as_i32(min_offset);
        let max_ptr = self.frame.get_frame_const_ptr_as_i32(max_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_i32(dst_offset);

        unsafe {
            let v = *v_ptr;
            let min = *min_ptr;
            let max = *max_ptr;

            let r = if v < min {
                min
            } else if v > max {
                max
            } else {
                v
            };

            *dst_ptr = r;
        }
    }

    #[inline]
    fn execute_cmp(&mut self, lhs_offset: u16, rhs_offset: u16, count: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr(rhs_offset);

        // SAFETY: The caller must ensure that lhs_offset, rhs_offset, and count
        // define valid, readable memory ranges within the current stack frame.
        unsafe {
            let lhs_slice = slice::from_raw_parts(lhs_ptr, count as usize);
            let rhs_slice = slice::from_raw_parts(rhs_ptr, count as usize);

            self.flags.z = lhs_slice == rhs_slice
        }
    }

    #[inline]
    fn execute_cmp_8(&mut self, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_ptr(lhs_offset);
        let rhs_ptr = self.frame.get_frame_ptr(rhs_offset);

        unsafe {
            self.flags.z = *lhs_ptr == *rhs_ptr;
        }
    }

    #[inline]
    fn execute_cmp_32(&mut self, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame.get_frame_const_ptr_as_u32(lhs_offset);
        let rhs_ptr = self.frame.get_frame_const_ptr_as_u32(rhs_offset);

        unsafe {
            self.flags.z = *lhs_ptr == *rhs_ptr;
        }
    }

    #[inline]
    fn execute_eq_8_imm(&mut self, lhs_offset: u16, rhs_u8_in_u16: u16) {
        let lhs_u8 = self.frame.read_frame_u8(lhs_offset);
        self.flags.z = lhs_u8 < rhs_u8_in_u16 as u8;
    }

    #[inline]
    fn execute_tst8(&mut self, lhs_offset: u16) {
        let lhs_u8 = self.frame.read_frame_u8(lhs_offset);
        self.flags.z = lhs_u8 == 1;
    }

    #[inline]
    fn execute_notz(&mut self) {
        self.flags.z = !self.flags.z;
    }

    #[inline]
    fn execute_stz(&mut self, target: u16) {
        let target_ptr = self.frame.get_frame_ptr(target);
        unsafe {
            *target_ptr = self.flags.z as u8;
        }
    }

    #[inline]
    fn execute_stnz(&mut self, target: u16) {
        let target_ptr = self.frame.get_frame_ptr(target);
        unsafe {
            *target_ptr = !self.flags.z as u8;
        }
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
        #[cfg(feature = "debug_vm")]
        {
            self.debug_output();
        }
    }

    fn debug_output(&self) {
        eprintln!(
            "total opcodes executed: {}, call_stack_depth: {}, max_call_depth:{}",
            self.debug.opcodes_executed, self.debug.call_depth, self.debug.max_call_depth
        );
    }

    #[inline]
    fn execute_mov(&mut self, dst_offset: u16, src_offset: u16, size: u16) {
        let src_ptr = self.frame.get_frame_ptr(src_offset);
        let dst_ptr = self.frame.get_frame_ptr(dst_offset);

        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, size as usize);
        }
        #[cfg(feature = "debug_vm")]
        {
            unsafe {
                eprintln!("mov {:X}", *dst_ptr);
            }
        }
    }

    #[inline]
    fn execute_mov32(&mut self, dst_offset: u16, src_offset: u16) {
        let src_ptr = self.frame.get_frame_ptr_as_u32(src_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_u32(dst_offset);

        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, 4);
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
        let dst_ptr = self.frame.get_frame_ptr(dst_offset);
        let src_ptr = self.heap.get_heap_const_ptr(const_offset as usize);

        unsafe {
            ptr::copy_nonoverlapping(src_ptr, dst_ptr, memory_size as usize);
        }
    }

    #[inline]
    fn execute_stx(
        &mut self,
        dst_offset: u16,
        const_lower: u16,
        const_upper: u16,
        source_offset: u16,
        memory_size: u16,
    ) {
        let indirect_offset = ((const_upper as u32) << 16) | (const_lower as u32);
        let dst_heap_ptr =
            self.frame
                .get_heap_ptr_via_frame_with_offset(dst_offset, indirect_offset, &self.heap);
        let source_ptr = self.frame.get_frame_const_ptr(source_offset);

        unsafe {
            ptr::copy_nonoverlapping(source_ptr, dst_heap_ptr, memory_size as usize);
        }
    }

    #[inline]
    fn execute_mov_lp(&mut self, dst_offset: u16, src_offset: u16, size: u16) {
        let src_ptr = self.frame.get_frame_ptr_as_u16(src_offset);
        let dst_ptr = self.frame.get_frame_ptr_as_u16(dst_offset);

        unsafe {
            std::ptr::copy(src_ptr, dst_ptr, size as usize);
        }
    }

    #[cfg(feature = "debug_vm")]
    pub fn debug_opcode(&self, opcode: u8, operands: &[u16; 5]) {
        eprintln!(
            "{:8} {}",
            OpCode::from(opcode),
            match self.handlers[opcode as usize] {
                HandlerType::Args0(_) => String::new(),
                HandlerType::Args1(_) => format!("{:04X}", operands[0]),
                HandlerType::Args2(_) => format!("{:04X}, {:04X}", operands[0], operands[1]),
                HandlerType::Args3(_) => format!(
                    "{:04X}, {:04X}, {:04X}",
                    operands[0], operands[1], operands[2]
                ),
                HandlerType::Args4(_) => format!(
                    "{:04X}, {:04X}, {:04X}, {:04X}",
                    operands[0], operands[1], operands[2], operands[3]
                ),
                HandlerType::Args5(_) => format!(
                    "{:04X}, {:04X}, {:04X}, {:04X}, {:04X}",
                    operands[0], operands[1], operands[2], operands[3], operands[4],
                ),
            }
        );
    }

    fn execute_call(&mut self, target: u16) {
        let return_info = CallFrame {
            return_address: self.ip + 1, // Instruction to return to
            previous_frame_offset: self.frame.frame_offset, // Previous frame position
            frame_size: 0,               // Will be filled by ENTER
        };

        self.call_stack.push(return_info);

        self.ip = target as usize;
        #[cfg(feature = "debug_vm")]
        {
            self.debug.call_depth += 1;
            if self.debug.call_depth > self.debug.max_call_depth {
                self.debug.max_call_depth = self.debug.call_depth;
            }
        }
    }

    #[inline]
    fn execute_host_call(&mut self, function_id: u16, bytes_to_copy_from_frame_ptr: u16) {
        let callback: &mut Box<dyn FnMut(HostArgs)> =
            self.host_functions.get_mut(&function_id).unwrap();

        unsafe {
            let host_args = HostArgs::new(
                self.frame
                    .stack_memory
                    .add(self.frame.frame_offset + self.last_frame_size as usize),
                self.frame.stack_memory_size - self.frame.frame_offset,
                self.heap.heap_memory,
                self.heap.heap_memory_size,
            );
            callback(host_args);
        }
    }

    #[inline]
    fn execute_enter(&mut self, aligned_size: u16) {
        let frame = self.call_stack.last_mut().unwrap();
        frame.frame_size = aligned_size as usize;
        self.last_frame_size = aligned_size;

        self.frame.push(aligned_size as usize);
    }

    #[inline]
    fn execute_ret(&mut self) {
        let call_frame = self.call_stack.pop().unwrap();

        self.frame
            .pop(call_frame.previous_frame_offset, call_frame.frame_size);

        // going back to the old instruction
        self.ip = call_frame.return_address;
        self.ip -= 1; // Adjust for automatic increment

        // NOTE: Any return value is always at frame_offset + 0

        #[cfg(feature = "debug_vm")]
        {
            self.debug.call_depth -= 1;
        }
    }
}
