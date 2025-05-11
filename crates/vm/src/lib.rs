/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
extern crate core;

use crate::host::{HostArgs, HostFunctionCallback};
use crate::memory::Memory;
use fixed32::Fp;
use seq_map::SeqMap;
use std::cmp::PartialEq;
use std::ptr;
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{BinaryInstruction, InstructionPosition};

pub mod heap;
pub mod host;
mod map;
mod map_open;
pub mod memory;
mod range;
mod slice_region;
mod string;
mod vec;

#[macro_export]
macro_rules! u8s_to_u16 {
    ($lsb:expr, $msb:expr) => {
        // Cast bytes to u16 before shifting to prevent overflow and ensure correct bit manipulation.
        // The most significant byte ($msb) is shifted left by 8 bits.
        // The least significant byte ($lsb) remains in the lower 8 bits.
        // The results are combined using a bitwise OR.
        (($msb as u16) << 8) | ($lsb as u16)
    };
}

#[macro_export]
macro_rules! i16_from_u8s {
    ($lsb:expr, $msb:expr) => {
        // Cast bytes to u16 before shifting to prevent overflow and ensure correct bit manipulation.
        // The most significant byte ($msb) is shifted left by 8 bits.
        // The least significant byte ($lsb) remains in the lower 8 bits.
        // The results are combined using a bitwise OR.
        ((($msb as u16) << 8) | ($lsb as u16)) as i16
    };
}

#[macro_export]
macro_rules! u32_from_u8s {
    ($lsb:expr, $msb:expr, $msb2:expr, $msb3:expr) => {
        // Cast bytes to u16 before shifting to prevent overflow and ensure correct bit manipulation.
        // The most significant byte ($msb) is shifted left by 8 bits.
        // The least significant byte ($lsb) remains in the lower 8 bits.
        // The results are combined using a bitwise OR.
        (($msb3 as u32) << 24) | (($msb2 as u32) << 16) | (($msb as u32) << 8) | ($lsb as u32)
    };
}

#[macro_export]
macro_rules! get_reg {
    ($vm:expr, $reg_idx:expr) => {
        $vm.registers[$reg_idx as usize]
    };
}

#[macro_export]
macro_rules! set_reg {
    // Stores a value into a register, converting it to u32
    // $vm:expr is the VM state (e.g., `&mut self`)
    // $reg_idx:expr is the destination register index (e.g., `dst_reg`)
    // $value:expr is the value to store (must be convertible to u32)
    ($vm:expr, $reg_idx:expr, $value:expr) => {
        // Use `as u32` to convert the value to the register's storage type
        $vm.registers[$reg_idx as usize] = $value as u32;
    };
}

type Handler0 = fn(&mut Vm);
type Handler1 = fn(&mut Vm, u8);
type Handler2 = fn(&mut Vm, u8, u8);
type Handler3 = fn(&mut Vm, u8, u8, u8);
type Handler4 = fn(&mut Vm, u8, u8, u8, u8);
type Handler5 = fn(&mut Vm, u8, u8, u8, u8, u8);

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
    pub opcodes_executed: u8,
    pub call_depth: usize,
    pub max_call_depth: usize,
}

pub struct CallFrame {
    return_address: usize,        // Instruction to return to
    previous_frame_offset: usize, // Previous frame position
    previous_stack_offset: usize, // Size of this frame
}

type RegContents = u32;

pub struct Vm {
    // Memory
    memory: Memory,

    // Execution state
    pc: usize,                            // Instruction pointer
    instructions: Vec<BinaryInstruction>, // Bytecode
    execution_complete: bool,             // Flag for completion

    // Function call management
    call_stack: Vec<CallFrame>, // Track function calls

    // Host function integration
    host_functions: SeqMap<u16, HostFunctionCallback>,

    handlers: [HandlerType; 256],

    pub registers: [RegContents; 256], // Normal CPUs have around 31 general purpose registers

    // TODO: Error state
    pub flags: Flags,
    pub debug: Debug,
    pub debug_enabled: bool,
}

impl Vm {}

const ALIGNMENT: usize = 8;
const ALIGNMENT_REST: usize = ALIGNMENT - 1;
const ALIGNMENT_MASK: usize = !ALIGNMENT_REST;

pub struct VmSetup {
    pub stack_memory_size: usize,
    pub heap_memory_size: usize,
    pub constant_memory: Vec<u8>,
    pub debug_enabled: bool,
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
        let memory = Memory::new(setup.stack_memory_size, &setup.constant_memory);

        assert!(
            setup.constant_memory.len() < setup.heap_memory_size / 2,
            "too much constant memory"
        );

        let mut vm = Self {
            memory, // Raw memory pointer
            pc: 0,
            instructions,
            execution_complete: false,
            call_stack: vec![],
            host_functions: SeqMap::default(),
            handlers: [const { HandlerType::Args0(Self::execute_unimplemented) }; 256],
            registers: [const { 0 }; 256],
            flags: Flags { z: false },
            debug: Debug {
                opcodes_executed: 0,
                call_depth: 0,
                max_call_depth: 0,
            },
            debug_enabled: setup.debug_enabled,
        };

        vm.handlers[OpCode::Alloc as usize] = HandlerType::Args3(Self::execute_alloc);

        // Load immediate
        vm.handlers[OpCode::Mov8FromImmediateValue as usize] =
            HandlerType::Args2(Self::execute_ld8);
        vm.handlers[OpCode::Mov32FromImmediateValue as usize] =
            HandlerType::Args5(Self::execute_ld32);

        // Copy data in frame memory
        vm.handlers[OpCode::MovReg as usize] = HandlerType::Args2(Self::execute_mov_reg);
        vm.handlers[OpCode::LdPtrFromEffectiveAddress as usize] =
            HandlerType::Args3(Self::execute_lea);

        // Copy to and from heap
        vm.handlers[OpCode::BlockCopy as usize] = HandlerType::Args4(Self::execute_mov_mem);

        // Comparisons - Int
        vm.handlers[OpCode::LtI32 as usize] = HandlerType::Args2(Self::execute_lt_i32);
        vm.handlers[OpCode::LeI32 as usize] = HandlerType::Args2(Self::execute_le_i32);
        vm.handlers[OpCode::GtI32 as usize] = HandlerType::Args2(Self::execute_gt_i32);
        vm.handlers[OpCode::GeI32 as usize] = HandlerType::Args2(Self::execute_ge_i32);

        // Comparison
        // TODO: vm.handlers[OpCode::CmpReg as usize] = HandlerType::Args2(Self::execute_cmp_reg);

        vm.handlers[OpCode::Eq8Imm as usize] = HandlerType::Args2(Self::execute_eq_8_imm);

        // Z flag
        vm.handlers[OpCode::MovToZFromReg as usize] = HandlerType::Args1(Self::execute_tst8);

        vm.handlers[OpCode::NotZ as usize] = HandlerType::Args0(Self::execute_not_z); // needed for normalized Z
        vm.handlers[OpCode::MovFromZToReg as usize] = HandlerType::Args1(Self::execute_st_z);
        vm.handlers[OpCode::MovFromNotZToReg as usize] = HandlerType::Args1(Self::execute_st_nz);

        // Logical Operations

        // Conditional jumps
        vm.handlers[OpCode::BNe as usize] = HandlerType::Args2(Self::execute_bnz);
        vm.handlers[OpCode::BEq as usize] = HandlerType::Args2(Self::execute_bz);

        // Unconditional jump
        vm.handlers[OpCode::B as usize] = HandlerType::Args4(Self::execute_jmp);

        // Operators - Int
        vm.handlers[OpCode::AddU32 as usize] = HandlerType::Args3(Self::execute_add_u32);
        vm.handlers[OpCode::MulU32 as usize] = HandlerType::Args3(Self::execute_mul_u32);
        vm.handlers[OpCode::SubU32 as usize] = HandlerType::Args3(Self::execute_sub_u32);

        vm.handlers[OpCode::NegI32 as usize] = HandlerType::Args2(Self::execute_neg_i32);
        vm.handlers[OpCode::ModI32 as usize] = HandlerType::Args3(Self::execute_mod_i32);
        vm.handlers[OpCode::DivI32 as usize] = HandlerType::Args3(Self::execute_div_i32);

        // Operators - Float (Fixed Point)
        vm.handlers[OpCode::DivF32 as usize] = HandlerType::Args3(Self::execute_div_f32);
        vm.handlers[OpCode::MulF32 as usize] = HandlerType::Args3(Self::execute_mul_f32);

        // Call, enter, ret
        vm.handlers[OpCode::Call as usize] = HandlerType::Args1(Self::execute_call);
        vm.handlers[OpCode::Enter as usize] = HandlerType::Args1(Self::execute_enter);
        vm.handlers[OpCode::Ret as usize] = HandlerType::Args0(Self::execute_ret);

        vm.handlers[OpCode::HostCall as usize] = HandlerType::Args3(Self::execute_host_call);

        // Halt - return to host
        vm.handlers[OpCode::Hlt as usize] = HandlerType::Args0(Self::execute_hlt);

        // String
        /* TODO: BRING THIS BACK
        vm.handlers[OpCode::StringAppend as usize] =
            HandlerType::Args3(Self::execute_string_append);

         */

        // Int
        vm.handlers[OpCode::IntToRnd as usize] =
            HandlerType::Args2(Self::execute_pseudo_random_i32);
        vm.handlers[OpCode::IntMin as usize] = HandlerType::Args3(Self::execute_min_i32);
        vm.handlers[OpCode::IntMax as usize] = HandlerType::Args3(Self::execute_max_i32);
        vm.handlers[OpCode::IntClamp as usize] = HandlerType::Args4(Self::execute_clamp_i32);

        vm.handlers[OpCode::IntAbs as usize] = HandlerType::Args2(Self::execute_abs_i32);

        vm.handlers[OpCode::IntToString as usize] = HandlerType::Args2(Self::execute_i32_to_string);
        vm.handlers[OpCode::IntToFloat as usize] = HandlerType::Args2(Self::execute_i32_to_f32);

        // Float (Fixed Point)
        vm.handlers[OpCode::FloatPseudoRandom as usize] =
            HandlerType::Args2(Self::execute_pseudo_random_i32);
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
        // vm.handlers[OpCode::FloatAtan2 as usize] = HandlerType::Args3(Self::execute_f32_atan2); // TODO:
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
        vm.handlers[OpCode::MapLen as usize] = HandlerType::Args2(Self::execute_map_len);
         */

        // Other ==========
        // Unwrap
        /*
        vm.handlers[OpCode::UnwrapJmpNone as usize] =
            HandlerType::Args3(Self::execute_unwrap_jmp_none);
        vm.handlers[OpCode::UnwrapJmpSome as usize] =
            HandlerType::Args3(Self::execute_unwrap_jmp_some);

         */

        //assert_eq!(vm.handlers.len(), OpCode::HostCall as usize);

        // Optional: Zero out the memory for safety?

        vm
    }
    #[must_use]
    pub const fn memory(&self) -> &Memory {
        &self.memory
    }

    pub fn memory_mut(&mut self) -> &mut Memory {
        &mut self.memory
    }

    pub fn execute_internal(&mut self) {
        self.execution_complete = false;
        self.flags.z = false;
        self.call_stack.clear();
        self.memory.reset_offset();

        #[cfg(feature = "debug_vm")]
        if self.debug_enabled {
            eprintln!(
                "start executing --------- frame {:X} heap: {:X}",
                self.memory.frame_offset, self.memory.heap_alloc_offset
            );
        }

        self.call_stack.push(CallFrame {
            return_address: 1,
            previous_frame_offset: 0,
            previous_stack_offset: 0,
        });

        while !self.execution_complete {
            let instruction = &self.instructions[self.pc];
            let opcode = instruction.opcode;

            #[cfg(feature = "debug_vm")]
            if self.debug_enabled {
                let operands = instruction.operands;
                eprint!("> {:04X}: ", self.pc);
                self.debug_opcode(opcode, &operands);
                self.debug.opcodes_executed += 1;

                //    let s = hexify::format_hex(&self.frame_memory()[..16]);
                //  eprintln!("mem: {s}");
            }

            self.pc += 1; // IP must be added BEFORE handling the instruction

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
        }
    }

    pub fn execute_from_ip(&mut self, ip: &InstructionPosition) {
        self.pc = ip.0 as usize;
        self.execute_internal();
    }

    fn execute_unimplemented(&mut self) {
        let unknown_opcode = OpCode::from(self.instructions[self.pc].opcode);
        eprintln!("error: opcode not implemented: {unknown_opcode} {unknown_opcode:?}");
        eprintln!("VM runtime halted.");
        self.debug_output();
        panic!("unknown OPCODE! {unknown_opcode} {unknown_opcode:?}");
    }

    pub fn stack_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.memory.stack_ptr(), self.memory.memory_size) }
    }

    pub fn frame_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.memory.frame_ptr(), self.memory.memory_size) }
    }

    pub fn heap_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.memory.get_heap_ptr(0), self.memory.memory_size) }
    }

    pub fn constant_size(&self) -> usize {
        self.memory.constant_memory_size
    }
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn instructions(&self) -> &[BinaryInstruction] {
        &self.instructions
    }
    pub fn reset(&mut self) {
        self.memory.reset();
        self.memory.reset_allocator();

        self.pc = 0;
        self.execution_complete = false;
        self.call_stack.clear();
    }

    pub fn reset_stack_and_heap_to_constant_limit(&mut self) {
        self.memory.reset_allocator();
        self.reset_frame();
        self.execution_complete = false;
        self.pc = 0;

        #[cfg(feature = "debug_vm")]
        {
            self.reset_debug();
        }
    }

    pub fn protect_heap_up_to_current_allocator(&mut self) {
        self.memory.protect_up_to_allocator();
    }

    pub fn reset_frame(&mut self) {
        self.memory.reset();
        self.call_stack.clear();
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
        self.memory.frame_offset
    }

    pub fn load_bytecode(&mut self, instructions: Vec<BinaryInstruction>) {
        self.instructions = instructions;
        self.pc = 0;
        self.execution_complete = false;
    }

    #[inline]
    fn execute_ld32(&mut self, dst_reg: u8, a: u8, b: u8, c: u8, d: u8) {
        set_reg!(self, dst_reg, Self::u8s_to_32(a, b, c, d));
    }

    #[inline]
    fn execute_ldi32(&mut self, dst_reg: u8, a: u8, b: u8, c: u8, d: u8) {
        set_reg!(self, dst_reg, Self::u8s_to_32(a, b, c, d) as i32);
    }
    #[inline]
    fn execute_ld8(&mut self, dst_reg: u8, octet: u8) {
        set_reg!(self, dst_reg, octet);
    }

    /*
    #[inline]
    pub fn execute_unwrap_jmp_some(&mut self, wrapped_ptr_reg: u8, jmp_ip_0: u8, jmp_ip_1: u8) {
        get_reg!(self, wrapped_ptr_reg, Ptr => ptr_addr);
        let ptr = self.memory.get_heap_const_ptr(ptr_addr as usize);
        unsafe {
            if *ptr != 0 {
                self.ip = u8s_to_u16!(jmp_ip_0, jmp_ip_1) as usize;
            }
        }
    }

    #[inline]
    pub fn execute_unwrap_jmp_none(&mut self, wrapped_ptr_reg: u8, jmp_ip_0: u8, jmp_ip_1: u8) {
        get_reg!(self, wrapped_ptr_reg, Ptr => ptr_addr);
        let ptr = self.memory.get_heap_const_ptr(ptr_addr as usize);
        unsafe {
            if *ptr == 0 {
                self.ip = u8s_to_u16!(jmp_ip_0, jmp_ip_1) as usize;
            }
        }
    }

     */

    #[inline]
    fn execute_alloc(&mut self, dst_reg: u8, size_0: u8, size_1: u8) {
        let memory_size = u8s_to_u16!(size_0, size_1);
        let data_ptr = self.memory.heap_allocate(memory_size as usize);
        set_reg!(self, dst_reg, data_ptr);
    }

    // Fixed Point special methods
    #[inline]
    fn execute_mul_f32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = Fp::from(get_reg!(self, lhs_reg) as i16);
        let rhs = Fp::from(get_reg!(self, rhs_reg) as i16);
        set_reg!(self, dst_reg, (lhs * rhs).inner());
    }

    #[inline]
    fn execute_div_f32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = Fp::from(get_reg!(self, lhs_reg) as i16);
        let rhs = Fp::from(get_reg!(self, rhs_reg) as i16);

        set_reg!(self, dst_reg, (lhs / rhs).inner());
    }

    #[inline]
    fn execute_f32_round(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);

        set_reg!(self, dst_reg, val.round().inner());
    }

    #[inline]
    fn execute_f32_floor(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);

        set_reg!(self, dst_reg, val.floor().inner());
    }

    #[inline]
    fn execute_f32_sqrt(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);

        set_reg!(self, dst_reg, val.sqrt().inner());
    }

    #[inline]
    fn execute_f32_sin(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);

        set_reg!(self, dst_reg, val.sin().inner());
    }

    #[inline]
    fn execute_f32_asin(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);

        set_reg!(self, dst_reg, val.asin().inner());
    }

    #[inline]
    fn execute_f32_cos(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);

        set_reg!(self, dst_reg, val.cos().inner());
    }

    #[inline]
    fn execute_f32_acos(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);

        set_reg!(self, dst_reg, val.acos().inner());
    }

    /*
    #[inline]
    fn execute_f32_atan2(&mut self, dst_reg: u8, val_reg: u8, y_reg: u8) {
        // TODO: Implement atan2 in fixed32
        todo!()
    }

     */

    #[inline]
    fn execute_f32_to_string(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);

        set_reg!(self, dst_reg, self.create_string(&val.to_string()));
    }

    #[inline]
    fn execute_f32_sign(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);
        // TODO: signum() is/was incorrect in Fixed32 crate
        set_reg!(
            self,
            dst_reg,
            Fp::from(if val < 0 {
                -1
            } else if val > 0 {
                1
            } else {
                0
            })
            .inner()
        );
    }

    #[inline]
    fn execute_neg_i32(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from(get_reg!(self, val_reg) as i16);

        let result_option = val.inner().checked_neg();

        match result_option {
            Some(result) => {
                set_reg!(self, dst_reg, result);
            }
            None => {
                panic!(
                    "VM Runtime Error: Signed 32-bit integer overflow during NEG_I32 (R{} = R{})",
                    dst_reg, val_reg,
                );
            }
        }
    }

    #[inline]
    const fn execute_add_u32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg);
        let rhs = get_reg!(self, rhs_reg);

        set_reg!(self, dst_reg, lhs.wrapping_add(rhs));
    }

    #[inline]
    const fn execute_mul_u32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg);
        let rhs = get_reg!(self, rhs_reg);

        set_reg!(self, dst_reg, lhs.wrapping_mul(rhs));
    }

    #[inline]
    const fn execute_sub_u32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg);
        let rhs = get_reg!(self, rhs_reg);

        set_reg!(self, dst_reg, lhs.wrapping_sub(rhs));
    }

    #[inline]
    fn execute_mod_i32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;

        let result = ((lhs % rhs) + rhs) % rhs;
        set_reg!(self, dst_reg, result);
    }

    #[inline]
    fn execute_div_i32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;

        let result_option = lhs.checked_div(rhs);

        match result_option {
            Some(result) => {
                set_reg!(self, dst_reg, result);
            }
            None => {
                panic!(
                    "VM Runtime Error: Signed 32-bit integer overflow during DIV_I32 (R{} = R{} - R{})",
                    dst_reg, lhs_reg, rhs_reg
                );
            }
        }
    }

    #[inline]
    fn execute_lt_i32(&mut self, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;
        self.flags.z = lhs < rhs;
    }

    #[inline]
    fn execute_le_i32(&mut self, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;
        self.flags.z = lhs <= rhs;
    }

    #[inline]
    fn execute_gt_i32(&mut self, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;
        self.flags.z = lhs > rhs;
    }

    #[inline]
    fn execute_ge_i32(&mut self, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;

        self.flags.z = lhs >= rhs;
    }

    #[inline]
    fn execute_pseudo_random_i32(&mut self, dst_reg: u8, src_reg: u8) {
        let src = get_reg!(self, src_reg);
        set_reg!(
            self,
            dst_reg,
            squirrel_prng::squirrel_noise5(src as u32, 0) as i32
        );
    }

    #[inline]
    fn execute_i32_to_string(&mut self, dst_reg: u8, val_reg: u8) {
        let val = get_reg!(self, val_reg) as i32;

        set_reg!(self, dst_reg, self.create_string(&val.to_string()));
    }

    #[inline]
    fn execute_i32_to_f32(&mut self, float_dest_reg: u8, int_source_reg: u8) {
        let int_source = get_reg!(self, int_source_reg) as i32;
        set_reg!(self, float_dest_reg, Fp::from(int_source as i16).inner());
    }

    #[inline]
    fn execute_abs_i32(&mut self, dst_reg: u8, val_reg: u8) {
        let val = get_reg!(self, val_reg) as i32;
        set_reg!(self, dst_reg, if val < 0 { -val } else { val });
    }

    #[inline]
    fn execute_min_i32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;

        set_reg!(self, dst_reg, if lhs < rhs { lhs } else { rhs });
    }

    #[inline]
    fn execute_max_i32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;

        set_reg!(self, dst_reg, if lhs > rhs { lhs } else { rhs });
    }

    #[inline]
    fn execute_clamp_i32(&mut self, dst_reg: u8, val_reg: u8, min_reg: u8, max_reg: u8) {
        let val = get_reg!(self, val_reg) as i32;
        let min_val = get_reg!(self, min_reg) as i32;
        let max_val = get_reg!(self, max_reg) as i32;

        set_reg!(
            self,
            dst_reg,
            if val < min_val {
                min_val
            } else if val > max_val {
                max_val
            } else {
                val
            }
        );
    }

    #[inline]
    fn execute_cmp_reg(&mut self, lhs_reg: u8, rhs_reg: u8) {
        self.flags.z = self.registers[lhs_reg as usize] == self.registers[rhs_reg as usize];
    }

    #[inline]
    fn execute_eq_8_imm(&mut self, val_reg: u8, octet: u8) {
        let compare = get_reg!(self, val_reg);
        self.flags.z = compare == octet as u32;
    }

    #[inline]
    fn execute_tst8(&mut self, val_reg: u8) {
        let val = get_reg!(self, val_reg);
        self.flags.z = val != 0;
    }

    #[inline]
    fn execute_not_z(&mut self) {
        self.flags.z = !self.flags.z;
    }

    #[inline]
    fn execute_st_z(&mut self, dst_reg: u8) {
        set_reg!(self, dst_reg, self.flags.z);
    }

    #[inline]
    fn execute_st_nz(&mut self, dst_reg: u8) {
        set_reg!(self, dst_reg, !self.flags.z);
    }

    #[inline]
    const fn execute_bnz(&mut self, branch_offset_0: u8, branch_offset_1: u8) {
        if !self.flags.z {
            self.pc =
                (self.pc as i32 + i16_from_u8s!(branch_offset_0, branch_offset_1) as i32) as usize;
        }
    }

    #[inline]
    const fn execute_bz(&mut self, branch_offset_0: u8, branch_offset_1: u8) {
        if self.flags.z {
            self.pc =
                (self.pc as i32 + i16_from_u8s!(branch_offset_0, branch_offset_1) as i32) as usize;
        }
    }

    #[inline]
    fn execute_jmp(&mut self, ip_0: u8, ip_1: u8, ip_2: u8, ip_3: u8) {
        self.pc = u32_from_u8s!(ip_0, ip_1, ip_2, ip_3) as usize;
    }

    #[inline]
    fn execute_hlt(&mut self) {
        self.execution_complete = true;
        #[cfg(feature = "debug_vm")]
        if self.debug_enabled {
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
    fn execute_mov_reg(&mut self, dst_reg: u8, src_reg: u8) {
        self.registers[dst_reg as usize] = self.registers[src_reg as usize];
    }

    #[inline]
    fn execute_lea(&mut self, dst_reg: u8, src_offset_0: u8, src_offset_1: u8) {
        let ptr_addr = self.memory.frame_offset as u32;
        set_reg!(
            self,
            dst_reg,
            ptr_addr + u8s_to_u16!(src_offset_0, src_offset_1) as u32
        );
    }

    #[inline]
    fn execute_mov_mem(
        &mut self,
        dst_pointer_reg: u8,
        src_pointer_reg: u8,
        memory_size_lower: u8,
        memory_size_upper: u8,
    ) {
        let dest_addr = get_reg!(self, dst_pointer_reg);
        let src_addr = get_reg!(self, src_pointer_reg);

        let memory_size = u8s_to_u16!(memory_size_lower, memory_size_upper);

        let dst_ptr = self.memory.get_heap_ptr(dest_addr as usize);
        let src_ptr = self.memory.get_heap_const_ptr(src_addr as usize);

        unsafe {
            ptr::copy_nonoverlapping(src_ptr, dst_ptr, memory_size as usize);
        }
    }

    #[cfg(feature = "debug_vm")]
    pub fn debug_opcode(&self, opcode: u8, operands: &[u8; 8]) {
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

    fn execute_call(&mut self, target: u8) {
        let return_info = CallFrame {
            return_address: self.pc + 1,
            previous_frame_offset: self.memory.frame_offset,
            previous_stack_offset: self.memory.stack_offset,
        };

        self.memory.set_fp();
        self.call_stack.push(return_info);
        self.pc = target as usize;

        #[cfg(feature = "debug_vm")]
        if self.debug_enabled {
            self.debug.call_depth += 1;
            if self.debug.call_depth > self.debug.max_call_depth {
                self.debug.max_call_depth = self.debug.call_depth;
            }
        }
    }

    #[inline]
    fn execute_host_call(
        &mut self,
        function_id_lower: u8,
        function_id_upper: u8,
        bytes_to_copy_from_frame_ptr: u8,
    ) {
        let heap = self.memory();

        let host_args = HostArgs::new(
            heap.memory,
            heap.memory_size,
            heap.stack_offset,
            bytes_to_copy_from_frame_ptr as usize,
        );

        let function_id = u8s_to_u16!(function_id_lower, function_id_upper);

        if let Some(callback) = self.host_functions.get_mut(&function_id) {
            callback(host_args);
        } else {
            panic!("problem");
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    #[inline(always)]
    fn execute_enter(&mut self, frame_size_for_local_variables: u8) {
        self.memory.inc_sp(frame_size_for_local_variables as usize);
    }

    #[inline]
    fn execute_ret(&mut self) {
        let call_frame = self.call_stack.pop().unwrap();

        self.memory.pop(
            call_frame.previous_frame_offset,
            call_frame.previous_stack_offset,
        );

        // going back to the old instruction
        self.pc = call_frame.return_address;
        self.pc -= 1; // Adjust for automatic increment

        // NOTE: Any return value is always at frame_offset + 0

        #[cfg(feature = "debug_vm")]
        if self.debug_enabled {
            self.debug.call_depth -= 1;
        }
    }

    #[inline]
    fn u8s_to_32(a: u8, b: u8, c: u8, d: u8) -> u32 {
        (a as u32) << 24 | (b as u32) << 16 | (c as u32) << 8 | (d as u32)
    }

    pub fn get_const_ptr_from_reg(&self, reg: u8) -> *const u8 {
        let ptr_addr = get_reg!(self, reg);
        self.memory.get_heap_const_ptr(ptr_addr as usize)
    }

    pub fn get_ptr_from_reg(&self, reg: u8) -> *mut u8 {
        let ptr_addr = get_reg!(self, reg);
        self.memory.get_heap_ptr(ptr_addr as usize)
    }
}
