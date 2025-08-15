/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
extern crate core;

use crate::host::{HostArgs, HostFunctionCallback};
use crate::memory::ExecutionMode::NormalExecution;
use crate::memory::{Memory, MemoryDebug};
use crate::VmState::Normal;
use fixed32::Fp;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::mem::discriminant;
use std::ptr;
use std::str::FromStr;
use swamp_vm_isa::opcode::OpCode;
use swamp_vm_isa::{BinaryInstruction, InstructionPosition};

mod grid;
pub mod host;
pub mod map_open;
pub mod memory;
pub mod prelude;
mod range;
mod sparse;
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
        (($msb3 as u32) << 24) | (($msb2 as u32) << 16) | (($msb as u32) << 8) | ($lsb as u32)
    };
}

#[macro_export]
macro_rules! u16_from_u8s {
    ($lsb:expr, $msb:expr) => {
        (($msb as u16) << 8) | ($lsb as u16)
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
        $vm.registers[$reg_idx as usize] = $value as u32
    };
}

type Handler0 = fn(&mut Vm);
type Handler1 = fn(&mut Vm, u8);
type Handler2 = fn(&mut Vm, u8, u8);
type Handler3 = fn(&mut Vm, u8, u8, u8);
type Handler4 = fn(&mut Vm, u8, u8, u8, u8);
type Handler5 = fn(&mut Vm, u8, u8, u8, u8, u8);
type Handler6 = fn(&mut Vm, u8, u8, u8, u8, u8, u8);
type Handler7 = fn(&mut Vm, u8, u8, u8, u8, u8, u8, u8);
type Handler8 = fn(&mut Vm, u8, u8, u8, u8, u8, u8, u8, u8);

#[derive(Copy, Clone)]
enum HandlerType {
    Args0(Handler0),
    Args1(Handler1),
    Args2(Handler2),
    Args3(Handler3),
    Args4(Handler4),
    Args5(Handler5),
    Args6(Handler6),
    Args7(Handler7),
    Args8(Handler8),
}

#[derive(Debug, Default)]
pub struct Debug {
    pub opcodes_executed: usize,
    pub call_depth: usize,
    pub max_call_depth: usize,
    pub max_stack_offset: usize,
}

pub struct CallFrame {
    pub return_address: usize,        // Instruction to return to
    pub previous_frame_offset: usize, // Previous frame position
    pub previous_stack_offset: usize, // Size of this frame
}

type RegContents = u32;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum TrapCode {
    StoppedByTestHarness,
    VecBoundsFail {
        encountered: usize,
        element_count: usize,
    },
    MapOutOfSpace,
    MapEntryNotFound,
    MapEntryNotFoundAndCouldNotBeCreated,
    MapEntryNotFoundForRemoval,
    LessThanTrap {
        a: u32,
        b: u32,
    },
    SparseOutOfSpace,
    SparseRemoveFailed,
    SparseGetFailed,
    MapCouldNotBeCopied,
    OverlappingMemoryCopy,
    MemoryCorruption,
    VecOutOfCapacity {
        encountered: u16,
        capacity: u16,
    },
    VecEmpty,
    VecNeverInitialized,
    GridBoundsXFail {
        x: u32,
        width: u16,
    },
    GridBoundsYFail {
        y: u32,
        height: u16,
    },
    GridBoundsFail,
    InvalidUtf8Sequence,
    UnalignedAccess,
    ReverseRangeNotAllowedHere,
    U8CheckFailed,
    Misaligned,
}

impl TrapCode {
    pub fn is_sort_of_equal(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }
}

impl TryFrom<u8> for TrapCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let code = match value {
            0 => Self::StoppedByTestHarness,
            1 => Self::VecBoundsFail {
                encountered: 0,
                element_count: 0,
            }, // TODO: Fix this
            2 => Self::MapOutOfSpace,
            3 => Self::MapEntryNotFound,
            4 => Self::MapEntryNotFoundAndCouldNotBeCreated,
            5 => Self::MapEntryNotFoundForRemoval,
            6 => Self::LessThanTrap { a: 0, b: 0 },
            7 => Self::SparseOutOfSpace,
            8 => Self::SparseRemoveFailed,
            9 => Self::SparseGetFailed,
            10 => Self::MapCouldNotBeCopied,
            11 => Self::OverlappingMemoryCopy,
            _ => return Err(()),
        };
        Ok(code)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseTrapCodeError;

impl Display for ParseTrapCodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Unable to parse string into a valid TrapCode")
    }
}
impl Error for ParseTrapCodeError {}
impl FromStr for TrapCode {
    type Err = ParseTrapCodeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let code = match s {
            "stopped_by_test_harness" => Self::StoppedByTestHarness,
            "vec_bounds_fail" => Self::VecBoundsFail {
                encountered: 0,
                element_count: 0,
            }, // TODO: FIX
            "vec_out_of_capacity" => Self::VecOutOfCapacity {
                encountered: 0,
                capacity: 0,
            }, // TODO: FIX
            "reverse_range_not_allowed_here" => Self::ReverseRangeNotAllowedHere,
            "map_out_of_space" => Self::MapOutOfSpace,
            "map_entry_not_found" => Self::MapEntryNotFound,
            "map_entry_or_create_failed" => Self::MapEntryNotFoundAndCouldNotBeCreated,
            "map_entry_remove_failed" => Self::MapEntryNotFoundForRemoval,
            "less_than_trap" => Self::LessThanTrap { a: 0, b: 0 },
            "sparse_out_of_space" => Self::SparseOutOfSpace,
            "sparse_remove_failed" => Self::SparseRemoveFailed,
            "sparse_get_failed" => Self::SparseGetFailed,
            "map_could_not_be_copied" => Self::MapCouldNotBeCopied,
            "overlapping_memory_copy" => Self::OverlappingMemoryCopy,
            _ => return Err(ParseTrapCodeError),
        };

        Ok(code)
    }
}

impl Display for TrapCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "trap {self:?}")
    }
}

#[derive(Eq, Debug, PartialEq)]
pub enum VmState {
    Normal,
    Panic(String),
    Trap(TrapCode),
    Halt,
    Step,
}

pub struct Vm {
    // Memory
    memory: Memory,

    // Execution state
    pc: usize,                            // Instruction pointer
    instructions: Vec<BinaryInstruction>, // Bytecode
    execution_complete: bool,             // Flag for completion

    // Function call management
    call_stack: Vec<CallFrame>, // Track function calls

    handlers: [HandlerType; 256],

    pub registers: [u32; 256], // Normal CPUs have around 31 general purpose registers

    // TODO: Error state
    pub debug: Debug,
    pub debug_stats_enabled: bool,
    pub debug_opcodes_enabled: bool,
    pub debug_operations_enabled: bool,
    pub state: VmState,
}

impl Vm {
    #[must_use]
    pub const fn is_execution_complete(&self) -> bool {
        self.execution_complete
    }
}

const ALIGNMENT: usize = 8;
const ALIGNMENT_REST: usize = ALIGNMENT - 1;
const ALIGNMENT_MASK: usize = !ALIGNMENT_REST;

pub struct VmSetup {
    pub stack_memory_size: usize,
    pub heap_memory_size: usize,
    pub constant_memory: Vec<u8>,
    pub debug_stats_enabled: bool,
    pub debug_opcodes_enabled: bool,
    pub debug_operations_enabled: bool,
}

impl Vm {
    #[allow(clippy::too_many_lines)]
    pub fn new(instructions: Vec<BinaryInstruction>, setup: VmSetup) -> Self {
        let memory = Memory::new(
            &setup.constant_memory,
            setup.stack_memory_size,
            setup.heap_memory_size,
        );

        assert!(
            setup.constant_memory.len() < setup.stack_memory_size / 2,
            "too much constant memory"
        );

        let mut vm = Self {
            memory, // Raw memory pointer
            pc: 0,
            instructions,
            execution_complete: false,
            call_stack: vec![],
            handlers: [const { HandlerType::Args0(Self::execute_unimplemented) }; 256],
            registers: [const { 0 }; 256],
            debug: Debug {
                opcodes_executed: 0,
                call_depth: 0,
                max_call_depth: 0,
                max_stack_offset: 0,
            },
            debug_stats_enabled: setup.debug_stats_enabled,
            debug_opcodes_enabled: setup.debug_opcodes_enabled,
            debug_operations_enabled: setup.debug_operations_enabled,
            state: Normal,
        };

        /*
            TODO: @idea: Instead of storing function pointers, the instructions vector itself
            includes the pointer to the instruction's handler code.

            type HandlerPointer = fn(&mut Vm, &FixedSizeOperandBlock);
            type FixedSizeOperandBlock = [u8; 8];

            BinaryInstruction has a field for HandlerPointer.
        */

        //vm.handlers[OpCode::Alloc as usize] = HandlerType::Args3(Self::execute_alloc);

        // Store
        vm.handlers[OpCode::StRegToFrame as usize] =
            HandlerType::Args6(Self::execute_st_regs_to_frame);
        vm.handlers[OpCode::StRegToFrameUsingMask as usize] =
            HandlerType::Args5(Self::execute_st_regs_to_frame_using_mask);

        vm.handlers[OpCode::St32UsingPtrWithOffset as usize] =
            HandlerType::Args6(Self::execute_stw_using_base_ptr_and_offset);
        vm.handlers[OpCode::St16UsingPtrWithOffset as usize] =
            HandlerType::Args6(Self::execute_sth_using_base_ptr_and_offset);
        vm.handlers[OpCode::St8UsingPtrWithOffset as usize] =
            HandlerType::Args6(Self::execute_stb_using_base_ptr_and_offset);

        // Load
        vm.handlers[OpCode::LdRegFromFrameRange as usize] =
            HandlerType::Args6(Self::execute_ld_regs_from_frame);
        vm.handlers[OpCode::LdRegFromFrameUsingMask as usize] =
            HandlerType::Args5(Self::execute_ld_regs_from_frame_using_mask);

        vm.handlers[OpCode::Ld32FromPointerWithOffset as usize] =
            HandlerType::Args6(Self::execute_ldw_from_base_ptr_and_offset);
        vm.handlers[OpCode::Ld16FromPointerWithOffset as usize] =
            HandlerType::Args6(Self::execute_ldh_from_base_ptr_and_offset);
        vm.handlers[OpCode::Ld8FromPointerWithOffset as usize] =
            HandlerType::Args6(Self::execute_ldb_from_base_ptr_and_offset);

        // Load immediate
        vm.handlers[OpCode::Mov8FromImmediateValue as usize] =
            HandlerType::Args2(Self::execute_mov_8);
        vm.handlers[OpCode::Mov16FromImmediateValue as usize] =
            HandlerType::Args3(Self::execute_mov_16);
        vm.handlers[OpCode::Mov32FromImmediateValue as usize] =
            HandlerType::Args5(Self::execute_mov_32);

        // Copy data in frame memory
        vm.handlers[OpCode::MovReg as usize] = HandlerType::Args2(Self::execute_mov_reg);
        vm.handlers[OpCode::LdPtrFromEffectiveFrameAddress as usize] =
            HandlerType::Args5(Self::execute_lea);

        vm.handlers[OpCode::Ld32FromAbsoluteAddress as usize] =
            HandlerType::Args5(Self::execute_ldw_from_absolute_address);

        vm.handlers[OpCode::Ld8FromAbsoluteAddress as usize] =
            HandlerType::Args5(Self::execute_ldb_from_absolute_address);

        // Copy to and from heap
        vm.handlers[OpCode::BlockCopy as usize] =
            HandlerType::Args6(Self::execute_mov_mem_with_immediate_size);

        vm.handlers[OpCode::FrameMemClr as usize] =
            HandlerType::Args8(Self::execute_frame_memory_clear);

        // Comparisons - Int
        vm.handlers[OpCode::LtI32 as usize] = HandlerType::Args3(Self::execute_lt_i32);
        vm.handlers[OpCode::LeI32 as usize] = HandlerType::Args3(Self::execute_le_i32);
        vm.handlers[OpCode::GtI32 as usize] = HandlerType::Args3(Self::execute_gt_i32);
        vm.handlers[OpCode::GeI32 as usize] = HandlerType::Args3(Self::execute_ge_i32);

        // Comparison u32
        vm.handlers[OpCode::LtU32 as usize] = HandlerType::Args3(Self::execute_lt_u32);
        vm.handlers[OpCode::LeU32 as usize] = HandlerType::Args3(Self::execute_le_u32);
        vm.handlers[OpCode::GtU32 as usize] = HandlerType::Args3(Self::execute_gt_u32);
        vm.handlers[OpCode::GeU32 as usize] = HandlerType::Args3(Self::execute_ge_u32);

        // Comparison
        vm.handlers[OpCode::CmpReg as usize] = HandlerType::Args3(Self::execute_cmp_reg);
        vm.handlers[OpCode::CmpBlock as usize] = HandlerType::Args5(Self::execute_cmp_block);

        vm.handlers[OpCode::Eq8Imm as usize] = HandlerType::Args3(Self::execute_eq_8_imm);
        vm.handlers[OpCode::TrapOnLessThan as usize] =
            HandlerType::Args2(Self::execute_trap_on_less_than);

        // Logical Operations
        vm.handlers[OpCode::MovEqualToZero as usize] =
            HandlerType::Args2(Self::execute_move_equal_to_zero);

        // Conditional jumps
        vm.handlers[OpCode::BFalse as usize] = HandlerType::Args3(Self::execute_branch_if_false);
        vm.handlers[OpCode::BTrue as usize] = HandlerType::Args3(Self::execute_branch_if_true);

        // Unconditional jump
        vm.handlers[OpCode::B as usize] = HandlerType::Args2(Self::execute_b);

        // Operators - Int
        vm.handlers[OpCode::AddU32 as usize] = HandlerType::Args3(Self::execute_add_u32);
        vm.handlers[OpCode::AddU32Imm as usize] = HandlerType::Args6(Self::execute_add_u32_imm);
        vm.handlers[OpCode::MulU32 as usize] = HandlerType::Args3(Self::execute_mul_u32);
        vm.handlers[OpCode::SubU32 as usize] = HandlerType::Args3(Self::execute_sub_u32);

        vm.handlers[OpCode::NegI32 as usize] = HandlerType::Args2(Self::execute_neg_i32);
        vm.handlers[OpCode::ModI32 as usize] = HandlerType::Args3(Self::execute_mod_i32);
        vm.handlers[OpCode::DivI32 as usize] = HandlerType::Args3(Self::execute_div_i32);

        // Operators - Float (Fixed Point)
        vm.handlers[OpCode::DivF32 as usize] = HandlerType::Args3(Self::execute_div_f32);
        vm.handlers[OpCode::MulF32 as usize] = HandlerType::Args3(Self::execute_mul_f32);

        // Call, enter, ret
        vm.handlers[OpCode::Call as usize] = HandlerType::Args4(Self::execute_call);
        vm.handlers[OpCode::Enter as usize] = HandlerType::Args4(Self::execute_enter);
        vm.handlers[OpCode::Ret as usize] = HandlerType::Args0(Self::execute_ret);

        //vm.handlers[OpCode::HostCall as usize] = HandlerType::Args3(Self::execute_host_call);

        // Halt - return to host
        vm.handlers[OpCode::Hlt as usize] = HandlerType::Args0(Self::execute_hlt);
        vm.handlers[OpCode::UserHalt as usize] = HandlerType::Args0(Self::execute_user_halt);
        vm.handlers[OpCode::Step as usize] = HandlerType::Args0(Self::execute_step);
        vm.handlers[OpCode::Trap as usize] = HandlerType::Args1(Self::execute_trap);
        vm.handlers[OpCode::Panic as usize] = HandlerType::Args1(Self::execute_panic);

        vm.handlers[OpCode::CheckU8 as usize] = HandlerType::Args1(Self::execute_check_u8);

        // Codepoint
        vm.handlers[OpCode::CodepointToString as usize] =
            HandlerType::Args2(Self::execute_codepoint_to_string);

        // Bool
        vm.handlers[OpCode::BoolToString as usize] =
            HandlerType::Args2(Self::execute_bool_to_string);

        // String
        vm.handlers[OpCode::StringAppend as usize] =
            HandlerType::Args3(Self::execute_string_append);

        vm.handlers[OpCode::StringDuplicate as usize] =
            HandlerType::Args2(Self::execute_string_duplicate);
        vm.handlers[OpCode::BytesToString as usize] =
            HandlerType::Args2(Self::execute_string_from_bytes);
        vm.handlers[OpCode::BytesToStringStorage as usize] =
            HandlerType::Args2(Self::execute_string_storage_from_bytes);

        vm.handlers[OpCode::StringRepeat as usize] =
            HandlerType::Args3(Self::execute_string_repeat);

        vm.handlers[OpCode::StringCmp as usize] = HandlerType::Args3(Self::execute_string_cmp);
        vm.handlers[OpCode::StringToString as usize] =
            HandlerType::Args2(Self::execute_string_to_string);
        vm.handlers[OpCode::StringStartsWith as usize] =
            HandlerType::Args3(Self::execute_string_starts_with);
        vm.handlers[OpCode::StringToInt as usize] = HandlerType::Args2(Self::execute_string_to_int);
        vm.handlers[OpCode::StringToFloat as usize] =
            HandlerType::Args2(Self::execute_string_to_float);

        vm.handlers[OpCode::StringIterInit as usize] =
            HandlerType::Args2(Self::execute_string_iter_init);
        vm.handlers[OpCode::StringIterNext as usize] =
            HandlerType::Args4(Self::execute_string_iter_next);
        vm.handlers[OpCode::StringIterNextPair as usize] =
            HandlerType::Args5(Self::execute_string_iter_next_pair);

        vm.handlers[OpCode::ByteToString as usize] =
            HandlerType::Args2(Self::execute_byte_to_string);

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
        vm.handlers[OpCode::FloatPseudoRandom as usize] =
            HandlerType::Args2(Self::execute_pseudo_random_i32);

        // Collections ==========

        // Range
        vm.handlers[OpCode::RangeInit as usize] = HandlerType::Args4(Self::execute_range_init);
        vm.handlers[OpCode::RangeIterInit as usize] =
            HandlerType::Args2(Self::execute_range_iter_init);
        vm.handlers[OpCode::RangeIterNext as usize] =
            HandlerType::Args4(Self::execute_range_iter_next);

        // Array
        vm.handlers[OpCode::ArrayInitWithLenAndCapacity as usize] =
            HandlerType::Args7(Self::execute_array_init);

        // Vec
        vm.handlers[OpCode::VecInit as usize] = HandlerType::Args7(Self::execute_vec_init);
        vm.handlers[OpCode::VecCopy as usize] = HandlerType::Args2(Self::execute_vec_copy);
        vm.handlers[OpCode::VecCopyRange as usize] =
            HandlerType::Args3(Self::execute_vec_copy_range);
        vm.handlers[OpCode::VecCmp as usize] = HandlerType::Args3(Self::execute_vec_cmp);
        vm.handlers[OpCode::VecIterInit as usize] = HandlerType::Args2(Self::execute_vec_iter_init);
        vm.handlers[OpCode::VecIterNext as usize] = HandlerType::Args4(Self::execute_vec_iter_next);
        vm.handlers[OpCode::VecIterNextPair as usize] =
            HandlerType::Args5(Self::execute_vec_iter_next_pair);
        vm.handlers[OpCode::VecPushAddr as usize] = HandlerType::Args2(Self::execute_vec_push_addr);
        vm.handlers[OpCode::VecExtend as usize] = HandlerType::Args2(Self::execute_vec_extend);
        vm.handlers[OpCode::VecGet as usize] = HandlerType::Args3(Self::execute_vec_get);
        vm.handlers[OpCode::VecPop as usize] = HandlerType::Args2(Self::execute_vec_pop);
        vm.handlers[OpCode::VecRemoveIndex as usize] =
            HandlerType::Args2(Self::execute_vec_remove_index);

        // Map
        vm.handlers[OpCode::MapInitWithCapacityAndKeyAndTupleSizeAddr as usize] =
            HandlerType::Args7(Self::execute_map_open_addressing_init);
        vm.handlers[OpCode::MapIterInit as usize] = HandlerType::Args2(Self::execute_map_iter_init);
        vm.handlers[OpCode::MapIterNext as usize] = HandlerType::Args4(Self::execute_map_iter_next);
        vm.handlers[OpCode::MapIterNextPair as usize] =
            HandlerType::Args5(Self::execute_map_iter_next_pair);
        vm.handlers[OpCode::MapGetEntryLocation as usize] =
            HandlerType::Args3(Self::execute_map_open_addressing_get_entry_location);
        vm.handlers[OpCode::MapGetOrReserveEntryLocation as usize] =
            HandlerType::Args3(Self::execute_map_open_addressing_get_or_reserve_entry);
        vm.handlers[OpCode::MapHas as usize] =
            HandlerType::Args3(Self::execute_map_open_addressing_has);
        vm.handlers[OpCode::MapRemove as usize] =
            HandlerType::Args2(Self::execute_map_open_addressing_remove);
        vm.handlers[OpCode::MapOverwrite as usize] =
            HandlerType::Args2(Self::execute_map_overwrite);

        // Sparse
        vm.handlers[OpCode::SparseInit as usize] = HandlerType::Args7(Self::execute_sparse_init);
        vm.handlers[OpCode::SparseAddGiveEntryAddress as usize] =
            HandlerType::Args7(Self::execute_sparse_add_get_entry_addr);
        vm.handlers[OpCode::SparseRemove as usize] =
            HandlerType::Args2(Self::execute_sparse_remove);
        vm.handlers[OpCode::SparseGetEntryAddr as usize] =
            HandlerType::Args5(Self::execute_sparse_get_entry_addr);
        vm.handlers[OpCode::SparseIsAlive as usize] =
            HandlerType::Args3(Self::execute_sparse_is_alive);

        vm.handlers[OpCode::SparseIterInit as usize] =
            HandlerType::Args2(Self::execute_sparse_iter_init);
        vm.handlers[OpCode::SparseIterNext as usize] =
            HandlerType::Args4(Self::execute_sparse_iter_next);
        vm.handlers[OpCode::SparseIterNextPair as usize] =
            HandlerType::Args5(Self::execute_sparse_iter_next_pair);

        vm.handlers[OpCode::GridInit as usize] = HandlerType::Args6(Self::execute_grid_init);
        vm.handlers[OpCode::GridGetEntryAddr as usize] =
            HandlerType::Args6(Self::execute_grid_get_entry_addr);

        vm
    }
    #[must_use]
    pub const fn memory(&self) -> &Memory {
        &self.memory
    }

    pub fn memory_mut(&mut self) -> &mut Memory {
        &mut self.memory
    }

    pub fn step(&mut self, host_function_callback: &mut dyn HostFunctionCallback) -> bool {
        let instruction = &self.instructions[self.pc];
        let opcode = instruction.opcode;

        if self.memory.execution_mode == NormalExecution {
            assert!(self.memory.stack_offset >= self.memory.constant_memory_size);
            assert!(self.memory.stack_offset <= self.memory.heap_start);
        }

        self.pc += 1; // IP must be added BEFORE handling the instruction

        if opcode == OpCode::HostCall as u8 {
            self.execute_host_call(
                instruction.operands[0],
                instruction.operands[1],
                instruction.operands[2],
                host_function_callback,
            );
        } else {
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
                HandlerType::Args6(handler) => handler(
                    self,
                    instruction.operands[0],
                    instruction.operands[1],
                    instruction.operands[2],
                    instruction.operands[3],
                    instruction.operands[4],
                    instruction.operands[5],
                ),
                HandlerType::Args7(handler) => handler(
                    self,
                    instruction.operands[0],
                    instruction.operands[1],
                    instruction.operands[2],
                    instruction.operands[3],
                    instruction.operands[4],
                    instruction.operands[5],
                    instruction.operands[6],
                ),
                HandlerType::Args8(handler) => handler(
                    self,
                    instruction.operands[0],
                    instruction.operands[1],
                    instruction.operands[2],
                    instruction.operands[3],
                    instruction.operands[4],
                    instruction.operands[5],
                    instruction.operands[6],
                    instruction.operands[7],
                ),
            }
        }

        !self.execution_complete
    }

    //  Optimization ideas:
    // ```
    //    use swamp_vm_types::BinaryInstruction;
    //
    //    type Handler = fn(&mut VM, &BinaryInstruction, *mut u32);
    //
    //    pub fn run(&mut self) {
    //         let handlers = unsafe { self.decoded_handlers.get_unchecked(..) };
    //         let instrs   = unsafe { self.instructions.get_unchecked(..) };
    //         let regs_ptr = self.regs.as_mut_ptr();
    //
    //         while !self.execution_complete {
    //             let pc = self.pc;
    //             self.pc += 1;
    //
    //             let h: Handler = unsafe { *handlers.get_unchecked(pc) };
    //
    //             let inst: &BinaryInstruction = unsafe { &*instrs.as_ptr().add(pc) };
    //
    //             h(self, inst, regs_ptr);
    //         }
    //     }
    //```
    //
    #[allow(clippy::too_many_lines)]
    pub fn execute_internal(&mut self, host_function_callback: &mut dyn HostFunctionCallback) {
        self.execution_complete = false;

        let inst_ptr = self.instructions.as_ptr();

        while !self.execution_complete {
            let instruction: &BinaryInstruction = unsafe { &*inst_ptr.add(self.pc) };
            let opcode = instruction.opcode;

            #[cfg(feature = "debug_vm")]
            if self.debug_opcodes_enabled {
                let regs = [0, 1, 2, 3, 4, 128, 129, 130];

                for reg in regs {
                    print!(
                        "{}",
                        tinter::bright_black(&format!("{reg:02X}: {:08X}, ", self.registers[reg]))
                    );
                }
                println!();

                let operands = instruction.operands;
                print!("> {:04X}: ", self.pc);
                self.debug_opcode(opcode, &operands);
            }

            #[cfg(feature = "debug_vm")]
            if self.debug_stats_enabled {
                self.debug.opcodes_executed += 1;
            }

            self.pc += 1; // IP must be added BEFORE handling the instruction

            if opcode == OpCode::HostCall as u8 {
                self.execute_host_call(
                    instruction.operands[0],
                    instruction.operands[1],
                    instruction.operands[2],
                    host_function_callback,
                );
            } else {
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
                    HandlerType::Args6(handler) => handler(
                        self,
                        instruction.operands[0],
                        instruction.operands[1],
                        instruction.operands[2],
                        instruction.operands[3],
                        instruction.operands[4],
                        instruction.operands[5],
                    ),
                    HandlerType::Args7(handler) => handler(
                        self,
                        instruction.operands[0],
                        instruction.operands[1],
                        instruction.operands[2],
                        instruction.operands[3],
                        instruction.operands[4],
                        instruction.operands[5],
                        instruction.operands[6],
                    ),
                    HandlerType::Args8(handler) => handler(
                        self,
                        instruction.operands[0],
                        instruction.operands[1],
                        instruction.operands[2],
                        instruction.operands[3],
                        instruction.operands[4],
                        instruction.operands[5],
                        instruction.operands[6],
                        instruction.operands[7],
                    ),
                }
            }
        }
    }

    pub const fn set_return_register_address(&mut self, r0_addr: u32) {
        set_reg!(self, 0, r0_addr);
    }

    pub fn set_register_pointer_addr_for_parameter(&mut self, register: u8, addr: u32) {
        assert!((1..=6).contains(&register), "not a parameter register");
        set_reg!(self, register, addr);
    }

    pub fn set_stack_start(&mut self, addr: usize) {
        if self.debug_operations_enabled {
            eprintln!("vm: set stack start and frame to: 0x{addr:08X}");
        }
        self.memory.set_stack_and_frame(addr);
    }

    pub fn resume(&mut self, host_function_callback: &mut dyn HostFunctionCallback) {
        self.execute_internal(host_function_callback);
    }

    pub fn execute_from_ip(
        &mut self,
        ip: &InstructionPosition,
        host_function_callback: &mut dyn HostFunctionCallback,
    ) {
        self.pc = ip.0 as usize;
        if self.debug_operations_enabled {
            eprintln!(
                "starting up the vm, normal_stack_start: {:08X} SP:{:08X} FP:{:08X}",
                self.memory.stack_start, self.memory.stack_offset, self.memory.frame_offset
            );
        }

        self.call_stack.clear();
        self.memory.reset_offset();

        #[cfg(feature = "debug_vm")]
        if self.debug_opcodes_enabled {
            eprintln!(
                "start executing --------- frame {:X} heap: {:X}",
                self.memory.frame_offset, self.memory.heap_alloc_offset
            );
        }

        self.execute_internal(host_function_callback);
    }

    pub const fn set_pc(&mut self, pc: &InstructionPosition) {
        self.pc = pc.0 as usize;
    }

    pub const fn pc(&self) -> usize {
        self.pc
    }

    pub fn fp(&self) -> usize {
        self.memory.frame_offset
    }

    pub fn sp(&self) -> usize {
        self.memory.stack_offset
    }

    pub fn call_stack(&self) -> &[CallFrame] {
        &self.call_stack
    }

    fn execute_unimplemented(&mut self) {
        let unknown_opcode = OpCode::from(self.instructions[self.pc - 1].opcode);
        eprintln!("error: opcode not implemented: {unknown_opcode} {unknown_opcode:?}");
        eprintln!("VM runtime halted.");
        self.debug_output();
        panic!("unknown OPCODE! {unknown_opcode} {unknown_opcode:?}");
    }

    pub fn frame_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.memory.frame_ptr(), self.memory.memory_size) }
    }

    pub fn heap_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.memory.get_heap_ptr(0), self.memory.memory_size) }
    }

    pub fn constant_memory(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(
                self.memory.get_heap_ptr(0),
                self.memory.constant_memory_size,
            )
        }
    }

    pub fn all_memory_up_to(&self, offset: usize) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.memory.get_heap_ptr(0), offset) }
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

    pub fn reset_heap_allocator(&mut self) {
        self.memory.reset_allocator();
        //self.pc = 0;
    }

    pub fn reset_minimal_stack_and_fp(&mut self) {
        self.memory.reset_stack_and_fp();
        self.reset_call_stack();
        self.execution_complete = false;
    }

    pub fn reset_call_stack(&mut self) {
        //self.memory.reset();
        self.call_stack.clear();
    }

    pub fn reset_debug(&mut self) {
        self.debug = Debug::default();
        self.memory.debug = MemoryDebug {
            max_heap_alloc_offset: 0,
        }
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
    fn execute_mov_32(&mut self, dst_reg: u8, a: u8, b: u8, c: u8, d: u8) {
        set_reg!(self, dst_reg, Self::u8s_to_32(a, b, c, d));
    }

    #[inline]
    fn execute_mov_16(&mut self, dst_reg: u8, a: u8, b: u8) {
        set_reg!(self, dst_reg, u16_from_u8s!(a, b));
    }

    #[inline]
    fn execute_mov_8(&mut self, dst_reg: u8, octet: u8) {
        set_reg!(self, dst_reg, octet);
    }

    // Fixed Point special methods
    #[inline]
    fn execute_mul_f32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = Fp::from_raw(get_reg!(self, lhs_reg) as i32);
        let rhs = Fp::from_raw(get_reg!(self, rhs_reg) as i32);
        set_reg!(self, dst_reg, (lhs * rhs).inner());
    }

    #[inline]
    fn execute_div_f32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = Fp::from_raw(get_reg!(self, lhs_reg) as i32);
        let rhs = Fp::from_raw(get_reg!(self, rhs_reg) as i32);

        set_reg!(self, dst_reg, (lhs / rhs).inner());
    }

    #[inline]
    fn execute_f32_round(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from_raw(get_reg!(self, val_reg) as i32);

        let int_val: i16 = val.round().into();
        set_reg!(self, dst_reg, int_val);
    }

    #[inline]
    fn execute_f32_floor(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from_raw(get_reg!(self, val_reg) as i32);

        let floored: i16 = val.floor().into();
        set_reg!(self, dst_reg, floored);
    }

    #[inline]
    fn execute_f32_sqrt(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from_raw(get_reg!(self, val_reg) as i32);

        set_reg!(self, dst_reg, val.sqrt().inner());
    }

    #[inline]
    fn execute_f32_sin(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from_raw(get_reg!(self, val_reg) as i32);

        set_reg!(self, dst_reg, val.sin().inner());
    }

    #[inline]
    fn execute_f32_asin(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from_raw(get_reg!(self, val_reg) as i32);

        set_reg!(self, dst_reg, val.asin().inner());
    }

    #[inline]
    fn execute_f32_cos(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from_raw(get_reg!(self, val_reg) as i32);

        set_reg!(self, dst_reg, val.cos().inner());
    }

    #[inline]
    fn execute_f32_acos(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from_raw(get_reg!(self, val_reg) as i32);

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
        let val = Fp::from_raw(get_reg!(self, val_reg) as i32);

        self.create_string(dst_reg, &val.to_string())
    }

    #[inline]
    fn execute_f32_sign(&mut self, dst_reg: u8, val_reg: u8) {
        let val = Fp::from_raw(get_reg!(self, val_reg) as i32);
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
        let val = get_reg!(self, val_reg) as i32;
        set_reg!(self, dst_reg, -val);
    }

    #[inline]
    const fn execute_add_u32(&mut self, dst_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg);
        let rhs = get_reg!(self, rhs_reg);

        set_reg!(self, dst_reg, lhs.wrapping_add(rhs));
    }

    #[inline]
    fn execute_add_u32_imm(
        &mut self,
        dst_reg: u8,
        lhs_reg: u8,
        rhs_1: u8,
        rhs_2: u8,
        rhs_3: u8,
        rhs_4: u8,
    ) {
        let lhs = get_reg!(self, lhs_reg);
        let rhs = u32_from_u8s!(rhs_1, rhs_2, rhs_3, rhs_4);

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

    /// This is the mathematical modulo, *not* the remainder.
    /// Like how it is done in Lua and Python
    /// <https://en.wikipedia.org/wiki/Modulo#In_programming_languages>
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
                    "VM Runtime Error: Signed 32-bit integer overflow during DIV_I32 (R{dst_reg} = R{lhs_reg} - R{rhs_reg})"
                );
            }
        }
    }

    #[inline]
    fn execute_lt_i32(&mut self, dest_bool_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;
        set_reg!(self, dest_bool_reg, lhs < rhs);
    }

    #[inline]
    fn execute_le_i32(&mut self, dest_bool_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;
        set_reg!(self, dest_bool_reg, lhs <= rhs);
    }

    #[inline]
    fn execute_gt_i32(&mut self, dest_bool_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;
        set_reg!(self, dest_bool_reg, lhs > rhs);
    }

    #[inline]
    fn execute_ge_i32(&mut self, dest_bool_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg) as i32;
        let rhs = get_reg!(self, rhs_reg) as i32;

        set_reg!(self, dest_bool_reg, lhs >= rhs);
    }

    #[inline]
    fn execute_ge_u32(&mut self, dest_bool_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg);
        let rhs = get_reg!(self, rhs_reg);

        set_reg!(self, dest_bool_reg, lhs >= rhs);
    }

    #[inline]
    fn execute_lt_u32(&mut self, dest_bool_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg);
        let rhs = get_reg!(self, rhs_reg);

        set_reg!(self, dest_bool_reg, lhs < rhs);
    }

    #[inline]
    fn execute_le_u32(&mut self, dest_bool_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg);
        let rhs = get_reg!(self, rhs_reg);

        set_reg!(self, dest_bool_reg, lhs <= rhs);
    }

    #[inline]
    fn execute_gt_u32(&mut self, dest_bool_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        let lhs = get_reg!(self, lhs_reg);
        let rhs = get_reg!(self, rhs_reg);

        set_reg!(self, dest_bool_reg, lhs > rhs);
    }

    #[inline]
    fn execute_pseudo_random_i32(&mut self, dst_reg: u8, src_reg: u8) {
        let src = get_reg!(self, src_reg);
        set_reg!(self, dst_reg, squirrel_prng::squirrel_noise5(src, 0) as i32);
    }

    #[inline]
    fn execute_i32_to_string(&mut self, dst_reg: u8, val_reg: u8) {
        let val = get_reg!(self, val_reg) as i32;

        self.create_string(dst_reg, &val.to_string());

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            let read_back_string = self.read_string(get_reg!(self, dst_reg), self.memory());
            eprintln!("i32_to_string: {val}, {dst_reg} '{read_back_string}'");
        }
    }

    #[inline]
    fn execute_bool_to_string(&mut self, dst_reg: u8, val_reg: u8) {
        let val = get_reg!(self, val_reg) != 0;

        self.create_string(dst_reg, &val.to_string());
    }

    const HEX_DIGITS: &'static [u8; 16] = b"0123456789ABCDEF";

    #[inline]
    const fn byte_to_prefixed_hex(val: u8, dst: &mut [u8; 4]) {
        dst[0] = b'0';
        dst[1] = b'x';
        dst[2] = Self::HEX_DIGITS[(val >> 4) as usize];
        dst[3] = Self::HEX_DIGITS[(val & 0x0F) as usize];
    }

    #[inline]
    fn execute_byte_to_string(&mut self, dst_reg: u8, val_reg: u8) {
        let val = get_reg!(self, val_reg);
        debug_assert!(val <= 0xff, "byte out of range");

        let mut buf = [0u8; 4];
        Self::byte_to_prefixed_hex(val as u8, &mut buf);

        // Safety: we know buf is valid ASCII
        let s = unsafe { std::str::from_utf8_unchecked(&buf) };

        self.create_string(dst_reg, s);
    }

    #[inline]
    fn execute_codepoint_to_string(&mut self, dst_reg: u8, val_reg: u8) {
        let char_raw = get_reg!(self, val_reg);
        let char = std::char::from_u32(char_raw).unwrap();
        self.create_string(dst_reg, &char.to_string());
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

    // Sort of the same as `sub`
    #[inline]
    fn execute_cmp_reg(&mut self, dest_bool_reg: u8, lhs_reg: u8, rhs_reg: u8) {
        set_reg!(
            self,
            dest_bool_reg,
            self.registers[lhs_reg as usize] == self.registers[rhs_reg as usize]
        );
    }

    // Sort of the same as `sub`
    #[inline]
    fn execute_eq_8_imm(&mut self, dest_bool_reg: u8, val_reg: u8, octet: u8) {
        let compare = get_reg!(self, val_reg);
        set_reg!(self, dest_bool_reg, compare == octet as u32);
        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "{compare} {octet} result: {}",
                get_reg!(self, dest_bool_reg)
            )
        }
    }

    #[inline]
    fn execute_check_u8(&mut self, check_u8_reg: u8) {
        let compare = get_reg!(self, check_u8_reg);
        if compare > 0xff {
            self.internal_trap(TrapCode::U8CheckFailed);
        }
    }

    #[inline]
    fn execute_trap_on_less_than(&mut self, a_reg: u8, b_reg: u8) {
        let a = get_reg!(self, a_reg);
        let b = get_reg!(self, b_reg);
        if a < b {
            self.internal_trap(TrapCode::LessThanTrap { a, b })
        }
    }

    #[inline]
    fn execute_move_equal_to_zero(&mut self, dst_reg: u8, src_reg: u8) {
        set_reg!(self, dst_reg, get_reg!(self, src_reg) == 0);
    }

    #[inline]
    const fn execute_branch_if_false(
        &mut self,
        test_reg: u8,
        branch_offset_0: u8,
        branch_offset_1: u8,
    ) {
        if get_reg!(self, test_reg) == 0 {
            self.pc =
                (self.pc as i32 + i16_from_u8s!(branch_offset_0, branch_offset_1) as i32) as usize;
        }
    }

    #[inline]
    const fn execute_branch_if_true(
        &mut self,
        test_reg: u8,
        branch_offset_0: u8,
        branch_offset_1: u8,
    ) {
        if get_reg!(self, test_reg) != 0 {
            self.pc =
                (self.pc as i32 + i16_from_u8s!(branch_offset_0, branch_offset_1) as i32) as usize;
        }
    }

    #[inline]
    fn execute_b(&mut self, branch_offset_0: u8, branch_offset_1: u8) {
        self.pc =
            (self.pc as i32 + i16_from_u8s!(branch_offset_0, branch_offset_1) as i32) as usize;
    }

    #[inline]
    fn execute_hlt(&mut self) {
        self.execution_complete = true;
        #[cfg(feature = "debug_vm")]
        if self.debug_opcodes_enabled {
            self.debug_output();
        }
    }

    #[inline]
    fn execute_user_halt(&mut self) {
        self.execution_complete = true;
        self.state = VmState::Halt;
        #[cfg(feature = "debug_vm")]
        if self.debug_opcodes_enabled {
            self.debug_output();
        }
    }

    #[inline]
    fn execute_step(&mut self) {
        self.execution_complete = true;
        self.state = VmState::Step;
        #[cfg(feature = "debug_vm")]
        if self.debug_opcodes_enabled {
            self.debug_output();
        }
    }

    #[inline]
    fn execute_trap(&mut self, trap_code: u8) {
        self.internal_trap(TrapCode::try_from(trap_code).unwrap());
    }

    pub fn internal_trap(&mut self, trap_code: TrapCode) {
        self.execution_complete = true;

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!("vm trap: '{trap_code}'");
        }
        self.state = VmState::Trap(trap_code);

        #[cfg(feature = "debug_vm")]
        if self.debug_opcodes_enabled {
            self.debug_output();
        }
    }

    #[inline]
    fn execute_panic(&mut self, panic_reason_reg: u8) {
        self.execution_complete = true;

        #[cfg(feature = "debug_vm")]
        if self.debug_opcodes_enabled {
            self.debug_output();
        }

        let heap_addr = get_reg!(self, panic_reason_reg);
        let str = self.read_string(heap_addr, &self.memory);

        #[cfg(feature = "debug_vm")]
        if self.debug_stats_enabled {
            eprintln!("panic: {str}");
        }

        self.state = VmState::Panic(str.to_string());
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
    fn execute_st_regs_to_frame(
        &mut self,
        frame_offset_0: u8,
        frame_offset_1: u8,
        frame_offset_2: u8,
        frame_offset_3: u8,
        start_reg: u8,
        count: u8,
    ) {
        let frame_offset = u32_from_u8s!(
            frame_offset_0,
            frame_offset_1,
            frame_offset_2,
            frame_offset_3
        );
        let const_reg_ptr = &self.registers[start_reg as usize] as *const u32;
        let target_ptr = self.memory.get_frame_ptr_as_u32(frame_offset);
        unsafe {
            ptr::copy_nonoverlapping(const_reg_ptr, target_ptr, count as usize);
        }
    }

    #[inline]
    fn execute_st_regs_to_frame_using_mask(
        &mut self,
        frame_offset_0: u8,
        frame_offset_1: u8,
        frame_offset_2: u8,
        frame_offset_3: u8,
        reg_mask: u8,
    ) {
        let frame_offset = u32_from_u8s!(
            frame_offset_0,
            frame_offset_1,
            frame_offset_2,
            frame_offset_3
        );

        let mut target_ptr = self.memory.get_frame_ptr_as_u32(frame_offset);
        let mut const_reg_ptr = &self.registers[0usize] as *const u32;
        let mut mask = reg_mask;
        for _ in 0..8 {
            if (mask & 0x1) != 0 {
                unsafe {
                    ptr::write(target_ptr, *const_reg_ptr);
                    target_ptr = target_ptr.add(1);
                }
            }
            mask >>= 1;
            unsafe {
                const_reg_ptr = const_reg_ptr.add(1);
            }
        }
    }

    #[inline]
    fn execute_stw_using_base_ptr_and_offset(
        &mut self,
        base_ptr_reg: u8,
        offset_0: u8,
        offset_1: u8,
        offset_2: u8,
        offset_3: u8,
        src_reg: u8,
    ) {
        let offset = u32_from_u8s!(offset_0, offset_1, offset_2, offset_3);
        //let const_reg_ptr = &self.registers[start_reg as usize] as *const u32;
        let ptr_to_write_to = self.get_ptr_from_reg_with_offset(base_ptr_reg, offset) as *mut u32;
        let value_to_copy = get_reg!(self, src_reg);

        unsafe {
            ptr::write(ptr_to_write_to, value_to_copy);
        }
    }

    #[inline]
    fn execute_sth_using_base_ptr_and_offset(
        &mut self,
        base_ptr_reg: u8,
        offset_0: u8,
        offset_1: u8,
        offset_2: u8,
        offset_3: u8,
        src_reg: u8,
    ) {
        let offset = u32_from_u8s!(offset_0, offset_1, offset_2, offset_3);
        //let const_reg_ptr = &self.registers[start_reg as usize] as *const u32;
        let ptr_to_write_to = self.get_ptr_from_reg_with_offset(base_ptr_reg, offset) as *mut u16;
        let value_to_copy = get_reg!(self, src_reg) as u16;

        unsafe {
            ptr::write(ptr_to_write_to, value_to_copy);
        }
    }
    #[inline]
    fn execute_stb_using_base_ptr_and_offset(
        &mut self,
        base_ptr_reg: u8,
        offset_0: u8,
        offset_1: u8,
        offset_2: u8,
        offset_3: u8,
        src_reg: u8,
    ) {
        let offset = u32_from_u8s!(offset_0, offset_1, offset_2, offset_3);
        //let const_reg_ptr = &self.registers[start_reg as usize] as *const u32;
        let ptr_to_write_to = self.get_ptr_from_reg_with_offset(base_ptr_reg, offset);
        let value_to_copy = get_reg!(self, src_reg) as u8;

        unsafe {
            ptr::write(ptr_to_write_to, value_to_copy);
        }
    }

    #[inline]
    pub fn execute_ldb_from_base_ptr_and_offset(
        &mut self,
        dst_reg: u8,
        base_ptr_reg: u8,
        offset_0: u8,
        offset_1: u8,
        offset_2: u8,
        offset_3: u8,
    ) {
        let offset = u32_from_u8s!(offset_0, offset_1, offset_2, offset_3);
        let ptr_to_read_from = self.get_const_ptr_from_reg_with_offset(base_ptr_reg, offset);
        unsafe {
            set_reg!(self, dst_reg, *ptr_to_read_from);
        }
    }

    #[inline]
    pub fn execute_ldw_from_base_ptr_and_offset(
        &mut self,
        dst_reg: u8,
        base_ptr_reg: u8,
        offset_0: u8,
        offset_1: u8,
        offset_2: u8,
        offset_3: u8,
    ) {
        let offset = u32_from_u8s!(offset_0, offset_1, offset_2, offset_3);
        let ptr_to_read_from =
            self.get_const_ptr_from_reg_with_offset(base_ptr_reg, offset) as *const u32;

        // u32 must be 4-byte aligned
        let raw_ptr = self.get_const_ptr_from_reg_with_offset(base_ptr_reg, offset) as usize;

        let base_addr = get_reg!(self, base_ptr_reg);
        const ALIGN: usize = std::mem::align_of::<u32>(); // == 4
        if (base_addr as usize) & (ALIGN - 1) != 0 {
            eprintln!("base_addr {base_addr} offset {offset}");
            self.internal_trap(TrapCode::Misaligned);
            return;
        }

        if raw_ptr & (ALIGN - 1) != 0 {
            self.internal_trap(TrapCode::Misaligned);
            return;
        }

        unsafe {
            set_reg!(self, dst_reg, *ptr_to_read_from);
        }
    }

    #[inline]
    fn execute_ldw_from_absolute_address(
        &mut self,
        dst_reg: u8,
        addr_0: u8,
        addr_1: u8,
        addr_2: u8,
        addr_3: u8,
    ) {
        let absolute_addr = u32_from_u8s!(addr_0, addr_1, addr_2, addr_3);

        let ptr_to_read_from = self.memory.get_heap_const_ptr(absolute_addr as usize) as *const u32;

        unsafe {
            set_reg!(self, dst_reg, *ptr_to_read_from);
        }
    }

    #[inline]
    fn execute_ldb_from_absolute_address(
        &mut self,
        dst_reg: u8,
        addr_0: u8,
        addr_1: u8,
        addr_2: u8,
        addr_3: u8,
    ) {
        let absolute_addr = u32_from_u8s!(addr_0, addr_1, addr_2, addr_3);

        let ptr_to_read_from = self.memory.get_heap_const_ptr(absolute_addr as usize);

        unsafe {
            set_reg!(self, dst_reg, *ptr_to_read_from);
        }
    }

    #[inline]
    pub fn execute_ldh_from_base_ptr_and_offset(
        &mut self,
        dst_reg: u8,
        base_ptr_reg: u8,
        offset_0: u8,
        offset_1: u8,
        offset_2: u8,
        offset_3: u8,
    ) {
        let offset = u32_from_u8s!(offset_0, offset_1, offset_2, offset_3);
        let ptr_to_read_from =
            self.get_const_ptr_from_reg_with_offset(base_ptr_reg, offset) as *const u16;
        unsafe {
            set_reg!(self, dst_reg, *ptr_to_read_from);
        }
    }

    #[inline]
    pub fn execute_ld_regs_from_frame(
        &mut self,
        start_reg: u8,
        offset_0: u8,
        offset_1: u8,
        offset_2: u8,
        offset_3: u8,
        count: u8,
    ) {
        let offset = u32_from_u8s!(offset_0, offset_1, offset_2, offset_3);
        let target_reg_ptr = &mut self.registers[start_reg as usize] as *mut u32;
        let source_frame_start = self.memory.get_frame_const_ptr_as_u32(offset);
        unsafe {
            ptr::copy_nonoverlapping(source_frame_start, target_reg_ptr, count as usize);
        }
    }

    #[inline]
    pub fn execute_ld_regs_from_frame_using_mask(
        &mut self,
        reg_mask: u8,
        offset_0: u8,
        offset_1: u8,
        offset_2: u8,
        offset_3: u8,
    ) {
        let offset = u32_from_u8s!(offset_0, offset_1, offset_2, offset_3);
        let mut target_reg_ptr = &mut self.registers[0usize] as *mut u32;
        let mut source_frame_start = self.memory.get_frame_const_ptr_as_u32(offset);
        let mut mask = reg_mask;
        for _ in 0..8 {
            if mask & 0x01 != 0 {
                unsafe {
                    ptr::write(target_reg_ptr, *source_frame_start);
                    source_frame_start = source_frame_start.add(1);
                }
            }
            mask >>= 1;
            unsafe {
                target_reg_ptr = target_reg_ptr.add(1);
            }
        }
    }

    #[inline]
    fn execute_lea(&mut self, dst_reg: u8, offset_0: u8, offset_1: u8, offset_2: u8, offset_3: u8) {
        let current_fp_addr = self.memory.frame_offset as u32;
        let offset = u32_from_u8s!(offset_0, offset_1, offset_2, offset_3);
        set_reg!(self, dst_reg, current_fp_addr + offset);
    }

    #[inline]
    pub fn execute_frame_memory_clear(
        &mut self,
        dst_pointer_0: u8,
        dst_pointer_1: u8,
        dst_pointer_2: u8,
        dst_pointer_3: u8,
        memory_size_0: u8,
        memory_size_1: u8,
        memory_size_2: u8,
        memory_size_3: u8,
    ) {
        let frame_offset =
            u32_from_u8s!(dst_pointer_0, dst_pointer_1, dst_pointer_2, dst_pointer_3);
        let total_bytes = u32_from_u8s!(memory_size_0, memory_size_1, memory_size_2, memory_size_3);

        assert!(
            frame_offset + total_bytes < self.memory.memory_size as u32,
            "trying to overwrite memory!"
        );
        let dst_ptr = self.memory.get_frame_ptr(frame_offset);

        unsafe {
            ptr::write_bytes(dst_ptr, 0, total_bytes as usize);
        }
    }

    #[inline]
    fn execute_mov_mem_with_immediate_size(
        &mut self,
        dst_pointer_reg: u8,
        src_pointer_reg: u8,
        memory_size_0: u8,
        memory_size_1: u8,
        memory_size_2: u8,
        memory_size_3: u8,
    ) {
        let dest_addr = get_reg!(self, dst_pointer_reg);
        let src_addr = get_reg!(self, src_pointer_reg);
        let memory_size = u32_from_u8s!(memory_size_0, memory_size_1, memory_size_2, memory_size_3);
        assert!(
            src_addr + memory_size < self.memory.memory_size as u32,
            "trying to overwrite memory"
        );

        // Check for overlapping memory regions
        let dest_end = dest_addr + memory_size;
        let src_end = src_addr + memory_size;

        if dest_addr < src_end && src_addr < dest_end {
            return self.internal_trap(TrapCode::OverlappingMemoryCopy);
        }

        #[cfg(feature = "debug_vm")]
        if self.debug_operations_enabled {
            eprintln!(
                "{:04X}> BLKCPY Size={:08X} \n  \
                DST_ADDR=0x{:08X}\n  \
                SRC_ADDR=0x{:08X}",
                self.pc - 1,
                memory_size,
                dest_addr,
                src_addr,
            );
        }

        let dst_ptr = self.memory.get_heap_ptr(dest_addr as usize);
        let src_ptr = self.memory.get_heap_const_ptr(src_addr as usize);

        unsafe {
            ptr::copy_nonoverlapping(src_ptr, dst_ptr, memory_size as usize);
        }
    }

    #[inline]
    fn execute_cmp_block(
        &mut self,
        dest_bool_reg: u8,
        src_addr_reg_a: u8,
        src_addr_reg_b: u8,
        size_lower: u8,
        size_upper: u8,
    ) {
        let size = u16_from_u8s!(size_lower, size_upper) as usize;

        let arc_addr_a = get_reg!(self, src_addr_reg_a);
        let src_addr_b = get_reg!(self, src_addr_reg_b);

        let src_ptr_a = self.memory.get_heap_const_ptr(arc_addr_a as usize);
        let src_ptr_b = self.memory.get_heap_const_ptr(src_addr_b as usize);

        unsafe {
            let slice_a = std::slice::from_raw_parts(src_ptr_a, size);
            let slice_b = std::slice::from_raw_parts(src_ptr_b, size);

            set_reg!(self, dest_bool_reg, slice_a == slice_b);
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
                HandlerType::Args6(_) => format!(
                    "{:04X}, {:04X}, {:04X}, {:04X}, {:04X}, {:04X}",
                    operands[0], operands[1], operands[2], operands[3], operands[4], operands[5],
                ),
                HandlerType::Args7(_) => format!(
                    "{:04X}, {:04X}, {:04X}, {:04X}, {:04X}, {:04X}, {:04X}",
                    operands[0],
                    operands[1],
                    operands[2],
                    operands[3],
                    operands[4],
                    operands[5],
                    operands[6],
                ),
                HandlerType::Args8(_) => format!(
                    "{:04X}, {:04X}, {:04X}, {:04X}, {:04X}, {:04X}, {:04X}, {:04X}",
                    operands[0],
                    operands[1],
                    operands[2],
                    operands[3],
                    operands[4],
                    operands[5],
                    operands[6],
                    operands[7],
                ),
            }
        );
    }

    fn execute_call(
        &mut self,
        absolute_pc_a: u8,
        absolute_pc_b: u8,
        absolute_pc_c: u8,
        absolute_pc_d: u8,
    ) {
        let absolute_pc = u32_from_u8s!(absolute_pc_a, absolute_pc_b, absolute_pc_c, absolute_pc_d);
        let return_info = CallFrame {
            return_address: self.pc + 1,
            previous_frame_offset: self.memory.frame_offset,
            previous_stack_offset: self.memory.stack_offset,
        };

        //self.memory.set_fp(); // we do not modify fp
        self.call_stack.push(return_info);
        self.pc = absolute_pc as usize;

        #[cfg(feature = "debug_vm")]
        if self.debug_stats_enabled {
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
        register_count: u8,
        callback: &mut dyn HostFunctionCallback,
    ) {
        let heap = self.memory();

        let function_id = u8s_to_u16!(function_id_lower, function_id_upper);

        unsafe {
            let host_args = HostArgs::new(
                function_id,
                heap.memory,
                heap.memory_size,
                heap.stack_offset,
                self.registers.as_mut_ptr(),
                register_count as usize + 1,
            );

            callback.dispatch_host_call(host_args);
        }
    }

    #[allow(clippy::missing_const_for_fn)]
    #[inline(always)]
    fn execute_enter(
        &mut self,
        frame_size_0: u8,
        frame_size_1: u8,
        frame_size_2: u8,
        frame_size_3: u8,
    ) {
        let frame_size = u32_from_u8s!(frame_size_0, frame_size_1, frame_size_2, frame_size_3);
        self.memory.set_fp_from_sp(); // set the frame pointer to what sp is now
        self.memory.inc_sp(frame_size as usize);
        #[cfg(feature = "debug_vm")]
        if self.debug_stats_enabled && self.memory.stack_offset > self.debug.max_stack_offset {
            self.debug.max_stack_offset = self.memory.stack_offset - self.memory.stack_start;
        }
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
        if self.debug_stats_enabled {
            self.debug.call_depth -= 1;
        }
    }

    #[inline]
    const fn u8s_to_32(a: u8, b: u8, c: u8, d: u8) -> u32 {
        u32::from_le_bytes([a, b, c, d])
    }

    #[inline]
    pub fn get_const_ptr_from_reg(&self, reg: u8) -> *const u8 {
        let ptr_addr = get_reg!(self, reg);
        self.memory.get_heap_const_ptr(ptr_addr as usize)
    }

    #[inline]
    pub fn get_const_ptr_from_reg_with_offset(&self, reg: u8, offset: u32) -> *const u8 {
        let ptr_addr = get_reg!(self, reg) + offset;
        self.memory.get_heap_const_ptr(ptr_addr as usize)
    }

    #[inline]
    pub fn get_ptr_from_reg(&self, reg: u8) -> *mut u8 {
        let ptr_addr = get_reg!(self, reg);
        self.memory.get_heap_ptr(ptr_addr as usize)
    }

    #[inline]
    pub fn get_ptr_and_addr_from_reg(&self, reg: u8) -> (*mut u8, u32) {
        let ptr_addr = get_reg!(self, reg);
        (self.memory.get_heap_ptr(ptr_addr as usize), ptr_addr)
    }

    #[inline]
    pub fn get_ptr_from_reg_with_offset(&self, reg: u8, offset: u32) -> *mut u8 {
        let ptr_addr = get_reg!(self, reg) + offset;
        self.memory.get_heap_ptr(ptr_addr as usize)
    }
}
