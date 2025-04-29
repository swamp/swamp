/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use source_map_cache::{SourceMapLookup, SourceMapWrapper};
use source_map_node::{FileId, Span};
use std::cmp::PartialEq;
use std::fmt::Write;
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::types::{
    BasicType, DecoratedOpcode, DecoratedOperand, DecoratedOperandAccessKind,
    DecoratedOperandOrigin, FrameMemoryAttribute, FrameMemoryInfo, PathInfo, b8_type, bytes_type,
    float_type, indirect_heap_ptr_type, int_type, map_iter_type, map_type, range_iter_type,
    range_type, slice_type, string_type, u8_type, u32_type, vec_iter_type, vec_type,
};
use swamp_vm_types::{
    BinaryInstruction, FrameMemoryAddress, FrameMemorySize, HeapMemoryAddress, HeapMemoryOffset,
    InstructionPosition, InstructionPositionOffset, MemorySize, Meta,
};
use tracing::info;
use yansi::{Color, Paint};

#[derive(Eq, PartialEq, Clone)]
pub struct SourceFileLineInfo {
    pub row: usize,
    pub file_id: usize,
}

fn convert_tabs_to_spaces(input: &str) -> String {
    input.replace('\t', " ")
}

#[must_use]
pub fn disasm_instructions_color(
    binary_instructions: &[BinaryInstruction],
    instruction_position_base: &InstructionPositionOffset,
    meta: &[Meta],
    memory_infos: &FrameMemoryInfo,
    ip_infos: &SeqMap<InstructionPosition, SourceFileLineInfo>,
    source_file_wrapper: &SourceMapWrapper,
) -> String {
    let mut string = String::new();
    let mut last_frame_size: u16 = 0;

    let mut last_line_info = SourceFileLineInfo {
        row: usize::MAX,
        file_id: usize::MAX,
    };
    let mut expected_next_row_to_show = 1;

    for (ip_offset, instruction) in binary_instructions.iter().enumerate() {
        let ip_index = instruction_position_base.0 + ip_offset as u16;
        if OpCode::Enter as u8 == instruction.opcode {
            last_frame_size = instruction.operands[0];
        }
        if let Some(found) = ip_infos.get(&InstructionPosition(ip_index)) {
            if last_line_info.file_id != found.file_id {
                last_line_info = found.clone();
                expected_next_row_to_show = found.row;
            }

            if found.row < expected_next_row_to_show {
                expected_next_row_to_show = found.row;
            }
            for row_to_display in expected_next_row_to_show..=found.row {
                let file_id = found.file_id as FileId;
                let line = source_file_wrapper
                    .get_source_line(file_id, row_to_display)
                    .expect(&format!("wrong row: {row_to_display}"));
                writeln!(
                    string,
                    "{:4} {} {}",
                    row_to_display,
                    "|".green(),
                    convert_tabs_to_spaces(line),
                )
                .expect("TODO: panic message");
            }

            expected_next_row_to_show = found.row + 1;
        }

        writeln!(
            string,
            "     {:04X}> {}",
            ip_index,
            disasm_color(
                instruction,
                FrameMemorySize(last_frame_size),
                memory_infos,
                &meta[ip_offset]
            )
        )
        .expect("TODO: panic message");
    }

    string
}
fn memory_kind_color(kind: Option<PathInfo>) -> String {
    kind.map_or_else(
        || "temp addr".to_string(),
        |path_info| path_info.convert_to_string(),
    )
}

#[must_use]
pub fn disasm_instructions_no_color(
    binary_instruction: &[BinaryInstruction],
    descriptions: &[String],
    ip_infos: &SeqMap<InstructionPosition, String>,
    frame_memory_info: &FrameMemoryInfo,
    include_comments: bool,
) -> String {
    let mut string = String::new();
    let mut last_frame_size: u16 = 0;

    for (ip_index, (instruction, comment)) in
        binary_instruction.iter().zip(descriptions).enumerate()
    {
        if OpCode::Enter as u8 == instruction.opcode {
            last_frame_size = instruction.operands[0];
        }
        if let Some(found) = ip_infos.get(&InstructionPosition(ip_index as u16)) {
            string += &format!("- {found} -\n");
        }

        let comment_to_use = if include_comments {
            comment.clone()
        } else {
            String::new()
        };

        string += &format!(
            "> {:04X}: {}\n",
            ip_index,
            disasm_no_color(
                instruction,
                FrameMemorySize(last_frame_size),
                frame_memory_info,
                &comment_to_use
            )
        );
    }

    string
}

#[allow(clippy::too_many_lines)]
#[must_use]
pub fn disasm_color(
    binary_instruction: &BinaryInstruction,
    frame_size: FrameMemorySize,
    memory_infos: &FrameMemoryInfo,
    meta: &Meta,
) -> String {
    let decorated = disasm(binary_instruction, frame_size, memory_infos);

    let name = format!("{:7}", decorated.name.blue());

    let mut converted_operands = Vec::new();
    let mut converted_comments = Vec::new();

    for operand in decorated.operands {
        let operand_addr = operand.kind.path_info();

        let (new_str, comment_str) = match &operand.kind {
            DecoratedOperandAccessKind::ReadFrameAddress(addr, memory_kind, attr) => {
                let color = if attr.is_temporary {
                    Color::BrightGreen
                } else {
                    Color::Green
                };

                (
                    format!("{}{}", "$".fg(color), format!("{:04X}", addr.0).fg(color)),
                    memory_kind_color(memory_kind.clone()),
                )
            }
            DecoratedOperandAccessKind::WriteFrameAddress(addr, memory_kind, attr) => {
                let color = if attr.is_temporary {
                    Color::BrightMagenta
                } else {
                    Color::Red
                };

                (
                    format!("{}{}", "$".fg(color), format!("{:04X}", addr.0).fg(color)),
                    memory_kind_color(memory_kind.clone()),
                )
            }
            DecoratedOperandAccessKind::HeapAddress(addr) => {
                let color = Color::Green;

                (
                    format!("{}{}", "%$".fg(color), format!("{:08X}", addr.0).fg(color)),
                    String::new(),
                )
            }
            DecoratedOperandAccessKind::ReadIndirectPointer(addr) => {
                let color = Color::Green;

                (
                    format!("({}{})", "$".fg(color), format!("{:04X}", addr.0).fg(color)),
                    String::new(),
                )
            }
            DecoratedOperandAccessKind::Ip(ip) => (
                format!("{}{}", "@".cyan(), format!("{:X}", ip.0).bright_cyan()),
                String::new(),
            ),
            DecoratedOperandAccessKind::MemorySize(data) => (
                format!("{}", format!("{:X}", data.0).yellow()),
                format!("{}{}", "int:", data.0),
            ),

            DecoratedOperandAccessKind::ImmediateU32(data) => (
                format!("{}", format!("{data:X}",).magenta()),
                format!("{}{}", "int:", *data as i32),
            ),
            DecoratedOperandAccessKind::ImmediateU16(data) => (
                format!("{}", format!("{data:X}",).magenta()),
                format!("{}{}", "int:", *data as i32),
            ),
            DecoratedOperandAccessKind::ImmediateU8(data) => (
                format!("{}", format!("{data:02X}",).magenta()),
                format!("{}{}", "int:", *data as i8),
            ),
            DecoratedOperandAccessKind::CountU16(data) => (
                format!("{}", format!("{data:04X}",).yellow()),
                format!("{}", "count"),
            ),
            DecoratedOperandAccessKind::WriteIndirectHeapWithOffset(
                frame_addr,
                memory_offset,
                memory_kind,
            ) => (
                format!(
                    "({})+{}",
                    format!("${:04X}", frame_addr.0).red(),
                    format!("{:X}", memory_offset.0).red()
                ),
                memory_kind_color(memory_kind.clone()),
            ),
            DecoratedOperandAccessKind::ReadIndirectHeapWithOffset(
                frame_addr,
                memory_offset,
                memory_kind,
            ) => (
                format!(
                    "({})+{}",
                    format!("${:04X}", frame_addr.0).green(),
                    format!("{:X}", memory_offset.0).green()
                ),
                memory_kind_color(memory_kind.clone()),
            ),
        };
        converted_operands.push(new_str);

        /*
        let memory_comment =
            operand_addr.map(PathInfo::convert_to_string);

        if let Some(comment) = memory_comment {
            memory_comments.push(comment);
        }

         */

        if !comment_str.is_empty() {
            converted_comments.push(comment_str);
        }
    }

    let comment_suffix = if converted_comments.is_empty() {
        String::new()
    } else {
        format!(" ({})", converted_comments.join(", "))
    };

    /*
    let memory_comment_suffix = if memory_comments.is_empty() {
        String::new()
    } else {
        format!(" {}", memory_comments.join(", "))
    };

     */

    let total_comment = format!("{} {}", meta.comment, comment_suffix);
    let print_comment = if total_comment.is_empty() {
        String::new()
    } else {
        format!(" {} {}", ";".bright_black(), total_comment.bright_black())
    };

    format!("{} {}{}", name, converted_operands.join(" "), print_comment)
}

#[must_use]
pub fn disasm_no_color(
    binary_instruction: &BinaryInstruction,
    frame_memory_size: FrameMemorySize,
    frame_memory_info: &FrameMemoryInfo,
    comment: &str,
) -> String {
    let decorated = disasm(binary_instruction, frame_memory_size, frame_memory_info);

    let name = decorated.name.to_string();

    let mut converted_operands = Vec::new();

    for operand in decorated.operands {
        let new_str = match operand.kind {
            DecoratedOperandAccessKind::ReadFrameAddress(addr, _memory_kind, _attr) => {
                format!("{}{}", "$", format!("{:04X}", addr.0))
            }
            DecoratedOperandAccessKind::WriteFrameAddress(addr, _memory_kind, _attr) => {
                format!("{}{}", "$", format!("{:04X}", addr.0))
            }
            DecoratedOperandAccessKind::HeapAddress(addr) => {
                format!("{}{}", "%$", format!("{:08X}", addr.0))
            }
            DecoratedOperandAccessKind::ReadIndirectPointer(addr) => {
                format!("({}{})", "$", format!("{:04X}", addr.0))
            }
            DecoratedOperandAccessKind::MemorySize(data) => format!("{}", format!("{:X}", data.0)),

            DecoratedOperandAccessKind::Ip(ip) => {
                format!("{}{}", "@", format!("{:X}", ip.0))
            }
            DecoratedOperandAccessKind::ImmediateU32(data) => format!("{}", format!("{data:08X}",)),
            DecoratedOperandAccessKind::ImmediateU16(data) => format!("{}", format!("{data:04X}",)),
            DecoratedOperandAccessKind::ImmediateU8(data) => format!("{}", format!("{data:02X}",)),
            DecoratedOperandAccessKind::CountU16(data) => format!("{}", format!("{data:04X}",)),
            DecoratedOperandAccessKind::WriteIndirectHeapWithOffset(a, b, _c) => {
                format!("{}", format!("{:08X}+{:08X}", a.0, b.0))
            }
            DecoratedOperandAccessKind::ReadIndirectHeapWithOffset(a, b, _c) => {
                format!("{}", format!("{:08X}+{:08X}", a.0, b.0))
            }
        };
        converted_operands.push(new_str);
    }

    let comment_suffix = if comment.is_empty() {
        String::new()
    } else {
        format!(" ; {comment}")
    };

    format!(
        "{} {}{}",
        name,
        converted_operands.join(" "),
        comment_suffix
    )
}

#[allow(clippy::too_many_lines)]
#[must_use]
pub fn disasm(
    binary_instruction: &BinaryInstruction,
    frame_memory_size: FrameMemorySize,
    frame_memory_info: &FrameMemoryInfo,
) -> DecoratedOpcode {
    let opcode: OpCode = binary_instruction.opcode.into();
    let operands = binary_instruction.operands;

    let operands_slice: &[DecoratedOperandAccessKind] = match opcode {
        OpCode::Hlt | OpCode::Ret | OpCode::Brk => &[],

        OpCode::Panic => &[to_read_frame(
            operands[0],
            &string_type(),
            frame_memory_info,
        )],

        OpCode::Ld32 => {
            let data = ((operands[2] as u32) << 16) | operands[1] as u32;

            &[
                to_write_frame(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ImmediateU32(data),
            ]
        }
        OpCode::Ld16 => {
            let data = operands[1] as u16;

            &[
                to_write_frame(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ImmediateU16(data),
            ]
        }

        OpCode::Ld8 => {
            let data = operands[1];

            &[
                to_write_frame(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ImmediateU8(data),
            ]
        }

        // Integer
        OpCode::AddI32 => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::SubI32 => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::MulI32 => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::DivI32 => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::ModI32 => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::NegI32 => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        // Fixed
        OpCode::AddF32 => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
            to_read_frame(operands[2], &float_type(), frame_memory_info),
        ],
        OpCode::MulF32 => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
            to_read_frame(operands[2], &float_type(), frame_memory_info),
        ],
        OpCode::DivF32 => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
            to_read_frame(operands[2], &float_type(), frame_memory_info),
        ],

        OpCode::ModF32 => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
            to_read_frame(operands[2], &float_type(), frame_memory_info),
        ],
        OpCode::SubF32 => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
            to_read_frame(operands[2], &float_type(), frame_memory_info),
        ],
        OpCode::NegF32 => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::LtF32 => &[
            to_read_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::LeF32 => &[
            to_read_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::GtF32 => &[
            to_read_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],
        OpCode::GeF32 => &[
            to_read_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::LtI32 => &[
            to_read_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::LeI32 => &[
            to_read_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::GtI32 => &[
            to_read_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],
        OpCode::GeI32 => &[
            to_read_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::FloatToString => &[
            to_write_frame(operands[0], &string_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatRound => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatFloor => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatSqrt => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatSign => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatAbs => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatSin => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatCos => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatAcos => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatAsin => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatAtan2 => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatMin => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
            to_read_frame(operands[2], &float_type(), frame_memory_info),
        ],

        OpCode::FloatMax => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
            to_read_frame(operands[2], &float_type(), frame_memory_info),
        ],

        OpCode::FloatClamp => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
            to_read_frame(operands[2], &float_type(), frame_memory_info),
            to_read_frame(operands[3], &float_type(), frame_memory_info),
        ],

        OpCode::FloatPseudoRandom => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::Eq8Imm => {
            let data = operands[1];

            &[
                to_read_frame(operands[0], &u8_type(), frame_memory_info),
                DecoratedOperandAccessKind::ImmediateU8(data),
            ]
        }

        OpCode::Tst8 => &[to_read_frame(operands[0], &u8_type(), frame_memory_info)],

        OpCode::Stz => &[to_write_frame(operands[0], &b8_type(), frame_memory_info)],

        OpCode::Stnz => &[to_write_frame(operands[0], &b8_type(), frame_memory_info)],

        OpCode::Cmp8 => &[
            to_read_frame(operands[0], &u8_type(), frame_memory_info),
            to_read_frame(operands[1], &u8_type(), frame_memory_info),
        ],

        OpCode::Cmp32 => &[
            to_read_frame(operands[0], &u32_type(), frame_memory_info),
            to_read_frame(operands[1], &u32_type(), frame_memory_info),
        ],

        OpCode::Cmp => &[
            to_read_frame(operands[0], &bytes_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
        ],

        OpCode::Bnz | OpCode::Bz | OpCode::Call => &[to_jmp_ip(operands[0])],
        OpCode::NotZ => &[],
        OpCode::HostCall => &[
            DecoratedOperandAccessKind::ImmediateU16(operands[0]),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[1])),
        ],
        OpCode::Enter => &[DecoratedOperandAccessKind::MemorySize(MemorySize(
            operands[0],
        ))],
        OpCode::Jmp => &[to_jmp_ip(operands[0])],
        OpCode::Mov => &[
            to_write_frame(operands[0], &bytes_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
        ],
        OpCode::Mov32 => &[
            to_write_frame(operands[0], &u32_type(), frame_memory_info),
            to_read_frame(operands[1], &u32_type(), frame_memory_info),
        ],
        OpCode::MovLp => &[
            to_write_frame(operands[0], &bytes_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
        ],

        OpCode::MovMem => {
            let heap_mem_addr = ((operands[2] as u32) << 16) | operands[1] as u32;
            &[
                to_write_frame(operands[0], &bytes_type(), frame_memory_info),
                DecoratedOperandAccessKind::HeapAddress(HeapMemoryAddress(heap_mem_addr)),
                DecoratedOperandAccessKind::MemorySize(MemorySize(operands[3])),
            ]
        }
        OpCode::Nop => &[],

        OpCode::VecPop => &[to_write_frame(operands[0], &vec_type(), frame_memory_info)],

        OpCode::VecFromSlice => &[
            to_write_frame(operands[0], &bytes_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
        ],

        OpCode::VecFetch => &[
            to_write_frame(operands[0], &vec_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::VecSwap => &[
            to_write_frame(operands[0], &vec_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
        ],

        OpCode::VecSet => &[
            to_write_frame(operands[0], &vec_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &bytes_type(), frame_memory_info),
        ],

        OpCode::VecIterInit => &[
            to_write_frame(operands[0], &vec_iter_type(), frame_memory_info),
            DecoratedOperandAccessKind::ReadIndirectPointer(FrameMemoryAddress(operands[1])),
        ],

        OpCode::VecIterNext => &[
            to_write_frame(operands[0], &vec_iter_type(), frame_memory_info),
            to_write_frame(operands[1], &bytes_type(), frame_memory_info),
            to_jmp_ip(operands[2]),
        ],

        OpCode::VecIterNextPair => &[
            to_write_frame(operands[0], &vec_iter_type(), frame_memory_info),
            to_write_frame(operands[1], &bytes_type(), frame_memory_info),
            to_write_frame(operands[2], &bytes_type(), frame_memory_info),
            to_jmp_ip(operands[3]),
        ],

        OpCode::VecClear => &[to_write_frame(operands[0], &vec_type(), frame_memory_info)],

        OpCode::VecPush => &[
            to_write_frame(operands[0], &vec_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info),
        ],

        OpCode::VecRemoveIndex => &[
            to_write_frame(operands[0], &vec_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::VecRemoveIndexGetValue => &[
            to_write_frame(operands[0], &vec_type(), frame_memory_info),
            to_write_frame(operands[1], &vec_type(), frame_memory_info),
            to_read_frame(operands[2], &bytes_type(), frame_memory_info),
        ],

        OpCode::VecCreate => &[
            to_write_frame(operands[0], &vec_type(), frame_memory_info),
            DecoratedOperandAccessKind::CountU16(operands[1]),
        ],

        OpCode::VecGet => &[
            to_write_frame(operands[0], &bytes_type(), frame_memory_info),
            to_read_frame(operands[1], &vec_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
        ],

        OpCode::VecGetRange => &[
            to_write_frame(operands[0], &bytes_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info),
            to_read_frame(operands[2], &range_type(), frame_memory_info),
        ],

        OpCode::MapNewFromPairs => &[
            to_write_frame(operands[0], &map_type(), frame_memory_info),
            to_read_frame(operands[1], &slice_type(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[3])),
            DecoratedOperandAccessKind::CountU16(operands[4]),
        ],

        OpCode::MapIterInit => &[
            to_write_frame(operands[0], &map_iter_type(), frame_memory_info),
            to_read_frame(operands[1], &map_type(), frame_memory_info),
        ],

        OpCode::MapIterNext => &[
            to_write_frame(operands[0], &map_iter_type(), frame_memory_info),
            to_write_frame(operands[1], &bytes_type(), frame_memory_info),
            to_jmp_ip(operands[2]),
        ],

        OpCode::MapIterNextPair => &[
            to_write_frame(operands[0], &map_iter_type(), frame_memory_info),
            to_write_frame(operands[1], &bytes_type(), frame_memory_info),
            to_write_frame(operands[2], &bytes_type(), frame_memory_info),
            to_jmp_ip(operands[3]),
        ],

        OpCode::MapRemove => &[
            to_write_frame(operands[0], &map_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info),
        ],

        OpCode::MapFetch => &[
            to_write_frame(operands[0], &bytes_type(), frame_memory_info),
            to_read_frame(operands[1], &map_type(), frame_memory_info),
            to_read_frame(operands[2], &bytes_type(), frame_memory_info),
        ],

        OpCode::MapSet => &[
            to_write_frame(operands[0], &map_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info),
            to_read_frame(operands[2], &bytes_type(), frame_memory_info),
        ],

        OpCode::MapHas => &[
            //  sets the Z flag
            to_read_frame(operands[0], &map_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info),
        ],

        OpCode::RangeIterInit => &[
            to_write_frame(operands[0], &range_iter_type(), frame_memory_info),
            DecoratedOperandAccessKind::ReadIndirectPointer(FrameMemoryAddress(operands[1])),
        ],

        OpCode::RangeIterNext => &[
            to_write_frame(operands[0], &range_iter_type(), frame_memory_info),
            to_write_frame(operands[1], &bytes_type(), frame_memory_info),
            to_jmp_ip(operands[2]),
        ],

        OpCode::StringAppend => &[
            to_write_frame(operands[0], &string_type(), frame_memory_info),
            to_read_frame(operands[1], &string_type(), frame_memory_info),
            to_read_frame(operands[2], &string_type(), frame_memory_info),
        ],

        OpCode::IntToRnd => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::IntAbs => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::IntMin => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
        ],

        OpCode::IntMax => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
        ],

        OpCode::IntClamp => &[
            to_write_frame(operands[0], &int_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
            to_read_frame(operands[2], &int_type(), frame_memory_info),
            to_read_frame(operands[3], &int_type(), frame_memory_info),
        ],

        OpCode::IntToString => &[
            to_write_frame(operands[0], &string_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::BoolToString => &[
            to_write_frame(operands[0], &string_type(), frame_memory_info),
            to_read_frame(operands[1], &b8_type(), frame_memory_info),
        ],

        OpCode::IntToFloat => &[
            to_write_frame(operands[0], &float_type(), frame_memory_info),
            to_read_frame(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::Alloc => &[
            to_write_frame(operands[0], &indirect_heap_ptr_type(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[1])),
        ],

        OpCode::Stx => {
            let heap_mem_offset = ((operands[2] as u32) << 16) | operands[1] as u32;
            &[
                DecoratedOperandAccessKind::WriteIndirectHeapWithOffset(
                    FrameMemoryAddress(operands[0]),
                    HeapMemoryOffset(heap_mem_offset),
                    None,
                ),
                DecoratedOperandAccessKind::ReadFrameAddress(
                    FrameMemoryAddress(operands[3]),
                    None,
                    FrameMemoryAttribute {
                        is_temporary: operands[3] >= frame_memory_info.variable_frame_size.0,
                    },
                ),
                DecoratedOperandAccessKind::MemorySize(MemorySize(operands[4])),
            ]
        }

        OpCode::Ldx => {
            let heap_mem_offset = ((operands[3] as u32) << 16) | operands[2] as u32;
            &[
                to_write_frame(operands[0], &indirect_heap_ptr_type(), frame_memory_info),
                DecoratedOperandAccessKind::ReadIndirectHeapWithOffset(
                    FrameMemoryAddress(operands[1]),
                    HeapMemoryOffset(heap_mem_offset),
                    None,
                ),
                DecoratedOperandAccessKind::MemorySize(MemorySize(operands[4])),
            ]
        }

        OpCode::UnwrapJmpNone => &[
            to_write_frame(operands[0], &bytes_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info), // todo: optional union type
            to_jmp_ip(operands[2]),
        ],

        OpCode::UnwrapJmpSome => &[
            to_write_frame(operands[0], &bytes_type(), frame_memory_info),
            to_read_frame(operands[1], &bytes_type(), frame_memory_info), // todo: optional union type
            to_jmp_ip(operands[2]),
        ],
    };

    let converted_operands = operands_slice
        .iter()
        .map(|kind| DecoratedOperand {
            kind: kind.clone(),
            origin: DecoratedOperandOrigin {},
        })
        .collect();

    DecoratedOpcode {
        name: opcode.to_string(),
        operands: converted_operands,
    }
}

fn to_write_frame(
    addr: u16,
    fallback_expected_type: &BasicType,
    frame_memory_info: &FrameMemoryInfo,
) -> DecoratedOperandAccessKind {
    let maybe_path = frame_memory_info.find_path_to_address_items(FrameMemoryAddress(addr));

    let is_temporary = maybe_path.is_none();
    DecoratedOperandAccessKind::WriteFrameAddress(
        to_frame(addr),
        maybe_path,
        FrameMemoryAttribute { is_temporary },
    )
}

fn to_read_frame(
    addr: u16,
    fallback_expected_type: &BasicType,
    frame_memory_info: &FrameMemoryInfo,
) -> DecoratedOperandAccessKind {
    let is_temporary = addr >= frame_memory_info.variable_frame_size.0;
    let maybe_path = frame_memory_info.find_path_to_address_items(FrameMemoryAddress(addr));
    DecoratedOperandAccessKind::ReadFrameAddress(
        to_frame(addr),
        maybe_path,
        FrameMemoryAttribute { is_temporary },
    )
}

fn to_jmp_ip(ip: u16) -> DecoratedOperandAccessKind {
    DecoratedOperandAccessKind::Ip(InstructionPosition(ip + 1))
}

fn to_frame(val: u16) -> FrameMemoryAddress {
    FrameMemoryAddress(val)
}
