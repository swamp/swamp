/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use std::cmp::PartialEq;
use std::fmt::Write;
use swamp_vm_debug_types::{
    DecoratedMemoryKind, DecoratedOpcode, DecoratedOperand, DecoratedOperandAccessKind,
    DecoratedOperandOrigin, FrameMemoryAttribute, FrameMemoryInfo,
};
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{
    BinaryInstruction, ConstantMemoryAddress, FrameMemoryAddress, FrameMemorySize,
    InstructionPosition, InstructionPositionOffset, MemorySize, Meta,
};
use yansi::{Color, Paint};

#[derive(Eq, PartialEq, Clone)]
pub struct SourceFileLineInfo {
    pub row: usize,
    pub col: usize,
    pub line: String,
    pub relative_file_name: String,
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
) -> String {
    let mut string = String::new();
    let mut last_frame_size: u16 = 0;

    let mut last_line_info = SourceFileLineInfo {
        row: usize::MAX,
        col: usize::MAX,
        line: String::new(),
        relative_file_name: String::new(),
    };
    for (ip_offset, instruction) in binary_instructions.iter().enumerate() {
        let ip_index = instruction_position_base.0 + ip_offset as u16;
        if OpCode::Enter as u8 == instruction.opcode {
            last_frame_size = instruction.operands[0];
        }
        if let Some(found) = ip_infos.get(&InstructionPosition(ip_index as u16)) {
            if last_line_info.relative_file_name != found.relative_file_name {
                writeln!(
                    string,
                    "{}:{}:{}",
                    found.relative_file_name.bright_blue(),
                    found.row,
                    found.col
                )
                .expect("should work");
                last_line_info = found.clone();
            }
            writeln!(
                string,
                "{:4} {} {}",
                found.row,
                "|".green(),
                convert_tabs_to_spaces(&found.line),
            )
            .expect("TODO: panic message");
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
fn memory_kind_color(kind: &DecoratedMemoryKind) -> String {
    let short_string = match kind {
        DecoratedMemoryKind::B8 => "b8",
        DecoratedMemoryKind::U8 => "u8",
        DecoratedMemoryKind::U16 => "u16",
        DecoratedMemoryKind::U32 => "u32",
        DecoratedMemoryKind::S32 => "i32",
        DecoratedMemoryKind::Fp32 => "fp32",
        DecoratedMemoryKind::Octets => "*b8",
        DecoratedMemoryKind::IndirectHeapPointer => "(heap_ptr)",
    };

    format!("{}", short_string)
}

#[must_use]
pub fn disasm_instructions_no_color(
    binary_instruction: &[BinaryInstruction],
    descriptions: &[String],
    ip_infos: &SeqMap<InstructionPosition, String>,
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
    let decorated = disasm(binary_instruction, frame_size);

    let name = format!("{:5}", decorated.name.blue());

    let mut converted_operands = Vec::new();
    let mut converted_comments = Vec::new();
    let mut memory_comments = Vec::new();

    for operand in decorated.operands {
        let operand_addr = match &operand.kind {
            DecoratedOperandAccessKind::ReadFrameAddress(addr, memory_kind, attr) => Some(addr),
            DecoratedOperandAccessKind::WriteFrameAddress(addr, memory_kind, attr) => Some(addr),
            _ => None,
        };

        let (new_str, comment_str) = match &operand.kind {
            DecoratedOperandAccessKind::ReadFrameAddress(addr, memory_kind, attr) => {
                let color = if attr.is_temporary {
                    Color::BrightGreen
                } else {
                    Color::Green
                };

                (
                    format!("{}{}", "$".fg(color), format!("{:04X}", addr.0).fg(color)),
                    memory_kind_color(&memory_kind),
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
                    memory_kind_color(&memory_kind),
                )
            }
            DecoratedOperandAccessKind::ReadIndirectPointer(addr) => {
                let color = Color::Green;

                (
                    format!("({}{})", "$".fg(color), format!("{:04X}", addr.0).fg(color)),
                    String::new(),
                )
            }
            DecoratedOperandAccessKind::ConstantAddress(addr) => {
                let color = Color::Yellow;

                (
                    format!("{}{}", "@#".fg(color), format!("{:08X}", addr.0).fg(color)),
                    "constant".to_string(),
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
                format!("{}", format!("{data:08X}",).magenta()),
                format!("{}{}", "int:", *data as i32),
            ),
            DecoratedOperandAccessKind::ImmediateU16(data) => (
                format!("{}", format!("{data:04X}",).magenta()),
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
        };
        converted_operands.push(new_str);

        let memory_comment = operand_addr.and_then(|addr| {
            if let Some(info) = memory_infos.get(addr) {
                Some(format!("{}", info.kind))
            } else {
                None
            }
        });

        if let Some(comment) = memory_comment {
            memory_comments.push(comment);
        }

        if !comment_str.is_empty() {
            converted_comments.push(comment_str);
        }
    }

    let comment_suffix = if converted_comments.is_empty() {
        String::new()
    } else {
        format!(" ({})", converted_comments.join(", "))
    };

    let memory_comment_suffix = if memory_comments.is_empty() {
        String::new()
    } else {
        format!(" {}", memory_comments.join(", "))
    };

    let total_comment = format!(
        "{} {}{}",
        meta.comment, memory_comment_suffix, comment_suffix
    );
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
    comment: &str,
) -> String {
    let decorated = disasm(binary_instruction, frame_memory_size);

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
            DecoratedOperandAccessKind::ReadIndirectPointer(addr) => {
                format!("({}{})", "$", format!("{:04X}", addr.0))
            }
            DecoratedOperandAccessKind::ConstantAddress(addr) => {
                format!("{}{}", "@#", format!("{:08X}", addr.0))
            }
            /*
            DecoratedOperandKind::HeapAddress(addr) => {
                format!("{}{}", "@", format!("{:04X}", addr.0))
            }
            DecoratedOperandKind::ReadIndirectMemory(addr, memory_offset, memory_kind) => {
                format!("({}{})", "$", format!("{:04X}+{}", addr.0, memory_offset.0))
            }
            DecoratedOperandKind::WriteIndirectMemory(addr, memory_offset, memory_kind) => {
                format!("({}{})", "$", format!("{:04X}+{}", addr.0, memory_offset.0))
            }

             */
            DecoratedOperandAccessKind::MemorySize(data) => format!("{}", format!("{:X}", data.0)),

            DecoratedOperandAccessKind::Ip(ip) => {
                format!("{}{}", "@", format!("{:X}", ip.0))
            }
            DecoratedOperandAccessKind::ImmediateU32(data) => format!("{}", format!("{data:08X}",)),
            DecoratedOperandAccessKind::ImmediateU16(data) => format!("{}", format!("{data:04X}",)),
            DecoratedOperandAccessKind::ImmediateU8(data) => format!("{}", format!("{data:02X}",)),
            DecoratedOperandAccessKind::CountU16(data) => format!("{}", format!("{data:04X}",)),
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
) -> DecoratedOpcode {
    let opcode: OpCode = binary_instruction.opcode.into();
    let operands = binary_instruction.operands;

    let operands_slice: &[DecoratedOperandAccessKind] = match opcode {
        OpCode::Hlt => &[],
        OpCode::Ret => &[],

        OpCode::Panic => &[to_read_frame(
            operands[0],
            DecoratedMemoryKind::IndirectHeapPointer,
            frame_memory_size,
        )],
        OpCode::LdConst => {
            let data = ((operands[2] as u32) << 16) | operands[1] as u32;

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
                DecoratedOperandAccessKind::ConstantAddress(ConstantMemoryAddress(data)),
                DecoratedOperandAccessKind::MemorySize(MemorySize(operands[3])),
            ]
        }

        OpCode::Ld32 => {
            let data = ((operands[2] as u32) << 16) | operands[1] as u32;

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
                DecoratedOperandAccessKind::ImmediateU32(data),
            ]
        }
        OpCode::Ld16 => {
            let data = operands[1] as u16;

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::U16, frame_memory_size),
                DecoratedOperandAccessKind::ImmediateU16(data),
            ]
        }

        OpCode::Ld8 => {
            let data = operands[1];

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::U8, frame_memory_size),
                DecoratedOperandAccessKind::ImmediateU8(data),
            ]
        }

        // Integer
        OpCode::AddI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::SubI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::MulI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::DivI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::ModI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::NegI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        // Fixed
        OpCode::AddF32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::MulF32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::DivF32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        OpCode::ModF32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::SubF32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::NegF32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        OpCode::LtF32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        OpCode::LeF32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        OpCode::GtF32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::GeF32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        OpCode::LtI32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        OpCode::LeI32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        OpCode::GtI32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::GeI32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        OpCode::FloatToString => &[
            to_write_frame(
                operands[0],
                DecoratedMemoryKind::IndirectHeapPointer,
                frame_memory_size,
            ),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatRound => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatFloor => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatSqrt => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatSign => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatAbs => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatSin => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatCos => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatAcos => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatAsin => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatAtan2 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatMin => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatMax => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatClamp => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[3], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::FloatPseudoRandom => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::Eq8Imm => {
            let data = operands[1];

            &[
                to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
                DecoratedOperandAccessKind::ImmediateU8(data),
            ]
        }
        OpCode::Eq32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],
        OpCode::Ne32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::Not8 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U8, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U8, frame_memory_size),
        ],

        OpCode::Tst8 => &[to_read_frame(
            operands[0],
            DecoratedMemoryKind::U8,
            frame_memory_size,
        )],

        OpCode::Stz => &[to_write_frame(
            operands[0],
            DecoratedMemoryKind::U8,
            frame_memory_size,
        )],

        OpCode::Stnz => &[to_write_frame(
            operands[0],
            DecoratedMemoryKind::U8,
            frame_memory_size,
        )],

        OpCode::Cmp8 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::U8, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U8, frame_memory_size),
        ],

        OpCode::Cmp32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::Cmp => &[
            to_read_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[1])),
        ],

        OpCode::Bnz => &[to_jmp_ip(operands[0])],
        OpCode::Bz => &[to_jmp_ip(operands[0])],
        OpCode::Call => &[to_jmp_ip(operands[0])],
        OpCode::HostCall => &[
            DecoratedOperandAccessKind::ImmediateU16(operands[0]),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[1])),
        ],
        OpCode::Enter => &[DecoratedOperandAccessKind::MemorySize(MemorySize(
            operands[0],
        ))],
        OpCode::Jmp => &[to_jmp_ip(operands[0])],
        OpCode::Mov => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
        ],
        OpCode::MovLp => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
        ],
        OpCode::Nop => &[],

        OpCode::VecPop => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
            DecoratedOperandAccessKind::CountU16(operands[3]),
        ],

        OpCode::VecFromSlice => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
            DecoratedOperandAccessKind::CountU16(operands[3]),
        ],

        OpCode::VecSubscript => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::VecSubscriptMut => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[3], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::VecIterInit => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandAccessKind::ReadIndirectPointer(FrameMemoryAddress(operands[1])),
        ],

        OpCode::VecIterNext => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_write_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_jmp_ip(operands[2]),
        ],

        OpCode::VecIterNextPair => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_write_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_write_frame(operands[2], DecoratedMemoryKind::Octets, frame_memory_size),
            to_jmp_ip(operands[3]),
        ],

        OpCode::VecClear => &[to_write_frame(
            operands[0],
            DecoratedMemoryKind::Octets,
            frame_memory_size,
        )],

        OpCode::VecPush => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::VecRemoveIndex => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::VecRemoveIndexGetValue => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::VecLen => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::VecCreate => &[to_write_frame(
            operands[0],
            DecoratedMemoryKind::U32,
            frame_memory_size,
        )],

        OpCode::VecGet => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::VecIsEmpty => &[to_read_frame(
            operands[0],
            DecoratedMemoryKind::Octets,
            frame_memory_size,
        )],

        OpCode::MapNewFromPairs => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[2])),
            DecoratedOperandAccessKind::MemorySize(MemorySize(operands[3])),
            DecoratedOperandAccessKind::CountU16(operands[4]),
        ],

        OpCode::MapIterInit => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandAccessKind::ReadIndirectPointer(FrameMemoryAddress(operands[1])),
        ],

        OpCode::MapIterNext => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_write_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_jmp_ip(operands[2]),
        ],

        OpCode::MapIterNextPair => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_write_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_write_frame(operands[2], DecoratedMemoryKind::Octets, frame_memory_size),
            to_jmp_ip(operands[3]),
        ],

        OpCode::MapLen => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::MapRemove => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::MapSubscript => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::MapSubscriptMutCreate => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::MapSubscriptMut => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::MapHas => &[
            //  sets the Z flag
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
        ],

        OpCode::RangeIterInit => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandAccessKind::ReadIndirectPointer(FrameMemoryAddress(operands[1])),
        ],

        OpCode::RangeIterNext => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_write_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_jmp_ip(operands[2]),
        ],

        OpCode::RangeIterNextPair => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_write_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            to_write_frame(operands[2], DecoratedMemoryKind::Octets, frame_memory_size),
            to_jmp_ip(operands[3]),
        ],

        OpCode::StringFromConstantSlice => {
            let data = ((operands[2] as u32) << 16) | operands[1] as u32;

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
                DecoratedOperandAccessKind::ConstantAddress(ConstantMemoryAddress(data)),
                DecoratedOperandAccessKind::MemorySize(MemorySize(operands[3])),
            ]
        }

        OpCode::StringLen => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(
                operands[1],
                DecoratedMemoryKind::IndirectHeapPointer,
                frame_memory_size,
            ),
        ],
        OpCode::StringAppend => &[
            to_write_frame(
                operands[0],
                DecoratedMemoryKind::IndirectHeapPointer,
                frame_memory_size,
            ),
            to_read_frame(
                operands[1],
                DecoratedMemoryKind::IndirectHeapPointer,
                frame_memory_size,
            ),
            to_read_frame(
                operands[2],
                DecoratedMemoryKind::IndirectHeapPointer,
                frame_memory_size,
            ),
        ],

        OpCode::IntToRnd => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::IntAbs => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::IntMin => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::IntMax => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::IntClamp => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[3], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::IntToString => &[
            to_write_frame(
                operands[0],
                DecoratedMemoryKind::IndirectHeapPointer,
                frame_memory_size,
            ),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::BoolToString => &[
            to_write_frame(
                operands[0],
                DecoratedMemoryKind::IndirectHeapPointer,
                frame_memory_size,
            ),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],

        OpCode::IntToFloat => &[
            to_write_frame(operands[0], DecoratedMemoryKind::U32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::U32, frame_memory_size),
        ],
        /*
                OpCode::Alloc => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandKind::MemorySize(MemorySize(operands[1])),
        ],
        OpCode::LtU16 => todo!(),
        OpCode::St32x => {
            let data = ((operands[3] as u32) << 16) | operands[2] as u32;
            &[
                DecoratedOperandKind::WriteIndirectMemory(
                    MemoryAddress(operands[0]),
                    MemoryOffset(operands[1]),
                    DecoratedMemoryKind::U32,
                ),
                DecoratedOperandKind::ImmediateU32(data),
            ]
        }
        OpCode::Stx => &[
            DecoratedOperandKind::WriteIndirectMemory(
                MemoryAddress(operands[0]),
                MemoryOffset(operands[1]),
                DecoratedMemoryKind::Octets,
            ),
            DecoratedOperandKind::ReadFrameAddress(
                FrameMemoryAddress(operands[2]),
                DecoratedMemoryKind::Octets,
                FrameMemoryAttribute {
                    is_temporary: operands[2] >= frame_memory_size.0,
                },
            ),
            DecoratedOperandKind::MemorySize(MemorySize(operands[3])),
        ],
        OpCode::Ldx => &[
            DecoratedOperandKind::ReadIndirectMemory(
                MemoryAddress(operands[0]),
                MemoryOffset(operands[1]),
                DecoratedMemoryKind::Octets,
            ),
            DecoratedOperandKind::ReadFrameAddress(
                FrameMemoryAddress(operands[2]),
                DecoratedMemoryKind::Octets,
                FrameMemoryAttribute {
                    is_temporary: operands[2] >= frame_memory_size.0,
                },
            ),
            DecoratedOperandKind::MemorySize(MemorySize(operands[3])),
        ],

         */
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
    mem: DecoratedMemoryKind,
    frame_memory_size: FrameMemorySize,
) -> DecoratedOperandAccessKind {
    let is_temporary = addr >= frame_memory_size.0;
    DecoratedOperandAccessKind::WriteFrameAddress(
        to_frame(addr),
        mem,
        FrameMemoryAttribute { is_temporary },
    )
}

fn to_read_frame(
    addr: u16,
    mem: DecoratedMemoryKind,
    frame_memory_size: FrameMemorySize,
) -> DecoratedOperandAccessKind {
    let is_temporary = addr >= frame_memory_size.0;
    DecoratedOperandAccessKind::ReadFrameAddress(
        to_frame(addr),
        mem,
        FrameMemoryAttribute { is_temporary },
    )
}

fn to_jmp_ip(ip: u16) -> DecoratedOperandAccessKind {
    DecoratedOperandAccessKind::Ip(InstructionPosition(ip + 1))
}

fn to_frame(val: u16) -> FrameMemoryAddress {
    FrameMemoryAddress(val)
}
