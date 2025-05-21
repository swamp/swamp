/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use fixed32::Fp;
use seq_map::SeqMap;
use source_map_cache::{SourceMapLookup, SourceMapWrapper};
use source_map_node::FileId;
use std::cmp::PartialEq;
use std::fmt::Write;
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::types::{
    BasicType, DecoratedOpcode, DecoratedOperand, DecoratedOperandAccessKind,
    DecoratedOperandOrigin, FrameMemoryAttribute, FrameMemoryInfo, FramePlacedType, PathInfo,
    TypedRegister, b8_type, bytes_type, float_type, int_type, map_iter_type, map_type,
    pointer_type_again, range_iter_type, range_type, slice_type, string_type, u8_type, u32_type,
    vec_iter_type, vec_type,
};
use swamp_vm_types::{
    BinaryInstruction, FrameMemoryAddress, HeapMemoryAddress, InstructionPosition,
    InstructionPositionOffset, MemoryOffset, MemorySize, Meta, ProgramCounterDelta, RegIndex,
};
//use yansi::{Color, Paint};

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

    let mut last_line_info = SourceFileLineInfo {
        row: usize::MAX,
        file_id: usize::MAX,
    };
    let mut expected_next_row_to_show = 1;

    for (ip_offset, instruction) in binary_instructions.iter().enumerate() {
        let absolute_pc = instruction_position_base.0 + ip_offset as u32;

        if let Some(found) = ip_infos.get(&InstructionPosition(absolute_pc)) {
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
                    .unwrap_or_else(|| panic!("wrong row: {row_to_display}"));
                writeln!(
                    string,
                    "{:4} {} {}",
                    row_to_display,
                    tinter::green("|"),
                    convert_tabs_to_spaces(line),
                )
                .expect("TODO: panic message");
            }

            expected_next_row_to_show = found.row + 1;
        }

        writeln!(
            string,
            "     {:04X}> {}",
            absolute_pc,
            disasm_color(
                instruction,
                memory_infos,
                &meta[ip_offset],
                &InstructionPosition(absolute_pc)
            )
        )
        .expect("TODO: panic message");
    }

    string
}
fn memory_kind_color(kind: Option<PathInfo>) -> String {
    kind.map_or_else(String::new, |path_info| path_info.convert_to_string())
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

    for (ip_index, (instruction, comment)) in
        binary_instruction.iter().zip(descriptions).enumerate()
    {
        if let Some(found) = ip_infos.get(&InstructionPosition(ip_index as u32)) {
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
            disasm_no_color(instruction, frame_memory_info, &comment_to_use)
        );
    }

    string
}

#[allow(clippy::too_many_lines)]
#[must_use]
pub fn disasm_color(
    binary_instruction: &BinaryInstruction,
    memory_infos: &FrameMemoryInfo,
    meta: &Meta,
    current_pc: &InstructionPosition,
) -> String {
    let decorated = disasm(binary_instruction, memory_infos);

    let name = format!("{:7}", tinter::blue(decorated.name));

    let mut converted_operands = Vec::new();
    let mut converted_comments = Vec::new();

    for operand in decorated.operands {
        let operand_addr = operand.kind.path_info();

        let (new_str, comment_str) = match &operand.kind {
            DecoratedOperandAccessKind::ReadRegister(addr, memory_kind, attr) => {
                let color = if attr.is_temporary {
                    tinter::Color::BrightGreen
                } else {
                    tinter::Color::Green
                };

                (
                    format!("{}", tinter::color(color, addr)),
                    memory_kind_color(memory_kind.clone()),
                )
            }
            DecoratedOperandAccessKind::WriteRegister(addr, memory_kind, attr) => {
                let color = if attr.is_temporary {
                    tinter::Color::BrightMagenta
                } else {
                    tinter::Color::Red
                };

                (
                    format!("{}", tinter::color(color, format!("{addr}"))),
                    memory_kind_color(memory_kind.clone()),
                )
            }
            DecoratedOperandAccessKind::AbsoluteMemoryPosition(addr) => {
                let color = tinter::Color::Green;

                (
                    format!(
                        "{}{}",
                        tinter::color(color, "%$"),
                        tinter::color(color, format!("{:08X}", addr.0))
                    ),
                    String::new(),
                )
            }
            DecoratedOperandAccessKind::ReadIndirectPointer(addr) => {
                let color = tinter::Color::Green;

                (
                    format!(
                        "({}{})",
                        tinter::color(color, "$"),
                        tinter::color(color, format!("{:04X}", addr.0))
                    ),
                    String::new(),
                )
            }
            DecoratedOperandAccessKind::DeltaPc(delta) => (
                format!(
                    "{} {:04X}",
                    tinter::bright_cyan(format!("{}", delta.0)),
                    (*current_pc + ProgramCounterDelta(1) + *delta).0
                ),
                String::new(),
            ),
            DecoratedOperandAccessKind::AbsolutePc(ip) => (
                format!(
                    "{}{}",
                    tinter::cyan("@"),
                    tinter::bright_cyan(format!("{:X}", ip.0))
                ),
                String::new(),
            ),
            DecoratedOperandAccessKind::MemorySize(data) => (
                format!("#{}", tinter::yellow(format!("{}", data.0))),
                format!("{}{:04X}", "hex:", data.0),
            ),
            DecoratedOperandAccessKind::ImmediateU32(data) => (
                format!("{}", tinter::magenta(format!("0x{data:X}",))),
                format!(
                    "{}{} {}{}",
                    "int:",
                    *data as i32,
                    "fixed:",
                    Fp::from_raw(*data as i32)
                ),
            ),
            DecoratedOperandAccessKind::ImmediateU16(data) => (
                format!("{}", tinter::magenta(format!("0x{data:X}",))),
                format!("{}{}", "int:", *data as i32),
            ),
            DecoratedOperandAccessKind::ImmediateU8(data) => (
                format!("{}", tinter::magenta(format!("0x{data:02X}"))),
                format!("{}{}", "int:", *data as i8),
            ),
            DecoratedOperandAccessKind::CountU8(data) => (
                format!("#{}", tinter::yellow(format!("{data}",))),
                format!("{}: {:02X}", "count", *data),
            ),
            DecoratedOperandAccessKind::CountU16(data) => (
                format!("#{}", tinter::yellow(format!("{data}",))),
                format!("{}: {:04X}", "count", *data),
            ),
            DecoratedOperandAccessKind::ReadFrameMemoryAddress(data) => (
                format!("{}", tinter::yellow(format!("{data}",))),
                String::new(),
            ),
            DecoratedOperandAccessKind::WriteFrameMemoryAddress(data) => (
                format!("{}", tinter::red(format!("{data}",))),
                String::new(),
            ),
            DecoratedOperandAccessKind::WriteBaseRegWithOffset(base_reg, offset) => (
                format!("[{} #{}]", tinter::red(base_reg), tinter::yellow(offset.0)),
                String::new(),
            ),
            DecoratedOperandAccessKind::ReadBaseRegWithOffset(base_reg, offset) => (
                format!(
                    "[{} #{}]",
                    tinter::green(base_reg),
                    tinter::yellow(offset.0)
                ),
                String::new(),
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
        format!(
            " {} {}",
            tinter::bright_black(";"),
            tinter::bright_black(total_comment)
        )
    };

    format!("{} {}{}", name, converted_operands.join(" "), print_comment)
}

#[must_use]
pub fn disasm_no_color(
    binary_instruction: &BinaryInstruction,
    frame_memory_info: &FrameMemoryInfo,
    comment: &str,
) -> String {
    let decorated = disasm(binary_instruction, frame_memory_info);

    let name = decorated.name.to_string();

    let mut converted_operands = Vec::new();

    for operand in decorated.operands {
        let new_str = match operand.kind {
            DecoratedOperandAccessKind::ReadRegister(addr, _memory_kind, _attr) => {
                format!("{}{}", "$", format!("{}", addr))
            }
            DecoratedOperandAccessKind::WriteRegister(addr, _memory_kind, _attr) => {
                format!("{}{}", "$", format!("{}", addr))
            }
            DecoratedOperandAccessKind::AbsoluteMemoryPosition(addr) => {
                format!("{}{}", "%$", format!("{:08X}", addr.0))
            }
            DecoratedOperandAccessKind::ReadIndirectPointer(addr) => {
                format!("({}{})", "$", format!("{:04X}", addr.0))
            }
            DecoratedOperandAccessKind::MemorySize(data) => format!("{:X}", data.0).to_string(),

            DecoratedOperandAccessKind::DeltaPc(delta) => {
                format!("{}{}", "", format!("{}", delta.0))
            }
            DecoratedOperandAccessKind::AbsolutePc(ip) => {
                format!("{}{}", "@", format!("{:X}", ip.0))
            }
            DecoratedOperandAccessKind::ImmediateU32(data) => format!("{data:08X}",).to_string(),
            DecoratedOperandAccessKind::ImmediateU16(data) => format!("{data:04X}",).to_string(),
            DecoratedOperandAccessKind::ImmediateU8(data) => format!("{data:02X}",).to_string(),
            DecoratedOperandAccessKind::CountU16(data) => format!("{data:04X}",).to_string(),
            DecoratedOperandAccessKind::CountU8(data) => format!("{data:02X}",).to_string(),
            DecoratedOperandAccessKind::ReadFrameMemoryAddress(data) => {
                format!("{data}",).to_string()
            }
            DecoratedOperandAccessKind::WriteFrameMemoryAddress(data) => {
                format!("{data}",).to_string()
            }
            DecoratedOperandAccessKind::WriteBaseRegWithOffset(base_reg, offset) => {
                format!("[R{} #{}]", base_reg, offset.0)
            }
            DecoratedOperandAccessKind::ReadBaseRegWithOffset(base_reg, offset) => {
                format!("[R{} #{}]", base_reg, offset.0)
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
    frame_memory_info: &FrameMemoryInfo,
) -> DecoratedOpcode {
    let opcode: OpCode = binary_instruction.opcode.into();
    let operands = binary_instruction.operands;

    let operands_slice: &[DecoratedOperandAccessKind] = match opcode {
        OpCode::Hlt | OpCode::Ret | OpCode::Brk => &[],
        OpCode::Trap => &[DecoratedOperandAccessKind::ImmediateU8(operands[0])],
        OpCode::Panic => &[to_read_reg(operands[0], &string_type(), frame_memory_info)],

        OpCode::St32UsingPtrWithOffset => {
            let offset = u16::from_le_bytes([operands[1], operands[2]]);

            &[
                DecoratedOperandAccessKind::WriteBaseRegWithOffset(
                    RegIndex(operands[0]),
                    MemoryOffset(offset),
                ),
                to_read_reg(operands[3], &int_type(), frame_memory_info),
            ]
        }

        OpCode::St8UsingPtrWithOffset => {
            let offset = u16::from_le_bytes([operands[1], operands[2]]);

            &[
                DecoratedOperandAccessKind::WriteBaseRegWithOffset(
                    RegIndex(operands[0]),
                    MemoryOffset(offset),
                ),
                to_read_reg(operands[3], &int_type(), frame_memory_info),
            ]
        }

        OpCode::St16UsingPtrWithOffset => {
            let offset = u16::from_le_bytes([operands[1], operands[2]]);

            &[
                DecoratedOperandAccessKind::WriteBaseRegWithOffset(
                    RegIndex(operands[0]),
                    MemoryOffset(offset),
                ),
                to_read_reg(operands[3], &int_type(), frame_memory_info),
            ]
        }

        OpCode::StRegToFrame => {
            let frame_ptr_offset = u16::from_le_bytes([operands[0], operands[1]]);

            &[
                DecoratedOperandAccessKind::WriteFrameMemoryAddress(FrameMemoryAddress(
                    frame_ptr_offset,
                )),
                to_read_reg(operands[2], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::CountU8(operands[3]),
            ]
        }

        OpCode::Mov16FromImmediateValue => {
            let immediate_value = u16::from_le_bytes([operands[1], operands[2]]);

            &[
                to_write_reg(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ImmediateU16(immediate_value),
            ]
        }

        OpCode::Mov32FromImmediateValue => {
            let immediate_value =
                u32::from_le_bytes([operands[1], operands[2], operands[3], operands[4]]);

            &[
                to_write_reg(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ImmediateU32(immediate_value),
            ]
        }

        OpCode::Ld8FromPointerWithOffset => {
            let offset = u16::from_le_bytes([operands[2], operands[3]]);

            &[
                to_write_reg(operands[0], &b8_type(), frame_memory_info),
                DecoratedOperandAccessKind::ReadBaseRegWithOffset(
                    RegIndex(operands[1]),
                    MemoryOffset(offset),
                ),
            ]
        }

        OpCode::Ld16FromPointerWithOffset => {
            let offset = u16::from_le_bytes([operands[2], operands[3]]);

            &[
                to_write_reg(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ReadBaseRegWithOffset(
                    RegIndex(operands[1]),
                    MemoryOffset(offset),
                ),
            ]
        }

        OpCode::Ld32FromPointerWithOffset => {
            let offset = u16::from_le_bytes([operands[2], operands[3]]);

            &[
                to_write_reg(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ReadBaseRegWithOffset(
                    RegIndex(operands[1]),
                    MemoryOffset(offset),
                ),
            ]
        }

        OpCode::Ld32FromAbsoluteAddress => {
            let data = u32::from_le_bytes([operands[1], operands[2], operands[3], operands[4]]);

            &[
                to_write_reg(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::AbsoluteMemoryPosition(HeapMemoryAddress(data)),
            ]
        }

        OpCode::LdRegFromFrame => {
            let frame_pointer_offset = u16::from_le_bytes([operands[1], operands[2]]);

            &[
                to_write_reg(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ReadFrameMemoryAddress(FrameMemoryAddress(
                    frame_pointer_offset,
                )),
                DecoratedOperandAccessKind::CountU8(operands[3]),
            ]
        }

        OpCode::Ld8FromAbsoluteAddress => {
            let data = u32::from_le_bytes([operands[1], operands[2], operands[3], operands[4]]);

            &[
                to_write_reg(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::AbsoluteMemoryPosition(HeapMemoryAddress(data)),
            ]
        }

        OpCode::Mov8FromImmediateValue => {
            let data = operands[1];

            &[
                to_write_reg(operands[0], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ImmediateU8(data),
            ]
        }

        // Integer
        OpCode::AddU32 => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::AddU32Imm => {
            let immediate_value =
                u32::from_le_bytes([operands[2], operands[3], operands[4], operands[5]]);
            &[
                to_write_reg(operands[0], &int_type(), frame_memory_info),
                to_read_reg(operands[1], &int_type(), frame_memory_info),
                DecoratedOperandAccessKind::ImmediateU32(immediate_value),
            ]
        }
        OpCode::SubU32 => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::MulU32 => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::DivI32 => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::ModI32 => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
        ],
        OpCode::NegI32 => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],

        // Fixed
        OpCode::MulF32 => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
            to_read_reg(operands[2], &float_type(), frame_memory_info),
        ],
        OpCode::DivF32 => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
            to_read_reg(operands[2], &float_type(), frame_memory_info),
        ],

        OpCode::ModF32 => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
            to_read_reg(operands[2], &float_type(), frame_memory_info),
        ],

        OpCode::LtI32 => &[
            to_read_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::LeI32 => &[
            to_read_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::GtI32 => &[
            to_read_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],
        OpCode::GeI32 => &[
            to_read_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],
        OpCode::LtU32 => &[
            to_read_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],
        OpCode::GeU32 => &[
            to_read_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],
        OpCode::FloatToString => &[
            to_write_reg(operands[0], &string_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatRound => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatFloor => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatSqrt => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatSign => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatAbs => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatSin => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatCos => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatAcos => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatAsin => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatAtan2 => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::FloatMin => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
            to_read_reg(operands[2], &float_type(), frame_memory_info),
        ],

        OpCode::FloatMax => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
            to_read_reg(operands[2], &float_type(), frame_memory_info),
        ],

        OpCode::FloatClamp => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
            to_read_reg(operands[2], &float_type(), frame_memory_info),
            to_read_reg(operands[3], &float_type(), frame_memory_info),
        ],

        OpCode::FloatPseudoRandom => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &float_type(), frame_memory_info),
        ],

        OpCode::Eq8Imm => {
            let data = operands[1];

            &[
                to_read_reg(operands[0], &u8_type(), frame_memory_info),
                DecoratedOperandAccessKind::ImmediateU8(data),
            ]
        }

        OpCode::MovToTFlagFromReg => &[to_read_reg(operands[0], &u8_type(), frame_memory_info)],

        OpCode::MovFromTFlagToReg => &[to_write_reg(operands[0], &b8_type(), frame_memory_info)],

        OpCode::MovFromNotTFlagToReg => &[to_write_reg(operands[0], &b8_type(), frame_memory_info)],

        OpCode::CmpReg => &[
            to_read_reg(operands[0], &u32_type(), frame_memory_info),
            to_read_reg(operands[1], &u32_type(), frame_memory_info),
        ],

        OpCode::CmpBlock => &[
            to_read_reg(operands[0], &u32_type(), frame_memory_info),
            to_read_reg(operands[1], &u32_type(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(u8_pair_to_u16(
                operands[2],
                operands[3],
            ))),
        ],

        OpCode::BFalse | OpCode::BTrue => &[to_branch_offset(i16::from_le_bytes([
            operands[0],
            operands[1],
        ]))],
        OpCode::Call => &[to_absolute_branch_pc(u32::from_le_bytes([
            operands[0],
            operands[1],
            operands[2],
            operands[3],
        ]))],
        OpCode::NotT => &[],
        OpCode::HostCall => &[
            DecoratedOperandAccessKind::ImmediateU16(u8_pair_to_u16(operands[0], operands[1])),
            DecoratedOperandAccessKind::MemorySize(MemorySize(u8_pair_to_u16(
                operands[2],
                operands[3],
            ))),
        ],
        OpCode::Enter => &[DecoratedOperandAccessKind::MemorySize(MemorySize(
            u8_pair_to_u16(operands[0], operands[1]),
        ))],
        OpCode::B => &[to_branch_offset(i16::from_le_bytes([
            operands[0],
            operands[1],
        ]))],
        OpCode::BlockCopyWithOffsets => {
            let dst_offset = u16::from_le_bytes([operands[1], operands[2]]);
            let src_offset = u16::from_le_bytes([operands[4], operands[5]]);

            &[
                DecoratedOperandAccessKind::WriteBaseRegWithOffset(
                    RegIndex(operands[0]),
                    MemoryOffset(dst_offset),
                ),
                DecoratedOperandAccessKind::ReadBaseRegWithOffset(
                    RegIndex(operands[3]),
                    MemoryOffset(src_offset),
                ),
                DecoratedOperandAccessKind::MemorySize(MemorySize(u16::from_le_bytes([
                    operands[6],
                    operands[7],
                ]))),
            ]
        }

        OpCode::BlockCopy => &[
            to_write_reg(operands[0], &bytes_type(), frame_memory_info),
            to_read_reg(operands[1], &bytes_type(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(u16::from_le_bytes([
                operands[2],
                operands[3],
            ]))),
        ],

        OpCode::FrameMemClr => &[
            DecoratedOperandAccessKind::WriteFrameMemoryAddress(FrameMemoryAddress(
                u8_pair_to_u16(operands[0], operands[1]),
            )),
            DecoratedOperandAccessKind::MemorySize(MemorySize(u8_pair_to_u16(
                operands[2],
                operands[3],
            ))),
        ],
        OpCode::MovReg => &[
            to_write_reg(operands[0], &u32_type(), frame_memory_info),
            to_read_reg(operands[1], &u32_type(), frame_memory_info),
        ],

        OpCode::LdPtrFromEffectiveAddress => {
            let data = u16::from_le_bytes([operands[1], operands[2]]);
            &[
                to_write_reg(operands[0], &pointer_type_again(), frame_memory_info),
                DecoratedOperandAccessKind::ReadFrameMemoryAddress(FrameMemoryAddress(data)),
            ]
        }

        OpCode::LoadEffectiveAddressIndexMultiplier => {
            let dst_offset = u16::from_le_bytes([operands[2], operands[3]]);
            let element_size = u16::from_le_bytes([operands[5], operands[6]]);
            &[
                to_write_reg(operands[0], &pointer_type_again(), frame_memory_info),
                DecoratedOperandAccessKind::WriteBaseRegWithOffset(
                    RegIndex(operands[1]),
                    MemoryOffset(dst_offset),
                ),
                to_read_reg(operands[4], &u32_type(), frame_memory_info),
                DecoratedOperandAccessKind::MemorySize(MemorySize(element_size)),
            ]
        }
        OpCode::Nop => &[],

        OpCode::SliceFromHeap => &[
            to_write_reg(operands[0], &slice_type(), frame_memory_info),
            to_read_reg(operands[1], &pointer_type_again(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(u8_pair_to_u16(
                operands[2],
                operands[3],
            ))),
            DecoratedOperandAccessKind::CountU16(0),
        ],

        OpCode::SlicePairFromHeap => &[
            to_write_reg(operands[0], &slice_type(), frame_memory_info),
            to_read_reg(operands[1], &pointer_type_again(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(u8_pair_to_u16(
                operands[2],
                operands[3],
            ))),
            DecoratedOperandAccessKind::MemorySize(MemorySize(0)),
            DecoratedOperandAccessKind::CountU16(0),
        ],

        OpCode::VecPop => &[
            to_write_reg(operands[0], &bytes_type(), frame_memory_info),
            to_write_reg(operands[1], &vec_type(), frame_memory_info),
        ],

        OpCode::VecSwap => &[
            to_write_reg(operands[0], &vec_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
        ],

        OpCode::VecSet => &[
            to_write_reg(operands[0], &vec_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &bytes_type(), frame_memory_info),
        ],

        OpCode::VecIterInit => &[
            to_write_reg(operands[0], &vec_iter_type(), frame_memory_info),
            DecoratedOperandAccessKind::ReadIndirectPointer(FrameMemoryAddress(0)),
        ],

        OpCode::VecIterNext => &[
            to_write_reg(operands[0], &vec_iter_type(), frame_memory_info),
            to_write_reg(operands[1], &bytes_type(), frame_memory_info),
            to_branch_offset(i16::from_le_bytes([operands[2], operands[3]])),
        ],

        OpCode::VecIterNextPair => &[
            to_write_reg(operands[0], &vec_iter_type(), frame_memory_info),
            to_write_reg(operands[1], &bytes_type(), frame_memory_info),
            to_write_reg(operands[2], &bytes_type(), frame_memory_info),
            to_branch_offset(i16::from_le_bytes([operands[3], operands[4]])),
        ],

        OpCode::VecClear => &[to_write_reg(operands[0], &vec_type(), frame_memory_info)],

        OpCode::VecPushAddr => &[
            to_write_reg(operands[0], &vec_type(), frame_memory_info),
            to_read_reg(operands[1], &bytes_type(), frame_memory_info),
        ],

        OpCode::VecInitWithLenAndCapacityAddr => {
            let length_count = u16::from_le_bytes([operands[2], operands[3]]);
            let capacity_count = u16::from_le_bytes([operands[4], operands[5]]);
            &[
                to_write_reg(operands[0], &bytes_type(), frame_memory_info),
                to_read_reg(operands[1], &bytes_type(), frame_memory_info),
                DecoratedOperandAccessKind::CountU16(length_count),
                DecoratedOperandAccessKind::CountU16(capacity_count),
            ]
        }

        OpCode::VecRemoveIndex => &[
            to_write_reg(operands[0], &vec_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::VecRemoveIndexGetValue => &[
            to_write_reg(operands[0], &vec_type(), frame_memory_info),
            to_write_reg(operands[1], &vec_type(), frame_memory_info),
            to_read_reg(operands[2], &bytes_type(), frame_memory_info),
        ],

        OpCode::VecCreate => &[
            to_write_reg(operands[0], &vec_type(), frame_memory_info),
            DecoratedOperandAccessKind::CountU16(u8_pair_to_u16(operands[1], operands[2])),
        ],

        OpCode::VecGet => &[
            to_write_reg(operands[0], &bytes_type(), frame_memory_info),
            to_read_reg(operands[1], &vec_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
        ],

        OpCode::VecGetRange => &[
            to_write_reg(operands[0], &bytes_type(), frame_memory_info),
            to_read_reg(operands[1], &bytes_type(), frame_memory_info),
            to_read_reg(operands[2], &range_type(), frame_memory_info),
        ],

        OpCode::MapNewFromPairs => &[
            to_write_reg(operands[0], &map_type(), frame_memory_info),
            to_read_reg(operands[1], &slice_type(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(u8_pair_to_u16(
                operands[2],
                operands[3],
            ))),
            DecoratedOperandAccessKind::MemorySize(MemorySize(u8_pair_to_u16(
                operands[2],
                operands[3],
            ))),
            DecoratedOperandAccessKind::CountU16(u8_pair_to_u16(operands[2], operands[3])),
        ],

        OpCode::MapInitWithCapacityAndKeySizeAddr => {
            let capacity_count = u16::from_le_bytes([operands[1], operands[2]]);
            let key_memory_size = u16::from_le_bytes([operands[3], operands[4]]);
            &[
                to_write_reg(operands[0], &bytes_type(), frame_memory_info),
                DecoratedOperandAccessKind::CountU16(capacity_count),
                DecoratedOperandAccessKind::MemorySize(MemorySize(key_memory_size)),
            ]
        }

        OpCode::MapIterInit => &[
            to_write_reg(operands[0], &map_iter_type(), frame_memory_info),
            to_read_reg(operands[1], &map_type(), frame_memory_info),
        ],

        OpCode::MapIterNext => &[
            to_write_reg(operands[0], &map_iter_type(), frame_memory_info),
            to_write_reg(operands[1], &bytes_type(), frame_memory_info),
            to_branch_offset(i16::from_le_bytes([operands[2], operands[3]])),
        ],

        OpCode::MapIterNextPair => &[
            to_write_reg(operands[0], &map_iter_type(), frame_memory_info),
            to_write_reg(operands[1], &bytes_type(), frame_memory_info),
            to_write_reg(operands[2], &bytes_type(), frame_memory_info),
            to_branch_offset(i16::from_le_bytes([operands[3], operands[4]])),
        ],

        OpCode::MapRemove => &[
            to_write_reg(operands[0], &map_type(), frame_memory_info),
            to_read_reg(operands[1], &bytes_type(), frame_memory_info),
        ],

        OpCode::MapGetEntryLocation => &[
            to_write_reg(operands[0], &bytes_type(), frame_memory_info),
            to_read_reg(operands[1], &map_type(), frame_memory_info),
            to_read_reg(operands[2], &bytes_type(), frame_memory_info),
        ],

        OpCode::MapGetOrReserveEntryLocation => &[
            to_write_reg(operands[0], &map_type(), frame_memory_info),
            to_read_reg(operands[1], &bytes_type(), frame_memory_info),
            to_read_reg(operands[2], &bytes_type(), frame_memory_info),
        ],

        OpCode::MapHas => &[
            //  sets the Z flag
            to_read_reg(operands[0], &map_type(), frame_memory_info),
            to_read_reg(operands[1], &bytes_type(), frame_memory_info),
        ],

        OpCode::RangeIterInit => &[
            to_write_reg(operands[0], &range_iter_type(), frame_memory_info),
            DecoratedOperandAccessKind::ReadIndirectPointer(FrameMemoryAddress(0)),
        ],

        OpCode::RangeIterNext => &[
            to_write_reg(operands[0], &range_iter_type(), frame_memory_info),
            to_write_reg(operands[1], &bytes_type(), frame_memory_info),
            to_branch_offset(i16::from_le_bytes([operands[2], operands[3]])),
        ],

        OpCode::StringAppend => &[
            to_write_reg(operands[0], &string_type(), frame_memory_info),
            to_read_reg(operands[1], &string_type(), frame_memory_info),
            to_read_reg(operands[2], &string_type(), frame_memory_info),
        ],

        OpCode::StringCmp => &[
            to_read_reg(operands[0], &string_type(), frame_memory_info),
            to_read_reg(operands[1], &string_type(), frame_memory_info),
        ],

        OpCode::IntToRnd => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::IntAbs => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::IntMin => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
        ],

        OpCode::IntMax => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
        ],

        OpCode::IntClamp => &[
            to_write_reg(operands[0], &int_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
            to_read_reg(operands[2], &int_type(), frame_memory_info),
            to_read_reg(operands[3], &int_type(), frame_memory_info),
        ],

        OpCode::IntToString => &[
            to_write_reg(operands[0], &string_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::BoolToString => &[
            to_write_reg(operands[0], &string_type(), frame_memory_info),
            to_read_reg(operands[1], &b8_type(), frame_memory_info),
        ],

        OpCode::IntToFloat => &[
            to_write_reg(operands[0], &float_type(), frame_memory_info),
            to_read_reg(operands[1], &int_type(), frame_memory_info),
        ],

        OpCode::Alloc => &[
            to_write_reg(operands[0], &pointer_type_again(), frame_memory_info),
            DecoratedOperandAccessKind::MemorySize(MemorySize(0)),
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

fn u8_pair_to_u16(p1: u8, p2: u8) -> u16 {
    u16::from_le_bytes([p1, p2])
}

fn to_write_reg(
    reg: u8,
    fallback_expected_type: &BasicType,
    frame_memory_info: &FrameMemoryInfo,
) -> DecoratedOperandAccessKind {
    //let maybe_path = frame_memory_info.find_path_to_address_items(FrameMemoryAddress(addr));
    //let is_temporary = maybe_path.is_none();
    let maybe_path = None;
    let is_temporary = false;

    DecoratedOperandAccessKind::WriteRegister(
        to_register(reg, fallback_expected_type),
        maybe_path,
        FrameMemoryAttribute { is_temporary },
    )
}

fn to_read_reg(
    reg: u8,
    fallback_expected_type: &BasicType,
    frame_memory_info: &FrameMemoryInfo,
) -> DecoratedOperandAccessKind {
    DecoratedOperandAccessKind::ReadRegister(
        to_register(reg, fallback_expected_type),
        None,
        FrameMemoryAttribute {
            is_temporary: false,
        },
    )
}

fn to_branch_offset(delta: i16) -> DecoratedOperandAccessKind {
    DecoratedOperandAccessKind::DeltaPc(ProgramCounterDelta(delta))
}

fn to_absolute_branch_pc(ip: u32) -> DecoratedOperandAccessKind {
    DecoratedOperandAccessKind::AbsolutePc(InstructionPosition(ip))
}

fn to_register(val: u8, ty: &BasicType) -> TypedRegister {
    let frame_placed = FramePlacedType::new(FrameMemoryAddress(0), ty.clone());
    TypedRegister::new_frame_placed(val, frame_placed)
}
