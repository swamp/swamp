/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use swamp_vm_isa::InstructionPosition;
use swamp_vm_types::types::FunctionInfo;
use swamp_vm_types::Meta;

pub struct FileOffsetEntry {
    pub start_pc: u32, // This u32 is the instruction index, so this should be plenty
    pub pc_count: u8,  // A line can't cover more than 255 instructions
    pub file_id: u16,  // Maximum 65535 files in a program
    pub line_row: u16, // Can not have more than 65536 lines in a single source file
}

pub struct FileOffsetTable {
    pub entries: Vec<FileOffsetEntry>,
}

impl Default for FileOffsetTable {
    fn default() -> Self {
        Self::new()
    }
}

impl FileOffsetTable {
    #[must_use]
    pub const fn new() -> Self {
        Self { entries: vec![] }
    }
    #[must_use]
    pub fn find(&self, ip: InstructionPosition) -> Option<&FileOffsetEntry> {
        let pc = ip.0;
        self.entries
            .iter()
            .find(|&entry| pc >= entry.start_pc && pc <= entry.start_pc + u32::from(entry.pc_count))
    }
}

pub struct FunctionDebugInfo {
    pub start_pc: u32,
    pub function_id: u16,
}

pub struct FunctionTable {
    pub entries: Vec<FunctionDebugInfo>,
}

impl Default for FunctionTable {
    fn default() -> Self {
        Self::new()
    }
}

impl FunctionTable {
    #[must_use]
    pub const fn new() -> Self {
        Self { entries: vec![] }
    }
    pub(crate) fn find(&self, ip: InstructionPosition) -> Option<&FunctionDebugInfo> {
        let pc = ip.0;
        let mut last_info = None;
        for entry in &self.entries {
            if entry.start_pc > pc {
                return last_info;
            }
            last_info = Some(entry);
        }
        last_info
    }
}

pub struct DebugInfo {
    pub function_lookup: SeqMap<u32, u8>,
    pub file_offsets: FileOffsetTable,
    pub function_table: FunctionTable,
    pub info_for_each_instruction: Vec<Meta>,
    pub function_infos: SeqMap<u16, FunctionInfo>,
}

pub struct DebugInfoForPc {
    pub meta: Meta,
    pub function_debug_info: FunctionInfo,
}

impl Default for DebugInfo {
    fn default() -> Self {
        Self::new()
    }
}

impl DebugInfo {
    #[must_use]
    pub fn new() -> Self {
        Self {
            function_lookup: SeqMap::default(),
            file_offsets: FileOffsetTable::new(),
            function_table: FunctionTable::new(),
            info_for_each_instruction: vec![],
            function_infos: SeqMap::default(),
        }
    }

    #[must_use]
    pub fn fetch(&self, pc: usize) -> Option<DebugInfoForPc> {
        let ip = InstructionPosition(pc as u32);
        let function = self.function_table.find(ip)?;

        let meta = &self.info_for_each_instruction[ip.0 as usize];

        let func_info = self.function_infos.get(&function.function_id)?;

        let info = DebugInfoForPc {
            meta: Meta {
                comment: meta.comment.clone(),
                node: meta.node.clone(),
            },
            function_debug_info: func_info.clone(),
        };

        Some(info)
    }
}
