/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::constants::ConstantsManager;
use crate::{ConstantInfo, FunctionIps, GenFunctionInfo};
use seq_map::SeqMap;
use source_map_node::Node;
use swamp_semantic::{ConstantId, ConstantRef, InternalFunctionDefinitionRef, InternalFunctionId};
use swamp_vm_debug_info::DebugInfo;
use swamp_vm_isa::InstructionPosition;
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::types::{CompleteFunctionInfo, FunctionInfo, FunctionInfoKind, HeapPlacedType};
use swamp_vm_types::{InstructionPositionOffset, PatchPosition};
use tracing::info;

pub struct GenOptions {
    pub is_halt_function: bool,
}

pub struct FunctionFixup {
    pub patch_position: PatchPosition,
    pub fn_id: InternalFunctionId,
    pub internal_function_definition: InternalFunctionDefinitionRef,
}

pub struct CodeGenState {
    pub(crate) constants_manager: ConstantsManager,
    pub(crate) constant_offsets: SeqMap<ConstantId, HeapPlacedType>,
    pub(crate) constant_functions_in_order: SeqMap<ConstantId, ConstantInfo>,
    pub function_infos: SeqMap<InternalFunctionId, GenFunctionInfo>,
    pub function_ips: FunctionIps,
    pub function_fixups: Vec<FunctionFixup>,
    pub functions: SeqMap<usize, CompleteFunctionInfo>,
    pub constants: SeqMap<usize, CompleteFunctionInfo>,
    pub layout_cache: LayoutCache,

    pub debug_info: DebugInfo,
}

impl CodeGenState {}

impl Default for CodeGenState {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            constants_manager: ConstantsManager::new(32 * 1024 * 1024),
            constant_offsets: SeqMap::default(),
            function_infos: SeqMap::default(),
            constant_functions_in_order: SeqMap::default(),
            function_ips: FunctionIps::default(),
            function_fixups: Vec::new(),
            functions: SeqMap::default(),
            constants: SeqMap::default(),
            debug_info: DebugInfo::default(),
            layout_cache: LayoutCache::new(),
        }
    }

    pub fn add_function(
        &mut self,
        function_info: FunctionInfo,
        node: &Node,
        comment: &str,
    ) -> CompleteFunctionInfo {
        self.add_function_helper(function_info, node, comment)
    }

    /// # Panics
    ///
    pub fn add_function_helper(
        &mut self,
        function_info: FunctionInfo,
        node: &Node,
        comment: &str,
    ) -> CompleteFunctionInfo {
        let complete_info = CompleteFunctionInfo {
            ip: function_info.ip_range.start,
            size: InstructionPositionOffset(0),
            info: function_info.clone(),
        };
        if let FunctionInfoKind::Constant(constant_id) = function_info.kind {
            self.constants
                .insert(constant_id, complete_info.clone())
                .unwrap();
        } else if let FunctionInfoKind::Normal(normal_id) = function_info.kind {
            if self.functions.contains_key(&normal_id) {
                // TODO: improve monomorph
                info!("skipping");
            } else {
                self.functions
                    .insert(normal_id, complete_info.clone())
                    .unwrap();
            }
        }

        complete_info
    }

    /// # Panics
    /// If function info is not a constant
    pub fn add_constant_function(
        &mut self,
        constant_info: FunctionInfo,
        node: &Node,
        comment: &str,
    ) -> CompleteFunctionInfo {
        let FunctionInfoKind::Constant(_id) = constant_info.kind else {
            panic!("must be constant")
        };

        self.add_function_helper(constant_info, node, comment)
    }

    #[must_use]
    pub const fn constant_functions(&self) -> &SeqMap<ConstantId, ConstantInfo> {
        &self.constant_functions_in_order
    }

    #[must_use]
    pub fn create_function_sections(&self) -> SeqMap<InstructionPosition, String> {
        let mut lookups = SeqMap::new();
        for (_func_id, function_info) in &self.function_infos {
            let description = function_info
                .internal_function_definition
                .assigned_name
                .clone();
            lookups
                .insert(function_info.ip_range.start, description)
                .unwrap();
        }

        for (_func_id, function_info) in &self.constant_functions_in_order {
            let description = format!("constant {}", function_info.constant_ref.assigned_name);
            lookups
                .insert(function_info.ip_range.start, description)
                .unwrap();
        }

        lookups
    }

    pub fn reserve_space_for_constants(&mut self, constants: &[ConstantRef]) {
        for constant in constants {
            let heap_placed_type = self
                .constants_manager
                .allocator
                .allocate(&mut self.layout_cache, &constant.resolved_type);

            self.constant_offsets
                .insert(constant.id, heap_placed_type)
                .unwrap();
        }
    }
}
