/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub use crate::{
    HeapMemoryAddress, HeapMemoryRegion, InstructionPosition, InstructionPositionOffset,
    InstructionRange, MemoryAlignment, MemoryOffset, MemorySize, StackMemoryAddress,
    aligner::SAFE_ALIGNMENT,
    aligner::align,
    types::{BasicType, BasicTypeId, BasicTypeKind, BasicTypeRef},
};
