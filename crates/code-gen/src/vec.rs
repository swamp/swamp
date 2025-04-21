/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_vm_types::MemoryOffset;

// Constants for vector struct field offsets
pub const VECTOR_DATA_PTR_OFFSET: u16 = 0; // Offset to the data pointer
pub const VECTOR_LENGTH_OFFSET: u16 = 2; // Offset to the length field

pub const MAP_LENGTH_OFFSET: MemoryOffset = MemoryOffset(4);
