/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::alloc::ScopeAllocator;
use swamp_types::Type;
use swamp_vm_types::types::FramePlacedType;

pub fn reserve_space_for_type(ty: &Type, allocator: &mut ScopeAllocator) -> FramePlacedType {
    allocator.reserve(ty)
}
