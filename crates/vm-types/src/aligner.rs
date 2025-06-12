/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub const SAFE_ALIGNMENT: usize = 8;

#[must_use]
pub fn align(addr: usize, alignment: usize) -> usize {
    debug_assert!(
        alignment.is_power_of_two(),
        "alignment must be a power of two"
    );
    (addr + alignment - 1) & !(alignment - 1)
}
