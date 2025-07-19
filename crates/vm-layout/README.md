# 🧩 Swamp VM Layout

## Overview

This crate is responsible for mapping high-level Swamp language types to their
low-level memory representations in the Swamp Virtual Machine. It serves as a
crucial bridge between the abstract type system defined in `swamp-types` and the
concrete memory layouts needed by the VM at runtime.

## Purpose

The `swamp-vm-layout` crate:

- Computes memory layouts for all Swamp types, determining their size,
  alignment, and field offsets

- Handles type lowering by mapping high-level types to their concrete memory
  representations

- Manages memory layout deduplication for structurally identical types

- Ensures proper alignment and memory padding according to C ABI
  requirements (special handling for zero size structs)

## Key Components

### `LayoutCache`

The core structure that manages type layout information with two primary caches:

- `id_to_layout`: Maps `TypeId` to `BasicTypeRef`, containing one entry per
  unique TypeId

- `kind_to_layout`: Maps `TypeKind` to `BasicTypeRef`, enabling sharing layouts
  between structurally identical types

### Layout Functions

The crate provides specialized layout functions for different type categories:

- Primitive types (integers, floats, booleans, strings)
- Composite types (structs, tuples)
- Sum types (enums, optional types)
- Collection types (vectors, maps, grids)
- Collection views

## Usage

This crate is primarily intended for use within the Swamp compiler toolchain and
VM implementation, usually not for direct use by Swamp language users.

```toml
[dependencies]
swamp-vm-layout = "0.2.14"
```

## License

This project is licensed under the MIT License - see the LICENSE file for
details.

## Contribution

We do not accept contributions. You are however welcome to:

- Utilize and fork the codebase according to the MIT license terms
- Identify and report any issues through the issue tracker
- Offer constructive feedback and suggestions
- Help others discover the project

Should you encounter bugs or have enhancement ideas, please don't hesitate to
open an issue. While I'm unable to accept contributions, your insights are
valuable to the project's improvement.

I appreciate your understanding and interest in this work. 🙏

## Copyright

_Copyright (c) 2025 Peter Bjorklund. All rights reserved._
