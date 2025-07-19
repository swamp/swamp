# Swamp VM Types

Defines the fundamental low-level data types, memory representations, and layout constants required by the Swamp Virtual Machine (VM).

## Overview

This crate provides the building blocks for representing memory addresses, data sizes, offsets, alignment, instruction formats, and the layout of core data structures within the VM's memory space. It is **not** intended for direct use by Swamp language programmers, but rather by developers working on the Swamp VM implementation itself.

These types are crucial for the correct functioning of the VM, ensuring proper memory access, data alignment, instruction decoding, and interaction between different VM components.

## Key Components

* **Memory Addresses:** Structs like `FrameMemoryAddress`, `ConstantMemoryAddress`, `HeapMemoryAddress`, `StackMemoryAddress` represent pointers or locations within different VM memory regions.
* **Memory Sizes & Offsets:** `MemorySize`, `FrameMemorySize`, `MemoryOffset` define the size of data types and relative positions in memory.
* **Memory Alignment:** The `MemoryAlignment` enum and associated functions (`align`, `align_frame_addr`, etc.) handle memory alignment requirements for different data types.
* **Instruction Format:** `BinaryInstruction` defines the packed binary layout of a single VM instruction, using opcodes defined in the `opcode` module.
* **Layout Constants:** Defines constants like `INT_SIZE`, `FLOAT_SIZE`, `PTR_SIZE`, `HEAP_PTR_SIZE`, `VEC_HEADER_SIZE`, `STRING_HEADER_SIZE`, etc., specifying the byte size of primitive types and internal data structure headers.
* **Internal Struct Layouts:** Defines `#[repr(C)]` structs like `VecHeader` and `StringHeader` to match the expected memory layout within the VM.

## Installation

This crate is primarily intended as a dependency for the Swamp VM implementation itself.

```toml
[dependencies]
swamp-vm-types = "0.2.14"
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
