# Swamp Code Generator

Translates the type-checked, resolved Abstract Semantic Graph (ASG) from `swamp-semantic` into
executable bytecode (`BinaryInstruction` from `swamp-vm-types`) for the Swamp Virtual Machine (VM).

## Overview

This crate represents the code generation phase of the Swamp compiler pipeline. It takes the
high-level, semantically rich representation of the program (where types are known, names are
resolved, etc.) and lowers it into a sequence of simple, low-level instructions that the Swamp VM
can directly execute.

## Key Responsibilities

* **Instruction Selection:** Chooses the appropriate VM opcodes (`swamp_vm_types::opcode::OpCode`)
  for each operation in the semantic tree (e.g., selecting `AddI32` for an integer addition).
* **Memory Management (Frame):** Assigns stack frame memory addresses (
  `swamp_vm_types::FrameMemoryAddress`) for local variables, function arguments, and temporary
  intermediate values.
* **Control Flow Generation:** Translates high-level control flow structures (like `if`, `match`,
  loops) into sequences of conditional and unconditional jump instructions (`Bz`, `Bnz`, `Jmp`).
* **Function Call Handling:** Generates the necessary instructions for function calls (`Call`,
  `Enter`, `Ret`), including setting up arguments and handling return values according to the VM's
  calling convention.
* **Constant Pool Interaction:** Places literal values (integers, strings, etc.) into a constant
  pool (managed externally or internally) and generates instructions (`LdConst`,
  `StringFromConstantSlice`) to load them into registers or memory at runtime.
* **Lowering Complex Operations:** Breaks down complex semantic operations (e.g., struct
  instantiation, list manipulation) into sequences of simpler VM instructions.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
