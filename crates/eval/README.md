# Swamp Evaluator (Interpreter)

A simple tree-walking interpreter for the [Swamp programming language](https://github.com/swamp/swamp), designed to execute the Abstract Semantic Graph (ASG) produced by `swamp-semantic`.

## Overview

This crate provides a straightforward way to run Swamp code by directly interpreting its semantic representation *after* parsing and semantic analysis have been completed. It walks the `swamp_semantic::Expression` tree and evaluates each node according to Swamp's operational semantics.

**Note:** This is a basic interpreter intended primarily for debugging. It is **not** optimized for performance. **The long-term plan is for execution to be handled by a more performant bytecode Virtual Machine (VM), at which point this interpreter may be deprecated or removed.**


## Key Features

*   **Executes ASG:** Directly interprets the output of `swamp-analyzer`.
*   **Tree-Walking:** Evaluates expressions recursively by traversing the semantic tree.
*   **Debugging/Testing:** Useful for verifying the correctness of the semantic analysis phase and understanding execution flow without needing the full VM.
*   **Simplicity:** Aims for correctness over speed.

## Installation

```toml
[dependencies]
swamp-eval = "0.1.16"
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
