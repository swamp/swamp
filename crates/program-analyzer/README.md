# Swamp Program Analyzer

Coordinates the semantic analysis phase for a complete [Swamp programming language](https://github.com/swamp/swamp) program by analyzing its modules in the correct order.

## Overview

This crate acts as the driver for semantic analysis across an entire Swamp program, defined by a main module and its dependencies. It takes the parsed Abstract Syntax Trees (ASTs) for modules (provided by `swamp-dep-loader`) and the calculated dependency order, then invokes the core `swamp-analyzer` for each module sequentially.

Its primary responsibility is to ensure that modules are analyzed in an order that respects their dependencies, making the symbols and types exported by one module available when analyzing subsequent modules that depend on it. It builds up the complete analyzed representation of the program.

## Key Functions

*   **Input:** Takes parsed modules (`DependencyParser`), the analysis order (`Vec<Vec<String>>`), a `SourceMap`, the core library symbols, and the overall `ProgramState`.
*   **Orchestration:** Iterates through modules in the correct dependency order.
*   **Invokes Analyzer:** Calls `swamp_analyzer::Analyzer` for each module, providing the necessary context (including symbols from already-analyzed dependencies).
*   **Collects Results:** Aggregates the analyzed modules (containing symbol tables and potentially main expressions) into a `swamp_modules::Modules` collection within the `Program` state.
*   **Error Handling:** Propagates errors encountered during the analysis of any module.

## Role in Compilation

This crate sits between dependency loading/parsing and later compilation stages like code generation:

1.  **Parsing & Dependency Loading (`swamp-dep-loader`):** Source Code -> Parsed Modules + Dependency Order
2.  **Program Analysis (`swamp-program-analyzer`):** Parsed Modules + Order -> Analyzed Program (`swamp_modules::Modules` + ASG in `swamp-semantic`)
3.  **Code Generation (`swamp-code-gen`):** Analyzed Program / ASG -> Bytecode (`swamp-vm-types`)

## Installation

This crate is a core part of the Swamp compiler and is not typically used standalone.

```toml
[dependencies]
swamp-program-analyzer = "0.2.16"
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
