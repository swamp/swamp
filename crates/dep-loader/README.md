# Swamp Dependency Loader

Parses Swamp source files, identifies module dependencies (`mod` and `use` statements), and calculates the correct order for semantic analysis.

## Overview

This crate orchestrates the initial parsing phase of the Swamp compilation process. Starting from an entry point module, it recursively:

1.  Parses Swamp source files into ASTs (`swamp-ast`) using `swamp-parser`.
2.  Scans the AST for `mod` (import) and `use` declarations to discover dependencies on other modules.
3.  Maps module paths (e.g., `crate.My.Module`, `some_package.Lib`) to file system paths using `swamp-source-map`.
4.  Builds a dependency graph of the modules.
5.  Determines the correct topological order in which modules must be analyzed to satisfy dependencies.

## Key Functionality

*   **`DependencyParser`:** Manages the state of parsed modules and scanned dependencies.
*   **`parse_local_modules`:** Recursively parses the entry module and its dependencies.
*   **`get_analysis_order`:** Calculates the topological sort of the module dependency graph.
*   **Path Resolution:** Handles finding module files based on conventions (`crate` vs. registry packages, `.swamp` extension).

## Installation

This crate is primarily used internally by the Swamp compiler.

```toml
[dependencies]
swamp-dep-loader = "0.2.10"
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
