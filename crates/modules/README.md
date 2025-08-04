# Swamp Modules

Data structures for representing modules and their contents in the [Swamp programming language](https://swamp-lang.org).

This crate defines how modules are structured and how symbols (types, functions, constants, etc.) are organized and accessed within them.

## Overview

*   **`Module` / `ModuleRef`:** Represents a compiled Swamp module.
*   **`SymbolTable`:** A key structure holding all the named definitions (symbols) exported by or defined within a module. It allows looking up types, functions, constants, aliases, type blueprints, and linked modules by name.
*   **`Symbol` Enum:** Defines the different kinds of entities that can be stored in a `SymbolTable`, such as `Type`, `FunctionDefinition`, `Constant`, `Alias`, `Blueprint`, `Module`, etc.
*   **Integration:** Relies heavily on types defined in `swamp-types` and `swamp-semantic` to represent the actual definitions stored in the symbol table.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
swamp-modules = "0.2.22"
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
