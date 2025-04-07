# Swamp Types

Core data structures for representing the type system of the [Swamp programming language](https://swamp-lang.org).

This crate defines the `Type` enum and associated structs used throughout the Swamp compiler and tooling to represent and manipulate types.

## Overview

*   **`Type` Enum:** The central enum representing all possible types in Swamp, including:
    *   Primitives (`Int`, `Float`, `String`, `Bool`, `Unit`, `Never`)
    *   Containers (`Tuple`, `Slice`, `SlicePair`)
    *   User-Defined Types (`NamedStruct`, `AnonymousStruct`, `Enum`)
    *   Functions (`Function` with `Signature`)
    *   Modifiers (`Optional`, `MutableReference`)
    *   Generics (`Generic`, `Blueprint`, `Variable`)
    *   External/FFI types (`External`)
*   **Supporting Structs:** Detailed definitions for complex types like `NamedStructType`, `EnumType`, `Signature`, `ParameterizedTypeBlueprint`, etc.
*   **Type Compatibility:** Includes logic (`compatible_with`, `assignable_type`) for checking type relationships.
*   **Source Information:** Integrates with `swamp-node` to link types back to source code locations.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
swamp-types = "0.1.16"
```

# License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

# Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
