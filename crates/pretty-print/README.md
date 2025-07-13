# Swamp Pretty Printer

Provides functions to format and display internal data structures from the [Swamp programming language](https://github.com/swamp/swamp) compiler in a human-readable, colorized way.

## Overview

This crate is a debugging and inspection tool for developers working on the Swamp compiler. It takes complex internal representations like the Abstract Semantic Graph (`swamp_semantic::Expression`), type structures (`swamp_types::Type`), symbol tables (`swamp_modules::SymbolTable`), etc., and formats them into indented, color-coded strings.

It utilizes `swamp-source-map-lookup` to include source text snippets and `yansi` for terminal colorization.

## Key Features

* **Formats Internal Structures:** Pretty-prints `Expression`, `Type`, `SymbolTable`, `Modules`, `AssociatedImpls`, and more.
* **Color Highlighting:** Uses terminal colors to differentiate between different kinds of symbols, types, and syntax elements.
* **Indentation:** Uses indentation to reflect the structure of the data (e.g., nested expressions, struct fields).
* **Source Integration:** Leverages `SourceMapLookup` to display identifiers based on their source text.

## Core Structures

* **`SourceMapDisplay`:** A wrapper around a `SourceMapLookup` implementation that provides the core formatting methods.
* **`SymbolTableDisplay`**, **`ExpressionDisplay`**, etc.: Wrapper structs that implement `std::fmt::Display` by using `SourceMapDisplay` to format the contained compiler structure.

## Installation

This crate is primarily intended for use within the Swamp compiler or related debugging tools.

```toml
[dependencies]
swamp-pretty-print = "0.2.10"
```
