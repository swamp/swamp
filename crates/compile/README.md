# Swamp Compile

The main orchestrator for the [Swamp programming language](https://github.com/swamp/swamp) compilation pipeline.

## Overview

This crate ties together various stages of the Swamp compiler to transform source code into a fully analyzed program representation (`swamp_semantic::Program`). It manages the overall process, including setting up the environment, resolving dependencies, and invoking the semantic analyzer.

## Key Responsibilities

*   **Bootstrapping:** Initializes the core library (`swamp-core`) and the default symbol table.
*   **Source Map Setup:** Creates and manages the `SourceMap` for tracking file locations.
*   **Dependency Resolution & Parsing:** Uses `swamp-dep-loader` to parse the main module and its dependencies, determining the correct analysis order.
*   **Analysis Orchestration:** Uses `swamp-program-analyzer` to drive the semantic analysis (`swamp-analyzer`) of all modules in the correct order.
*   **Error Reporting:** Integrates with `swamp-error-report` to display compilation errors with source context.
*   **High-Level API:** Provides functions like `bootstrap_and_compile` to run the entire pipeline.

## Installation

This crate is typically used as the main entry point for compiling Swamp code, either as a library or within a command-line tool.

```toml
[dependencies]
swamp-compile = "0.2.16"
```
