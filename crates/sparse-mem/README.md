# 🪣 sparse-mem

Welcome to `sparse-mem`, a low-level memory layout implementation for fixed-capacity, generation-tracked sparse arrays designed specifically for the [Swamp VM](https://github.com/swamp/swamp).

## What is it?

This crate provides the raw memory layout and operations for a sparse array structure that:

- Works with fixed capacity (decided upfront)
- Tracks generations to prevent dangling references
- Follows the Swamp VM Collection standard
- Operates directly on raw memory pointers

## Not a typical Rust collection

Important note: This is **not** a standard Rust collection! It's designed to work with raw memory layouts designed for VM internals. If you're looking for a more typical Rust sparse slot map implementation with all the Rust safety guarantees, check out my other crate:

👉 [sparse-slot](https://crates.io/crates/sparse-slot) ([github](https://github.com/piot/sparse-slot))

## Usage

This is a low-level crate meant for VM implementers. You'll be working with raw pointers and unsafe code (living on the edge!):

```rust
use std::ptr;

// Allocate memory (you need to handle this part)
let capacity = 10;
let element_size = 4;
let total_size = sparse_mem::layout_size(capacity, element_size);
let memory = vec![0u8; total_size];

unsafe {
    // Initialize the sparse array
    sparse_mem::init(memory.as_mut_ptr(), capacity, element_size);

    // Allocate a slot
    if let Some((id, generation)) = sparse_mem::allocate(memory.as_mut_ptr()) {
        // Use the id and generation to insert data
        let my_data: [u8; 4] = [1, 2, 3, 4];
        sparse_mem::insert(memory.as_mut_ptr(), id, my_data.as_ptr());

        // Later, check if still valid and remove
        if sparse_mem::is_alive(memory.as_mut_ptr(), id, generation) {
            sparse_mem::remove(memory.as_mut_ptr(), id, generation);
        }
    }
}
```

## Safety

Since this crate deals with direct raw memory manipulation, almost all functions are `unsafe`. Use with caution! ⚠️

## Why use this?

It probably only makes sense if you are:
- Implementing a VM with specific memory layout requirements
- Need precise control over memory allocation and layout
- Want a sparse array that follows the Swamp VM Collection standard.

## About Contributions

This is an open source project with a single copyright holder.
Even though the code is publicly available under [LICENSE](LICENSE), I am not accepting external contributions at this time.

You are welcome to:

- Use the code according to the license terms
- Fork the project for your own use, following the license terms
- Report issues
- Provide feedback
- Share the project

If you have suggestions or find bugs, please feel free to open an issue for discussion. While I cannot accept pull requests, I value your feedback and engagement with the project.

Thank you for your understanding and interest in the project! 🙏

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

_Copyright (c) 2024 Peter Bjorklund. All rights reserved_