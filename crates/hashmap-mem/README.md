# 🪣 hashmap-mem

A memory-efficient hashmap implementation designed for Virtual
Machine implementations (specifically the Swamp VM) and similar low-level use cases.

## What is this?

`hashmap-mem` is a specialized hashmap implementation designed primarily for use
in Virtual Machine implementations, particularly the
[Swamp VM](https://github.com/swamp/swamp) environment, following the Swamp VM
collection standard.

This is **not** a general-purpose hashmap with all the safety guarantees you'd
expect from a standard Rust collection. Instead, it's a memory-optimized implementation.
For a more "normal" hashmap implementation with the rust security guarantees, please checkout my seq-map crate:

👉 📦 [seq-map](https://crates.io/crates/seq-map) ([github](https://github.com/piot/seq-map))

## Features

- **Fast lookups**: Uses [`FxHasher64`](https://crates.io/crates/fxhash) for efficient hashing
- **Tombstone-based deletion**: Quick removal of entries without costly
  rehashing
- Can not, by design, be resized

## Safety

This crate uses `unsafe` code extensively and expects you to manage memory
correctly.

- Memory must be properly allocated and aligned and deallocated after use.
- The map must be properly initialized before use
- Key and value pointers must be valid

## Memory Layout

The hashmap consists of a header followed by buckets:

```text
+-------------+
| MapHeader   |
+-------------+
| Bucket 0    |
| Bucket 1    |
| ...         |
| Bucket N-1  |
+-------------+
```

Each bucket has:

- A status byte (Empty, Tombstone, or Occupied)
- Key data (properly aligned)
- Value data (properly aligned)

## API Overview

- `layout`: Calculate memory layout for the map
- `init`: Initialize a new map in pre-allocated memory
- `get_or_reserve_entry`: Find or create an entry for a key
- `lookup`: Find an existing entry
- `has`: Check if a key exists
- `remove`: Remove an entry
- `overwrite`: Copy all entries from one map to another
- `find_next_valid_entry`: Iterator-like functionality

## License

This is my personal open source project. While you can use the code under the [LICENSE](LICENSE), I'm not accepting pull requests at this time.

Feel free to:

- Use and fork the code
- Report issues
- Share feedback
- Spread the word

If you have ideas or find bugs, open an issue and I'll check it out. Though I can't merge external code right now, your input is always appreciated!

Thanks for understanding! 🙏

---

_Copyright (c) 2025 Peter Bjorklund. All rights reserved_
