# Swamp Types

Core data structures for the [Swamp](https://swamp-lang.org) compiler and tooling, defining the
language’s type system.

## Supported Types

### Primitives

* `Byte`
* `Int`
* `Float`
* `Bool`
* `String`
* `Unit` (empty)
* `Never`

### Strings & Storage

* `StringStorage(TypeRef, capacity)`

### Aggregates & Containers

* `Tuple(Vec<TypeRef>)`
* `Range(TypeRef)`
* `FixedCapacityAndLengthArray(TypeRef, len)`
* `SliceView(TypeRef)`

### User-Defined

* `NamedStruct(NamedStructType)`
* `AnonymousStruct(AnonymousStructType)`
* `Enum(EnumType)`

### Functions & Signatures

* `Function(Signature)`
* `Optional(TypeRef)`

### Collections & Views

* `VecStorage(TypeRef, capacity)` / `DynamicLengthVecView(TypeRef)`
* `StackStorage(TypeRef, cap)` / `StackView(TypeRef)`
* `QueueStorage(TypeRef, cap)` / `QueueView(TypeRef)`
* `MapStorage(Rc<Key>, Rc<Value>, cap)` / `DynamicLengthMapView(Rc<Key>, Rc<Value>)`
* `SparseStorage(TypeRef, cap)` / `SparseView(TypeRef)`
* `GridStorage(TypeRef, rows, cols)` / `GridView(TypeRef)`

## License

MIT — see [LICENSE](LICENSE) for details.

Copyright Peter Bjorklund. All rights
reserved. [https://github.com/swamp/swamp](https://github.com/swamp/swamp)
