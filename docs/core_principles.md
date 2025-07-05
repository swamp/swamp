# Swamp Core Principles

## 0. What got us here?

I dislike and want to avoid the following:

- **deep clones**. very performance heavy. And related to that I want to avoid
  having the managing of collections like stacks and lists spread out on to very
  small "entities".

- **heap allocation** during runtime. It can be done in the engine for certain
  containers, like Strings, SlotMap or similar concepts.

- **garbage collection**. Even though it is allocated in an arena and the
  garbage collection is just one root, it still takes time to do a deep clone of
  the root object.

For optimal performance and to avoid garbage collection for core game data, the
fundamental rule is: Persistent Game State must always be stored in designated
containers as blittable types. This ensures that the authoritative state of the
game is self-contained, efficiently managed, and ready for each new tick without
any essential/useful data residing on the general heap.

## 1. The World Model

- **Single Implicit World**: All Swamp code operates within one implicit `World`
  context, simplifying the programmer's mental model.

- **World Management**: The host application creates and manages `World`
  instances, allowing multiple isolated worlds (gameplay, UI, render) when
  needed.

- **Central Data Repository**: The World exclusively holds data containers,
  centralizing data ownership and lifetime management.

### Principle 1.1: One World to Rule Them All

All Swamp language code operates within the conceptual context of a single,
implicit `World`. This `World` instance serves as the sole owner and manager of
all top-level data **containers**.

- **Rationale:** This principle simplifies the language model for the Swamp
  programmer. There is no need to manage `World` pointers or specify which
  **container** belongs to which `World` from within Swamp code; it's always the
  current, implicit one.

### Principle 1.2: World Instance Management by Host

While Swamp code perceives a single `World`, the overarching host application
("game engine", that is external to Swamp) is responsible for creating,
managing, and destroying actual `World` instances. A host can manage multiple,
isolated `World` instances (e.g., a `gameplay_world`, `ui_world`,
`render_world`).

- **Rationale:** This allows for modularity and separation of concerns at the
  application level (e.g., different game subsystems, testing environments). The
  Swamp language itself remains focused on in-`World` logic, while the engine
  handles broader architectural concerns. The lifetime of a `World` instance,
  and thus all its contents, is controlled by the engine.

### Principle 1.3: World as the Universal Data Repository

The World is the only place that holds containers. All persistent, collectively
managed data elements reside within these **containers**.

- **Rationale:** This centralizes data ownership and lifetime management within
  a given `World` instance, contributing to predictability and simplifying the
  tracking of memory resources.

## 2. Data Containers: Core Storage Units

This section describes Swamp's primary mechanism for storing collections of data
elements. These are the fundamental building blocks for organizing game data.

- **Explicit Declaration**: Containers must be declared with a unique identifier
  (which becomes its handle type), fixed maximum capacity, and element
  structure.

- **Fixed Capacity**: Container capacity is determined at declaration and
  remains constant during the World's lifetime, enabling upfront memory
  allocation.

- **Static Lifetime**: Containers exist for the entire lifetime of their parent
  World instance, eliminating concerns about container availability.

### Principle 2.1: Explicit Declaration and Nature of Data Containers

All primary data storage units, **containers**, must be explicitly declared
within the Swamp language. Such a declaration must define:

1. A unique identifier for the **container**. This identifier also serves as the
   basis for a distinct **handle type** used to reference elements within this
   specific **container**.

2. The fixed maximum capacity (number of elements) of the **container**.

3. The structural definition of the elements that will be stored within it.

- **Rationale:** This explicitness ensures that the properties, capacity, and
  element structure of all major data stores are known at compile-time. This
  contributes to predictability, enables static analysis, and forms the basis of
  Swamp's type-safe handle system.

### Principle 2.2: Fixed Capacity

All data **containers** have a fixed capacity (maximum number of elements)
determined at their declaration. This capacity cannot change during the lifetime
of the `World` instance to which they belong.

- **Rationale:** Fixed capacity allows for all necessary memory for a `World`'s
  **containers** to be allocated upfront by the engine during `World`
  initialization. This design choice prevents runtime allocations for
  **container** growth, significantly reducing memory fragmentation, simplifying
  memory management, and leading to more predictable performance.

  Basically, it can't be smaller during runtime, since then it would invalidate
  entities have indexes that are out of bounds. And it doesn't really make sense
  to increase the size either. It is safe to increase the size, but it would
  create a performance hit when that happens. reallocation and copying all
  entities to the new container. It can be a possibility for special case, but
  is not part of the core principles.

### Principle 2.3: Static Lifetime (Relative to World Instance)

All data **containers** are initialized when their parent `World` instance is
created by the engine and persist for the entire lifetime of that `World`.

- **Rationale:** From the perspective of Swamp code operating within a given
  `World`, all its **containers** are effectively static and always present.
  Their memory is reclaimed only when the engine destroys the entire `World`
  instance. This ensures reliable availability of data storage during the
  `World`'s operation.

## 3. Container Elements

### Principle 3.1: Blittable Elements

Elements stored within a **container** must be **fully blittable**. A blittable
type has a memory representation that:

1. Contains no pointers or references, directly or indirectly, to externally
   managed memory (except for Swamp's own handle types, which are managed within
   the `World`).

2. Can be safely copied byte-for-byte (e.g., via `memcpy`) without invalidating
   its state or the state of other data.

3. Is composed of primitive or Composite Types types, other Swamp handle types
   (or maybe fixed-size arrays of blittable types).

- **Rationale:** This is huge for performance (fast copies, predictable layout),
  simple memory management (no hidden allocations per element), easier
  serialization, and it lets us do cool memory layout tricks (like Structure of
  Arrays - SoA)ent memory layout transformations (like Structure of Arrays -
  SoA).

### Principle 3.2: Element Composition and Handle Types

Elements are typically defined by `named struct`s. Fields within these `struct`s
can be:

1. Primitive types (e.g., integers, fixed-point numbers, booleans).

2. Reference handle types, which are identifiers derived from other
   **container** declarations (e.g., a `Goblin` element having a
   `target: Player` field, where `Player` is the handle type for a `Player`
   **container**).

3. Other blittable `struct`s (which are treated as value types, directly
   embedded within the element).

4. Small, fixed-size arrays of blittable types.

- **Rationale:** This defines the valid composition of data elements, ensuring
  all parts are compatible with Swamp's memory model. The use of reference
  handle types for references is key to maintaining decoupling and safety.

### Principle 3.3: Embedded Fixed-Size Arrays

An element's `struct` definition may include fixed-size arrays of blittable
types, where the array's size is known at compile time and is typically small.

- **Rationale:** This allows for embedding short sequences of data (like next
  steps in a movement path, things the avatar is holding, etc) directly within
  an element for convenience and locality, provided they adhere to blittable
  constraints. For larger or variably sized collections, use separate
  **containers** accessed via reference handles.

## 4. Reference Handles: Safe Indirection

### Principle 4.1: Nature and Derivation of Reference Handles

A reference handle is a type-safe, managed reference to an element residing
within a specific **container**. When a **container** is declared, its
identifier also establishes a distinct reference handle type.

- **Rationale:** Reference handles provide a necessary level of indirection, to
  have relationships between elements in different **containers**.

### Principle 4.2: Internal Reference Handle Operation (Conceptual)

To ensure safe and valid access, a reference handle internally encapsulates (or
allows the runtime to derive) information necessary for secure element location
and validation. This includes:

1. An **index** to the element's slot within that target `container`. Maybe an
   unsigned 16 bit number.

2. A **generation counter** associated with the slot, used to detect and
   invalidate stale handles that might refer to reused slots. Probably an u8
   generation counter.

- **Rationale:** This internal structure allows handles to be compact yet carry
  sufficient information for the runtime to perform its safety checks without
  the programmer having to add extra code for it.

### Principle 4.3: Implicit and Safe Reference Handle Dereferencing

Accessing fields of an element through a handle is an implicit operation,
automatically managed and safety-checked by the Swamp runtime. Programmers do
not manually "dereference" handles in an unsafe manner.

- **Behavior & Rationale:**

  1. The runtime uses the handle's static type to identify the target
     `container`.

  2. It uses the handle's internal `index` field to access the specific slot
     within that `container`.

  3. Crucially, it compares the `generation` value stored within the handle
     (from when it was created) with the current `generation` value of the slot
     in the `container`.

  4. **Matching Generations:** Access is granted (both for reads or writes). The
     reference handle is valid and points to the intended live element.

  5. **Mismatched Generations (Dangling Handle):** The handle is stale — it
     refers to a slot that has been reused or whose element has been
     invalidated. Access is denied for writes to prevent data corruption (or
     might be allowed for write using use a zeroed area. If many multiple writes
     occur, how to handle that? zeroed area may run out for writes.
     - The operation returns a default-initialized (e.g., zeroed) value for the
       field's type. This ensures program stability and predictability even with
       stale handles.
     - Debug builds may issue warnings.

**Rationale:** This mechanism is fundamental to Swamp's safety, virtually
eliminating use-after-free errors for elements within **containers**. It
fulfills the design goal that programmers rarely, if ever, need to perform
manual lookup or validation of handles; safety is built into the access
mechanism itself.

## 5. Operations on Data Containers

### Principle 5.1: Element Creation

The language provides a mechanism to safely create new elements within a
designated data **container**, which yields a valid pointer (or handle) to the
newly created element.

- **Behavior & Rationale:** This operation involves finding an available (empty
  or recyclable) slot within the specified **container**, zeroing the element's
  memory, updating the slot's generation count to a new value, and returning a
  handle/pointer that captures this new generation and index. If the
  **container** is at full capacity, the creation operation will return a zeroed
  out handle and pointer (or panic).

### Principle 5.2: Element Invalidation and Slot Recycling

The language provides a mechanism to invalidate an existing element within a
data **container**, making its slot available for reuse by subsequent creation
operations.

- **Behavior & Rationale:** This is primarily achieved by updating (e.g.,
  incrementing) the generation count of the element's slot. This act immediately
  invalidates all existing handles that referred to that specific instance (as
  their stored generation number will no longer match the slot's new generation
  number). This does not involve deallocating (but maybe zeroing) from the
  fixed-capacity **container**.

### Principle 5.3: Efficient Iteration

Data **containers** support an efficient iteration mechanism that yields valid
handles only to live, accessible elements.

- **Behavior & Rationale:** The iteration process is designed to automatically
  skip over empty or invalidated (despawned) slots. This ensures that loops and
  processing systems operate only on active data, simplifying logic and
  improving performance.

## 6. Core Safety Guarantees (Within a Swamp World Instance)

### Principle 6.1: Prevention of Dangling Element Handles (Use-After-Invalidation)

The generation count system ensures that attempts to use stale handles
(referring to invalidated or reused slots) do not lead to memory corruption or
access to unintended data.

### Principle 6.2: No Memory Leaks from Container Elements

All element memory is part of fixed-capacity data **containers**. the memory is
never "deallocated", just marked as invalidated (increasing generation counter)
(and maybe zeroed out?). The lifetime of the **container** memory itself is tied
to its `World` instance.

### Principle 6.3: Strong Handle Type Safety

Handles intended for one type of data **container** are statically distinct and
incompatible with those for other **container** types. This is enforced by the
compiler, preventing type confusion errors at runtime.

### Principle 6.4: Implicit Access Validation

All data access performed through handles includes implicit index bounds and
generation validation by the runtime, ensuring memory safety without requiring
manual checks by the programmer.

## 7. Memory Layout

### Principle 7.1: Structure of Arrays (SoA) Option

The language provides a mechanism for a data **container** to optionally specify
its memory layout as Structure of Arrays (SoA). If not specified, the default
layout is Array of Structures (AoS).

- **Rationale:** SoA layout can offer significant performance benefits for
  systems that iterate over and process only a few specific fields of many
  elements simultaneously, due to improved CPU cache locality. AoS groups all
  fields of a single element together. Providing this option allows developers
  to optimize memory access patterns based on specific needs.

---

## 8. Swamp Language Syntax: Realizing the Principles

This section illustrates how the core principles outlined above are expressed
through the Swamp language's syntax.

### 8.1 Defining Data Structures (`struct`)

Data structures are declared as the normal Swamp Syntax, but the fields of
course must be blittable or fields that are reference handles.

### 8.2 Declaring Data Containers and Their Handle Types

Data containers are declared using the container keyword. This declaration also
implicitly defines the unique handle type associated with that container, using
the container's name.

1. Container with Inline Element Structure Definition:

```rust

struct RootWorld {
  monsters: [Monster; 512],
  active_projectiles: [ProjectileData; 2048],
  time: Int,
}

// 'Monster' also becomes the distinct handle type for its elements.
// Capacity is 512 elements.
// The structure of each 'Monster' element is defined "inline":
struct Monster[512] {
  position: Vector2,         // { x: Int, y: Int}
  health: Int,
  target_entity: GameEntity,  // 'GameEntity' would be another handle type
}
```

1. Container Referencing an Existing Struct for Element Structure:

This case can be useful if there is a need for different containers but several
of them wants to use the same underlying type without having to alias that type
or duplicate the structure.

```rust
struct ProjectileData {
  owner: GameEntity,
  velocity: Vector2,
  damage: Int,
}

// Declares a container 'ActiveProjectile' using 'ProjectileData' for its
// element structure.
// 'ActiveProjectile' becomes the handle type for this container. Capacity 2048.

```

8.3 Using Handle Types

When a field in a struct or a variable is declared with a type that matches a
declared container name, it is treated as a handle.

MAYBE: Maybe have an optional prefix, that is not needed but if the programmer
needs clarity, like a `&` prefix to denote that it is a reference handle? Maybe
not needed?

```rust
struct Player {
  // 'Monster' is a handle type
  current_target: Monster,
  // 'ActiveProjectile' is a handle type
  active_projectile: ActiveProjectile
}

container GameEntity[1024] {
  // Embeds a Player struct, which contains
  // two handles in its fields
  player_data: Player,
  is_active: Bool,
}
```

---

## Initial idea

- `World` is the only thing that holds containers.
- All containers are fixed size.
- All containers are of static lifetime, the world always exists.
- All elements inside a container are fully blittable (every element can be
  fully copied with a memcpy).
- Elements can have fields that are "reference handles" into another container
  and index (container_id, index, and generation).
- An element can maybe have fixed size blittable arrays of very small sizes, but
  is better to be looked up in a container.
- A container can optionally be specified to be a SoA.
- You rarely, (never?), have to do explicitly lookup a "reference handle"
  manually. Code is emitted to do the lookup from the World:
- You can quickly iterate through the container (it keeps a index to next valid
  entry or similar)

## Questions

- Maybe we need containers that are hashmaps, so a value is hashed to an index
  and linear probing and similar. Or should those be solved a "normal"
  collections?

## Reference Handle

Four bytes:

- offset: u16
- generation: u8 (u4 should be enough)

Later in the code to get the main_spell, you just type as usual:

```typescript
mana: Int = entity.main_spell.mana

// for the `entity.main_spell` part, it emits code that is similar to:
// either inline the code to fetch the spell or call a generated function
// ptr_to_spell = &world.spells[entity.main_spell._hidden_index]
// if ptr_to_spell->generation != entity.main_spell._hidden_generation {
//   // it has been removed earlier, or replaced with a new spell
//   // return a pointer to a zeroed out memory area.
// } else {
//  return the ptr_to_spell
// }

// after that it offsets the ptr_to_spell to get the mana offset and return the Int

// there can also be ways to get these safely, with `?.` but should rarely be needed
mana: Int? = entity.main_spell?.mana
```

## Swamp Collection Types

Swamp's collection types are designed for performance and clarity, storing their
elements in **contiguous blocks of memory**. Crucially, the data *always
follows directly* after the type header fields (like `len` and `capacity`),
meaning **no separate pointers** are typically involved for accessing the
elements within an instance of these types.

**Regarding Memory Alignment (32-bit Architecture)**: The len and capacity
fields, both `u16` (2 bytes), collectively occupy 4 bytes. On a typical 32-bit
architecture, this 4-byte header naturally aligns the subsequent data elements,
meaning no padding bytes are required between the capacity field and the first
data item (Item 0).

**Mutability in Swamp is verified in the analyzer semantic step in the compiler,
the underlying memory layout is exactly the same, irrespective of mutability.**

---

### 1. Fixed-Capacity Array (Raw Storage): `[T; N]`

This is Swamp's most basic, fixed-size container. Think of it as a raw,
compile-time sized chunk of memory.

- **Syntax:** `[ElementType; Capacity]` (e.g., `[Int; 42]`)
- **Purpose:** Ideal for fixed-size buffers where the maximum number of elements
  is always known at compile time.
- **Opcode**: `array.subscript $R$m, #fixed_known_size`

#### Memory Layout

| Field      | Type     | Notes                                           |
| :--------- | :------- | :---------------------------------------------- |
| `capacity` | `u16`    | Maximum allocated capacity (will always be `N`) |
| `len`      | `u16`    | Current number of elements                      |
| `Item 0`   | `Type T` | First element                                   |
| `Item 1`   | `Type T` |                                                 |
| `...`      | `...`    |                                                 |
| `Item N-1` | `Type T` | Last element; `N` is the compile-time capacity  |

- **Key Behavior:**
  - **Capacity**: Fixed N, known at compile time. This N is stored in the
    capacity field.
  - **Length**: Internally stored as N in the len field. Cannot be altered by
    API calls; elements cannot be pushed or popped.
  - **Bounds Check:** Access is checked against `N` by the compiler, potentially
    at compile-time or by generating assembly that compares against the literal
    `N`.
  - **Design Rationale for Header Fields**: While len and capacity are known at
    compile time for [T; N], they are explicitly allocated space (4 bytes) in
    the array's memory layout. This deliberate trade-off provides
    out-of-the-box, zero-cost compatibility (no data copies, no pointer
    indirection) when converting an [T; N] to a Vec`<T>` or [T] slice. This is
    crucial for avoiding expensive memory operations, especially for large
    arrays,
  - Note on len/capacity usage: Although [T; N] structurally contains len and
    capacity fields, it's generally not recommended to use these fields for
    dynamic length management within [T; N] itself. Its len is effectively
    always N. However, due to the shared memory layout, it could technically be
    bounds-checked in the same way as a Vec (i.e., index < len), but this is
    usually redundant as len will simply be N.

---

### 2. Fixed-Capacity Vector (Owned Storage): `Vec<T; N>`

This type *owns* a fixed-size, compile-time allocated buffer and manages a
dynamic `length` within that buffer. The data is stored directly after its
fields.

- **Syntax:** `Vec<ElementType; Capacity>` (e.g., `Vec<Int; 42>`)
- **Purpose:** Use when you need a growable collection but know its absolute
  maximum size at compile time, allowing for stack allocation or static data.
  And if it is in a "Root type" in Swamp.

#### Fixed-Capacity Vector Memory Layout

| Field      | Type     | Notes                                           |
| :--------- | :------- | :---------------------------------------------- |
| `capacity` | `u16`    | Maximum allocated capacity (will always be `N`) |
| `len`      | `u16`    | Current number of elements                      |
| `Item 0`   | `Type T` | First element of the buffer                     |
| `Item 1`   | `Type T` |                                                 |
| `...`      | `...`    |                                                 |
| `Item N-1` | `Type T` | Last element of the buffer                      |

- **Key Behavior:**
  - **Capacity:** Fixed `N`, known at compile time. This `N` is stored in the
    `capacity` field.
  - **Length:** **Explicitly tracked** by a `len` field (`u16`).
  - **Bounds Check:** Accesses are checked against `len`; additions are checked
    against `capacity` (`N`).
  - `Vec<T; 10>` and `Vec<T; 20>` are **distinct types**.

---

### 3. Dynamic-Length Vector (View): `Vec<T>`

This is the non-capacity-specific type that represents a vector. **It has the
exact same in-memory structure as `Vec<T; N>`**, allowing it to be passed around
without the specific `N` being part of its type. Its `len` can be changed. It is
an **Unsized Type (DST)**, meaning its total size is not known at compile time.

- **Syntax:** `Vec<ElementType>` (e.g., `Vec<Int>`)
- **Opcode**: `vec.subscript $R$m, $R$n`. $R$m is the pointer to the vec header,
  $R$n is the Int index.
- **Purpose:** For functions that operate on vectors without requiring knowledge
  of their compile-time fixed capacity. It allows modifying the `len` and
  elements (if the binding is mutable).
- **Memory Allocation**: `Vec<T>` instances can be allocated either on the stack
  (if their size is known at compile time and reasonably small) or on the heap
  (for dynamically sized vectors). This choice doesn't affect the memory layout,
  only where the memory is obtained.

#### Dynamic-Length Vector Memory Layout

| Field      | Type     | Notes                                      |
| :--------- | :------- | :----------------------------------------- |
| `capacity` | `u16`    | Maximum allocated capacity (runtime value) |
| `len`      | `u16`    | Current number of elements                 |
| `Item 0`   | `Type T` | First element of the buffer,               |
| `Item 1`   | `Type T` |                                            |
| `...`      | `...`    |                                            |
| `Item M-1` | `Type T` | Last element of the buffer                 |

- **Key Behavior:**
  - **Capacity:** Stored **runtime `u16` field**. Cannot typically be changed
    (no re-allocation), as its underlying storage has a fixed size.
  - **Length:** Stored **runtime `u16` field**, can be modified (e.g., `push`,
    `pop`), if the `Vec<T>` binding is mutable.
  - **Bounds Check:** Accesses are checked against `len`; additions against
    `capacity`.
  - **Dynamic:** A `Vec<T; N>` can be passed to functions expecting `Vec<T>`,
    regardless of the original `N`.

---

### 4. Slice/View: `[T]`

This is the primary way to refer to a contiguous sequence of elements without
owning the data. **It is structurally identical to `Vec<T>`**, containing `len`
and `capacity` fields with the data directly following. It is an Unsized Type
(DST). It is structurally identical to `Vec<T>` (and `Vec< T;N`> / `[T;N]`),
containing len and capacity fields with the data directly following.

- **Syntax:** `[ElementType]` (e.g., `[Int]`)
- **Purpose:** For functions that need to read or modify elements within any
  contiguous memory block, without changing its length or ownership.
- **Universal Compatibility (Zero-Cost Conversion)**: Due to the unified memory
  layout across all contiguous vector/array-like types, a [T] slice can
  represent the active portion of any `[T; N]` ( Fixed-Capacity Array),
  `Vec<T; N>` (Fixed-Capacity Vector), or `Vec<T>` (Dynamic-Length Vector) with
  *zero-cost conversion* (no data copying or runtime transformation required).
  It's simply a reinterpretation of the existing memory.

#### Slice/View Memory Layout

| Field      | Type     | Notes                                        |
| :--------- | :------- | :------------------------------------------- |
| `capacity` | `u16`    | Maximum allocated capacity (from the source) |
| `len`      | `u16`    | Number of elements in this view              |
| `Item 0`   | `Type T` | First element of the buffer                  |
| `Item 1`   | `Type T` |                                              |
| `...`      | `...`    |                                              |
| `Item M-1` | `Type T` | Last element of the buffer                   |

- **Key Behavior:**
  - **Structural Identity:** This type has the **exact same memory layout and
    fields** as `Vec<T>`.
  - **Capacity:** Stored **runtime `u16` field**.
  - **Length:** Stored **runtime `u16` field**, represents the length of the
    *view*. **Cannot be changed** through the slice type (even if the binding is
    mutable).
  - **Bounds Check:** Access is checked against `len`.
  - **Usage:** A `Vec<T>` (or `Vec<T; N>`) can be passed as `[T]` to a function.

### 5. Fixed-Capacity Map (Owned Storage): `[K, V; N]`

This type represents an owned, fixed-size hash map where all buckets are stored
directly within its contiguous memory block. It uses an open addressing (e.g.,
linear probing) strategy for collision resolution.

Syntax: `[KeyType, ValueType; Capacity]` (e.g., `[String, Int; 100]`)
Purpose: For maps where the maximum number of buckets (N) is known at compile time.

Slice mutability is a checked at compile time, and is not any different in
runtime.

### 6. Dynamic-Length Map: `[K, V]`

This is the non-capacity-specific type that represents a map. It has the exact
same in-memory structure as `[K, V; N]`, allowing it to be passed around
without the specific N being part of its type. Its len can be changed (if
mutable), and elements can be inserted/removed up to the underlying capacity.

### 7. Internal Compiler Concept: Compiler-Internal Initializer List

This is **not a runtime type** that programmers would declare or manipulate
directly. It is an internal compiler construct used to optimize the
initialization of all collection types  (`[T;N]`, `Vec<T;N>`, `[K,V;N]`, etc.)
from literal values or expressions.

- **Purpose:** To provide a highly efficient mechanism for populating new
  collection instances.
- **Structure (Conceptual):** At compile time, it represents a sequence of
  expressions that will yield elements (for vectors/arrays) or key-value pairs
  (for maps).
- **Mechanism:** The compiler generates specialized, optimized code that
  directly evaluates these expressions and places the resulting values into the
  target collection's allocated memory. This direct filling ensures that there
  is **no intermediate collection created** and no extra memory copy overhead
  during initialization, making it as performant as hand-coded initialization
  loops.
