# Swamp Core Principles

## 0. What got us here?

I dislike and want to avoid the following:

- **deep clones**. very performance heavy. And related to that I want to avoid
  having the managing of collections like stacks and lists spread out on to very
  small "entities".

- **heap allocation** during runtime. It shouldn't be needed, except for things
  like string appends?

- **variable sized heap allocations**.

- **garbage collection**. Even though it is allocated in an arena and the
  garbage collection is just one root, it still takes time to do a deep clone of
  the root object.

## 1. The World Model

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
// Declares a container named 'Monster'.
// 'Monster' also becomes the distinct handle type for its elements.
// Capacity is 512 elements.
// The structure of each 'Monster' element is defined "inline":
container Monster[512] {
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
container ActiveProjectile: ProjectileData[2048]
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
  and linear probing and similar.

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
