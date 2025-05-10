# Core Principles and Rules of the Swamp Language Ownership Model

## Environment

Swamp operates under the following foundational principles:

- **Single-Threaded Execution:** All operations are considered within a single
  thread of execution, simplifying state management and eliminating data race
  concerns at the language level. But the idea is to have multiple processes
  running in parallel where each operation itself is single threaded. Say
  audio::tick, render::update, and simulation::tick() can happen at the same
  time, but with totally isolated data.
- **No Aliased Mutable Owned Resources by Default:** There are no global
  variables, singleton or other global state. Functions returning "owned" data
  provide new, unique instances.
- **No Unmanaged Resource Pointers:** Pointers in Swamp refer to data managed
  within its own memory model (primitives, or frame data for complex types).
  There are no raw, unmanaged pointers to external resources that would require
  manual lifetime management outside Swamp's ownership and GC system. Resources
  are managed using register stored values (u32 handles).
- **Automatic Memory Management:** Heap-allocated data inside frame memory
  placed "collections" are managed by a fast garbage collector (GC),
  automatically reclaiming memory for objects that are no longer reachable. The
  GC is fast since it only has a single root to consider.

---

## Rules

### Rule 1: Variable Ownership and Data Representation

All variables in Swamp are owners of the data they represent.

- **Primitive Types** (e.g., integers, booleans, fixed point (floats)) hold
  their values directly. Ownership is straightforward value ownership.

- **Complex Types** (all other types) are represented by an **owned `u32`
  pointer**. This pointer refers to the actual data of the complex type, which
  resides allocated in the frame. The variable directly owns this pointer and,
  through it, has exclusive ownership of the frame data it points to (unless
  explicitly shared via immutable views as per Rule 4c). If the frame stored
  type is a collection type (Vec, Map, etc), they can also point to heap
  allocated data.

---

### Rule 2: Function Calls - Return Value Ownership

Function returns in Swamp **always transfer full ownership** of the resulting
data to the caller.

- For **primitive types**, the value inside the register is copied to the
  callers register.

- For **complex types**, the caller is responsible for allocating the necessary,
  zeroed, frame memory for the result _before_ the function call. The called
  function then initializes this pre-allocated frame memory directly (via an
  implicit hidden pointer argument). The `u32` pointer to this now-initialized,
  caller-owned frame memory is what the receiving variable in the caller's scope
  is assigned and takes/confirms ownership of.

---

### Rule 3: Function Calls - Parameter References (Borrows)

Non-owning mutable references (`&` syntax) are a distinct and strictly limited
mechanism in Swamp:

- They are permitted **exclusively for passing temporary borrows as function
  parameters**.
- The caller guarantees that the data pointed to by the reference is valid and
  owned for the entire duration of the function call. Either it is stored in
  their frame.

  - In future versions of Swamp, this reference can be allocated on the heap
    area owned by collection and the "collection" that owns that heap are is not
    reachable through other parameters during the call.

- These references are **ephemeral**: they can **never be stored** in variables,
  struct fields, or any other data structure, nor can they be returned from
  functions. This ensures they cannot outlive the data they borrow. The function
  can mutate them and are also allowed to reborrow them in arguments to other
  functions.

  It is safe to deep-clone the ephemeral reference into an ownership through
  assignment (see below), but should probably issue a warning since this is not
  generally recommended. TODO: Should maybe not even be allowed, since it is
  semantically suspicious.

---

### Rule 4: Assignment (`=`) Semantics

- **(a) Primitive Types:** Assignment between primitive types always results in
  a **`ValueCopy`**. The value from the RHS is copied to the LHS.

- **(b) Complex Types - LHS Assigned from a New Instance (Literal or Function
  Call):** When the RHS is a complex literal (e.g., `"[1,2,3]"`,
  `"a string literal"`) or a function call returning a complex type, the LHS
  (whether its context is for mutable access or an immutable view) receives a
  pointer to a newly created and owned data instance.

  - `AssignmentKind`: **`PointerCopyTakeOwnership`**.

- **(c) Complex Types - LHS is an Immutable View, RHS is an Existing IMMUTABLE
  Variable/Location:** When assigning an existing _immutable_ complex
  variable/location to an LHS that provides an immutable view (e.g.,
  `immutable_view = existing_immutable_complex_var.field`). Since neither the
  source data nor the new view can be used for mutation, it is safe to share the
  underlying data.

  - `AssignmentKind`: **`PointerCopyShared`**. (The resulting pointer value is
    copied; both LHS and RHS point to the same frame data).

- **(d) Complex Types - LHS (Mutable Access Context) from Existing RHS
  Variable/Location (`mut lhs_var = rhs_var`):** The behavior depends on the
  characteristics of the `rhs_var`'s type:
  - **d1. If `rhs_var`'s type is "Complex Blittable":** (A "blittable" complex
    type is one whose entire frame-allocated data block can be duplicated by a
    simple memory copy to create a semantically independent new object. This
    implies it contains only primitives or pointers that can be safely bitwise
    copied without causing unintended sharing of mutable sub-components.)
    - The assignment `mut lhs_var = rhs_var` is **allowed directly** (no
      explicit method call).
    - `AssignmentKind`: **`BlitAndTakeOwnership`**. LHS gets a new, independent
      copy via efficient memory copy of frame data and takes ownership.
  - **d2. If `rhs_var`'s type is "Non-Blittable Complex":** Direct assignment
    `mut lhs_var = rhs_var` is a **Semantic Error**. You need call an explicit
    method, since it is not automatically allowed by the Swamp rules to obtain
    an owned copy:
    - **`lhs_var = rhs_var.clone()`**: Use this if the type's `.clone()` method
      correctly handles cloning its non-blittable fields (only "one level deep"
      in its own implementation) to produce an independent owned copy.
    - **`lhs_var = rhs_var.deep_clone()`**: Use this if the type's non-blittable
      fields contain items that are themselves non-blittable, requiring a more
      explicitly thorough or potentially very costly recursive duplication. This
      signals intent for a full, deep copy.
    - For both explicit `clone()` and `deep_clone()`, the assignment of the
      method's result ( which is a new owned instance) to `LHS_Var` uses
      `AssignmentKind`: \* \*`PointerCopyTakeOwnership`\*\*.
