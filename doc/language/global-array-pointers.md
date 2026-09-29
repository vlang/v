# Global pointers to fixed arrays

A global initialized with the address of a fixed-array literal receives initialized storage
before `main` runs. For example, `__global values = &[4]int{}` points to four zeroed integers,
and `__global values = &[3, 5]!` points to the two supplied values.

The storage remains valid after global initialization. Element defaults also apply to arrays
of structs and nested fixed arrays, just as they do for local fixed-array literals. Initializers
such as `&[4]int{init: index * 2}` fill each element before the global pointer is assigned.
Arrays of aligned structs retain the alignment required by their elements.
This also applies to fixed-array aliases, nested fills, and alignment inherited
through struct value fields.
An alias constructor such as `&Cells{init: 7}` also fills every element.
Alias chains and nested rows returned by functions are initialized as well, including
array literals whose row functions return fixed-array aliases.
Parentheses and `unsafe` blocks around the fixed-array literal preserve this initialization,
including `&(unsafe { [4]int{} })`. Statements before the final literal run in order, and local
bindings keep their scope, as in `unsafe { value := 7; &[4]int{init: value} }`.
Optional or result elements retain inherited alignment, and freeing aligned
array pointers uses the matching aligned deallocator.

Use `free` for V allocations and `C.free` for C allocations. Casting either allocation to an
aligned fixed-array pointer does not change which allocator owns the memory.
Boehm GC, VGC, and preallocation builds retain their builtin allocation and cleanup semantics for
aligned array pointers as well as ordinary allocations. VGC traces managed objects referenced
by the elements of aligned arrays.

Conditional `if` and `match` initializers allocate storage for the selected literal branch.
A branch that refers to an existing global array preserves that array's identity.
With VGC, global fixed arrays and fixed-array pointer slots are scanned as roots, so their
managed contents remain reachable without copies of the pointers on thread stacks.
