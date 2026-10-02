# Fixed array allocator families

Heap promotion preserves the alignment of a fixed array and its containing struct.
On Windows, with garbage collection and preallocation disabled, V's `malloc`,
`memdup`, and aligned heap copies all allocate through `_aligned_malloc`.
Builtin `free` uses the matching `_aligned_free` family, including
when an unannotated struct inherits alignment from a fixed array field.

Pointers allocated by `C.malloc` or a foreign C API retain that API's ownership rules.
Release those pointers with `C.free` or the API's own deallocator. A cast to a V struct
pointer does not change the allocator family.
