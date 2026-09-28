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
Alias chains and nested rows returned by functions are initialized as well.
Parentheses and `unsafe` blocks around the fixed-array literal preserve this initialization,
including `&(unsafe { [4]int{} })`. Statements before the final literal run in order, and local
bindings keep their scope, as in `unsafe { value := 7; &[4]int{init: value} }`.
Optional or result elements retain inherited alignment, and freeing aligned
array pointers uses the matching aligned deallocator.
