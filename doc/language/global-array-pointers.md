# Global pointers to fixed arrays

A global initialized with the address of a fixed-array literal receives initialized storage
before `main` runs. For example, `__global values = &[4]int{}` points to four zeroed integers,
and `__global values = &[3, 5]!` points to the two supplied values.

The storage remains valid after global initialization. Element defaults also apply to arrays
of structs and nested fixed arrays, just as they do for local fixed-array literals.
