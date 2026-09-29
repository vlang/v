# Translated fixed-array arguments at C boundaries

In `@[translated]` files, a fixed array can decay to a pointer argument. V's platform `int` and
C's `int` can have different storage widths, so passing a fixed array of V `int` values to a C
`&int` parameter converts its elements to temporary C `int` storage for the call.

Changes made by the C function are copied back to an addressable source array, including a nested
row. Array literals and arrays returned by functions are also converted before the call. The C
temporary is valid for the duration of the call.

Arguments with overlapping source ranges share the converted buffer, including a whole array and
its rows passed in either order. Pointer equality, offsets, and mutations remain visible across
parameters. Nested fixed arrays are converted in full when a C parameter points to a fixed-size row,
including aliases of that pointer type.
