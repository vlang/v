# Fixed arrays in translated C

In files marked `@[translated]`, a fixed array can be passed to a pointer parameter.
The pointer addresses the first element. Its element type must match the parameter,
except that `voidptr` accepts any element type and C byte types (`char`, `i8`, and `u8`)
can be used interchangeably. The same compatibility rules apply to pointer assignments
and equality or inequality comparisons with pointers.
Aliases inside nested fixed arrays and pointers resolve to their underlying types;
array lengths and integer widths must still match.
Nested lengths compare by value, including named constants, for byte-compatible pointer
assignments and calls. Nested rvalue literals are materialized before their first row is
passed to a pointer parameter.
When a later call argument contains an `if` or `match` expression, the pointer still
addresses the original array. Indexed array arguments retain the index evaluated
before that later argument. Arrays passed by value keep their value-copy semantics.
Dynamic arrays retain their V representation and do not decay to element pointers.
Ordinary V files retain their usual argument checks.
