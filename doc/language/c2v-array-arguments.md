# Fixed arrays in translated C

In files marked `@[translated]`, a fixed array can be passed to a pointer parameter.
The pointer addresses the first element. Its element type must match the parameter,
except that `voidptr` accepts any element type and C byte types (`char`, `i8`, and `u8`)
can be used interchangeably. The same compatibility rules apply to pointer assignments
and equality or inequality comparisons with pointers.
Dynamic arrays retain their V representation and do not decay to element pointers.
Ordinary V files retain their usual argument checks.
