# Translated fixed-array pointer arithmetic

In `@[translated]` files, adding an integer offset to a fixed array yields a pointer to an element.
The array may appear on either side of `+`. Global initializers can use this arithmetic with global
fixed arrays, including fixed-array fields and nested rows.

The pointer refers to the original array storage. Fixed-array addresses that cannot change during
the expression remain inline. When evaluating the right operand can affect the left operand's value
or address, the compiler captures the left operand first to preserve evaluation order.
Global initializers retain these temporaries too, including `offset() + values`.
When a global initializer decays a returned array or an array literal, its backing storage lasts
for the program lifetime and retains the element type's alignment.

An inferred global keeps its checked type, so an array-plus-offset initializer remains a pointer
when the global is read or passed to a function. This includes arithmetic within value-producing
`if`, `match`, and `unsafe` blocks, even when the called function is declared later.

Function-returned arrays and array literals used in pointer arithmetic receive owned, aligned
backing storage, so the resulting pointers remain valid after leaving a function or inner block.
Addressable array variables retain their original storage and aliasing behavior.
