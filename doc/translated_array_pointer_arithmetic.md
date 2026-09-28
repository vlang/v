# Translated fixed-array pointer arithmetic

In `@[translated]` files, adding an integer offset to a fixed array yields a pointer to an element.
The array may appear on either side of `+`. Global initializers can use this arithmetic with global
fixed arrays, including fixed-array fields and nested rows.

The pointer refers to the original array storage. Fixed-array addresses that cannot change during
the expression remain inline. When evaluating the right operand can affect the left operand's value
or address, the compiler captures the left operand first to preserve evaluation order.
Global initializers retain these temporaries too, including `offset() + values`.

An inferred global keeps its checked type, so an array-plus-offset initializer remains a pointer
when the global is read or passed to a function.
