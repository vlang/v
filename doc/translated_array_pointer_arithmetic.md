# Translated fixed-array pointer arithmetic

In `@[translated]` files, adding an integer offset to a fixed array yields a pointer to an element.
The array may appear on either side of `+`, including when the offset has an integer alias type.
Global initializers can use this arithmetic with global fixed arrays, including fixed-array fields
and nested rows.

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
When a local array or array field escapes through such a pointer, its containing local is moved
to the heap. Writes through the local and its pointers continue to affect the same storage.
Stores into globals, mutable pointer parameters, and indirect destinations also retain this storage.
The same promotion applies to arrays introduced by multiple or tuple declarations.
Temporary fixed-array arguments passed to translated functions and methods receive persistent
backing storage, including calls from ordinary V files. Addressable arguments keep their identity.

Local arrays passed to translated fixed-array parameters are conservatively promoted too, since
callees can retain their address without returning it. This can allocate even for a callee that
does not retain the array. Array values produced by blocks are copied before leaving that scope.
Ordinary functions and methods that forward arrays to these callees preserve the same retention
requirement through further wrappers, including recursive and generic calls. The original caller
provides persistent backing storage; forwarding a local array continues to preserve its identity.
When a program includes translated code, indirect calls through function values also preserve
fixed-array arguments conservatively, since their runtime target may retain the array.
