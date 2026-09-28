# Struct values passed by reference

A struct update expression such as `Settings{...original value: 7}` can be passed
directly to an immutable reference parameter, just like a struct initializer.
The compiler materializes the updated value in a temporary before passing its
address. Updating the temporary does not change the original struct.
When an expected pointer type converts an update into a stored reference, such as
an element of `[]&Settings`, the updated value receives its own heap allocation.
The reference remains valid after the scope constructing the update returns.
When the stored element is a pointer to an interface, each pointer layer receives
its own storage after the updated struct is boxed as an interface.
Aliases of interface pointer types retain the same pointer depth.
Nested interface pointers compare their pointer values.
For nested pointers to a sum type, the update is wrapped as the sum value before
the pointer layers receive storage.
Nested sum variants are wrapped from the innermost sum outward.
