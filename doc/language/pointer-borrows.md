# Pointer borrows

The address of an indexed pointer refers to the pointed-to storage, including
when the pointer comes from a cast. The pointer expression need not be a variable.

Global fixed arrays have static storage, so pointers to their elements can be retained.
A local fixed-array element can be borrowed directly by a function call, including when
parentheses surround the address. In translated C files, pointer offsets can be part of
that call argument. Storing a local fixed-array address still requires `unsafe`.

Translated files also allow aliases of struct pointer parameters, including aliases
stored in struct fields. Ordinary V files retain their non-heap pointer parameter checks.
