# Translated C source

Files generated from C can mark their module with `@[translated]`.
In these files, struct initializers may omit pointer fields, including pointers in nested
structs. Those fields are initialized to null, matching C aggregate initialization.
Explicit `@[required]` fields and incompatible field values are still checked.
Ordinary V files retain their reference initialization checks, even when they use types
from a translated file.
