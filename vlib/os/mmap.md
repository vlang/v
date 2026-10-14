# Memory-mapped files

`mmap_open(path, .read_only)` or `.read_write` maps an existing nonempty file.
`mmap_create(path, length)` creates or truncates a file to a positive, zero-filled length
and maps it read-write. Lengths larger than a V array can represent are rejected before creation
or truncation. Windows paths support Unicode.

`len()` reports mapped bytes. `bytes()` is unsafe because it returns a zero-copy view:
keep the mapping open, do not resize or free the view, and never modify a read-only mapping.
The underlying file must not be truncated while a view is in use.
`sync()` requests write-back of a read-write mapping; Windows also flushes the retained file handle.
Read-only and closed mappings return an error from `sync()`.
`close()` releases the mapping and invalidates all earlier views; repeated close calls are harmless.
