# Single-letter structs in options and results

Concrete struct names, including single-letter names, retain their types when wrapped in
`?Type` or `!Type`. They support the same unwrapping, fallback, and propagation operations
as other concrete struct types, both in a module and through an import.

For example, a module declaring `struct M` can return it from `fn get() !M` or
`fn find() ?M`. The wrapped value remains that module's `M`, rather than a generic parameter.
