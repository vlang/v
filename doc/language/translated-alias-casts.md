# Translated C casts

In `@[translated]` files, a C typedef with a lowercase name can be used in a cast,
for example `uintptr_t(value)`. The alias may be declared in another file in the
same module. Its cast uses the same argument checks as other type casts.
Local function values with the same name still resolve as function calls.
Pointer casts also accept lowercase struct and typedef names, such as
`&debug_info(pointer)` and `&uintptr_t(pointer)`.

Translated files can also cast numeric values to enums, including lowercase enum names,
and to booleans directly, matching C conversions. Ordinary V files retain their explicit
`unsafe` requirements
for these casts, even when another file in their module is translated.
