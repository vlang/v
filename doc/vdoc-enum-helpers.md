# Enum helpers in generated documentation

`v doc` includes the compiler-generated `Enum.from[W](input W) !Enum` method for enums.
For flag enums, it also includes `zero`, `is_empty`, `has`, `all`, `set`, `set_all`, `clear`,
`clear_all`, and `toggle`, with their signatures and descriptions.

A declared static method replaces the generated helper with the same name. Its declared
visibility applies even when it appears before the enum or in a separate file. A private
replacement is omitted from public documentation; use `v doc -all` to include it.
