# Imported constants in fixed-array lengths

A fixed-array length referring to an imported constant respects its import alias.
For `import sizes as fx`, `[fx.max_items]int` uses `sizes.max_items`, even when another module
is named `fx` and imported under a different alias.
When the same alias has different meanings in different source files, constant resolution
leaves the length unresolved rather than falling back to a real module with the alias name.

Compile-time fixed-array size quantifiers support `$d()`; other forms, including `$env()`,
remain errors even when the environment value is empty or contains an integer.
