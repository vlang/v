# Size expressions in translated C

In `@[translated]` files, `sizeof` accepts constant expressions and named values.
Declarations later in the same module are recognized, including constants in deferred
compile-time type or size conditions. Declaration attributes such as `@[if feature ?]`
exclude disabled candidates from the name lookup.

Known type names retain their type interpretation, including lowercase aliases and
function-local types. Ordinary V files retain their existing `sizeof` parsing rules.
