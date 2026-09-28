# Size expressions in translated C

In `@[translated]` files, `sizeof` accepts constant expressions and named values.
Declarations later in the same module are recognized, including constants in deferred
compile-time type or size conditions. Declaration attributes such as `@[if feature ?]`
exclude disabled candidates from the name lookup.
Qualified names through imported modules, including import aliases, are resolved after parsing
so that both constants and type names retain their meaning.
When deferred type-test branches declare a constant and a type with the same name, `sizeof`
uses the constant's storage type only when the constant's branch is selected.

Known type names retain their type interpretation, including lowercase aliases and
function-local types. Ordinary V files retain their existing `sizeof` parsing rules.
