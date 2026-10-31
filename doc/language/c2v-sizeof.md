# Size expressions in translated C

In `@[translated]` files, `sizeof` accepts constant expressions and named values.
Declarations later in the same module are recognized, including constants in deferred
compile-time type or size conditions and header-style constants without initializers.
Top-level `$match` declarations follow the selected arm when its subject is known. Deferred
matches and location-dependent `$if` or `$match` conditions retain candidates until normal
parsing in the declaring file or compile-time selection resolves them.
Declaration attributes such as `@[if feature ?]`
exclude disabled candidates from the name lookup.
Globals are recognized before their declarations, including grouped globals in other parsed
files, so an indexed expression such as `sizeof(Regs[0])` keeps its value interpretation.
Globals from another module do not force current-module or imported type names to be parsed
as value operands.
Qualified names through imported modules, including import aliases, are resolved after parsing
so that both constants and type names retain their meaning. Enum members such as `sizeof(Color.red)`
and members through enum aliases are measured as values, including enums declared in later files.
When deferred type-test branches declare a constant or global and a type with the same name,
`sizeof` uses the value's storage type only when its branch is selected, including header-style
constants that have a declared type without an initializer. Compound operands such
as `sizeof(Item + 0)` keep their expression interpretation when `Item` is a deferred constant.

Known type names retain their type interpretation, including lowercase aliases, generic struct
instantiations such as `sizeof(c_box[int])`, and function-local types. Ordinary V files retain their
existing `sizeof` parsing rules.
