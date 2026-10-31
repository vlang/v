# Expressions in match conditions

Conditions in `match true` can compare optional values with casts such as
`value == ?Choice(.first)`. Different cast operands identify different conditions,
just as different selector paths, indices, and cast target types do.
Repeating the same condition still produces a duplicate-case error.
