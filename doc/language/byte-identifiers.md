# Byte identifiers

Use `u8` for an unsigned 8-bit integer. The former `byte` type alias has been removed.
`byte` is an ordinary identifier and can name a constant, variable, function or method.

For example, with `const byte = 8`, a `byte` branch in `match value` compares `value` against
the constant `8`; it does not match every integer as a type pattern. The evaluator follows
the same rule. FastC also resolves `byte` expressions to their declared symbols and reports
an unresolved name when no such declaration exists.
