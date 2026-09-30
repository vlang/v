# Byte identifiers

Use `u8` for an unsigned 8-bit integer. The former `byte` type alias has been removed.
`byte` is an ordinary identifier and can name a constant, variable, function or method.

For example, with `const byte = 8`, a `byte` branch in `match value` compares `value` against
the constant `8`; it does not match every integer as a type pattern. The evaluator follows
the same rule. FastC also resolves `byte` expressions to their declared symbols and reports
an unresolved name when no such declaration exists.

A call such as `byte(8)` invokes the declared function, including when it takes one argument.
FastC uses a separate C function symbol so this name can coexist with its internal `byte` typedef.
`sizeof(byte)` measures the constant or variable's type; for `const byte = f64(8)`, the result
is `8`, including with the native ARM64 backend.
The evaluator also uses declared widths and resolves type aliases for `sizeof` value operands,
without evaluating a constant initializer or function call.

Generated documentation highlights `byte` as an ordinary identifier or function name,
while `u8` retains builtin type highlighting. Byte method documentation belongs to `u8`.
