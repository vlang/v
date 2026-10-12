# Generic return inference

A generic call used as a function's return value can infer its result type from that function's
declared return type. This context also reaches the value inside `dump(...)`, the operand of
unary `+`, `-`, `~`, or `!`, and the selected branch of a returned expression. Numeric arithmetic,
boolean logic, flag operations, and string concatenation also receive compatible return context.
For `<<`, `>>`, and `>>>`, it reaches only the left operand; the shift count keeps its own type.
Nested generic calls receive the inferred parameter type of the surrounding call. For example,
returning `identity(default_value(false))` as `string` supplies `string` to both generic calls,
while a parameter declared
as `int` supplies `int` to its argument. Explicit type arguments and typed value arguments retain
precedence over the enclosing return type.

Returns inside `$for variant in Sum.variants` retain the same context when a branch narrows a
sum value to the current variant. The variant's type does not replace the declared return type
of an unrelated generic call.

Receiver and argument types still determine generic calls that are not return values.

String interpolation uses the checked concrete result type of a direct generic call, including
a parenthesized call or one unwrapped with an `or` block. Its arguments are lowered normally,
so map and array methods can be passed to the call. The result retains the string representation
of its concrete type, including runes, arrays, and structs.
