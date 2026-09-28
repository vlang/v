# Generic return inference

A generic call used as a function's return value can infer its result type from that function's
declared return type. This context also reaches the value inside `dump(...)` and the selected
branch of a returned expression.

Returns inside `$for variant in Sum.variants` retain the same context when a branch narrows a
sum value to the current variant. The variant's type does not replace the declared return type
of an unrelated generic call.

Receiver and argument types still determine generic calls that are not return values.
