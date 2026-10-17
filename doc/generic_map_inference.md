# Generic inference for maps and callbacks

Generic calls infer their type arguments from map key and value types and from each
value returned by a callback. Caller-defined interface types retain their identity
when passed through these parameters.

For example, `maps.to_map` called with a `map[string]SomeInterface` and a callback
returning `(string, string)` produces a `map[string]string`. It cannot be passed to
a function requiring `map[string]SomeInterface`, even when the generic call appears
directly inside that function's arguments.

Likewise, `maps.invert` swaps the input's key and value types. The expected result
type cannot replace the types inferred from its input. Incompatible map results are
reported as type errors before code generation.
