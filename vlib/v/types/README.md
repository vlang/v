# Type checking

The builtin error constructors require a string message. To propagate an error from an
`or` block, use `return err`. To add context, use `return error('context: ${err}')`.
Passing the error value directly as `error(err)` is rejected before C generation and includes
a hint to use `err`; `error_with_code(err, code)` is also rejected.

Enum values must be declared by their enum, including qualified values such as `Color.blue`
and shorthand values such as `.blue` in `match` branches. Unknown values are rejected before
C generation.

Compiler integrations can use `Scope.contains(name)` to check whether a binding is visible
in a scope or its parents without copying the binding's type. It includes unresolved type
bindings and follows the same shadowing and scope-reuse rules as `Scope.lookup(name)`.
