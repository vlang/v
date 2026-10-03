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

An inline anonymous struct parameter accepts a matching anonymous literal from another module.
Anonymous types declared as fields or aliases keep their declared field visibility.

Empty C typedef declarations can represent scalar types defined by a C header, such as `wchar_t`.
Explicit casts from those values to numbers or runes are validated by the C compiler.
Declared C structs with fields still cannot be cast to numbers or runes.

Interface method calls use the interface's method declarations, including inherited declarations,
for access checks, even when a concrete receiver in another module has the same name.
Private concrete methods remain private.

An explicit `unsafe { nil }` interface field initializer creates an empty interface, including
inside parentheses or a collapsed struct literal. Bare `nil` still requires an unsafe block.

Calls inferred inside a comptime sum-type branch use that branch's concrete variant type.
Explicit generic arguments retain the types written at the call site.
