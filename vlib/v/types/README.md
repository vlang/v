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

Compiler integrations can prepare a `LibraryBodyFrontier` after the initial semantic check
and pass reached declaration IDs to `TypeChecker.check_library_body_frontier_nodes`.
The snapshot preserves declaration order, source context, and checking ranges. A changed
AST or an unindexed declaration returns `none`, so integrations can fall back to
`check_reached_library_bodies` with the complete used-function map.

Verbose builds include bodies checked by reachability frontiers in the `checked late` count.
The initial pass also checks the library methods that a generic body names in a member
access, and what their bodies name. A call on a value of a type parameter has no receiver
type for reachability to resolve before the generic instances exist.

An `or` block in a struct field initializer must provide the unwrapped payload type.
For a `?bool` field, use `input.value or { false }`; an optional fallback value is rejected.
Direct option values can still initialize optional fields without an `or` block.

Variadic function types keep their variadic tail when used as parameters or fields.
For `fn (int, ...string) bool`, a call must supply the fixed `int` argument and may supply
zero or more strings. A `fn (int, []string) bool` still requires an explicit array argument.
Restoring transformed function values and reconstructing callback signatures also keep this tail.

An interface narrowed by `if mut value is T` can be passed to a function accepting `mut T`.
The function mutates the same concrete object stored in the interface.
A `mut &T` parameter still needs a mutable pointer variable; narrowing an interface does not
create a pointer slot that the function can reassign.

Each `_test.v` file must contain at least one active `test_` function. A file whose tests
are all excluded by conditional compilation reports a missing-test error. When the file
has no active function declarations, this diagnostic points to the start of the file;
a helper-only file points to its helper function.
