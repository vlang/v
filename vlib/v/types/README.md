# Type checking

Compiler stages can retain a checked expression's semantic identity with
`TypeChecker.checked_expr_type_id(node)`. The returned `TypeId` belongs to that checker's
interner; a different worker can assign the same number to a different type.
Use `type_by_id(id)` for the canonical semantic value and `type_name_by_id(id)` only where
existing code still needs a textual spelling. `c_type_by_id(id)` reads the C layout cache by
identity without hashing the semantic type again. Rewriting or retyping a node invalidates its
cached identity. `parse_type_id(text)` resolves source text at an indexed type boundary;
its scalar cache keeps both the semantic ID and compatibility value across source contexts.
Source annotations and dynamically resolved call types remain separate.

`TypeChecker.semantic_type_id(value)` interns a semantic descriptor in the same table.
Markused retains declared sum receivers by this identity and keys checked receiver-method
lookups by numeric type and method indexes. Unresolved source spellings use their own indexes,
so a semantic ID must not be substituted for a source-spelling ID.

Nominal `Struct`, `Interface`, `Enum`, `SumType`, and `Alias` descriptors store their source
identity in `name_id`, a `u32` assigned by `flat.source_type_id`. Use `name()` to resolve a
spelling at an integration boundary. Equality, hashing, and owned copies retain the numeric
identity. This spelling identity is shared across workers; semantic `TypeId` values still belong
to a particular checker's interner.

Alias targets, generic binding values, and original return annotations use `flat.TypeTextBindings`.
Parameter annotations, sum variants, implemented interfaces, and interface embeddings use
`flat.TypeTextListBindings`. Binding keys retain declaration names; values carry source IDs.
`InterfaceImplIndex` retains its ordered implementation names in a `TypeTextList`;
`names_text()` resolves them for diagnostics and emission while preserving interface tag order.
Canonical name caches, recursive-alias stacks, and deferred return annotations also retain IDs.

Generic constraints, frozen interface implementation lists, and cached C type representations
also retain numeric spelling identities. AST and worker publication copy these indexes and their
list storage directly; only ordinary declaration payloads require string promotion.

Numeric APIs retain identities across stages. Text APIs resolve spellings when parsing source,
reporting diagnostics, or integrating with an emitter that consumes names.

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
`TypeChecker.reached_library_body_nodes(used)` returns the remaining declarations accepted
by the convergence check, including ambiguous short names. After markused completes that
check, `library_body_convergence_complete` certifies its returned reachability set. The driver
can omit a duplicate scan until another path adds roots, such as profiling.
Reachability can complete their semantic checks without revisiting already-checked bodies
when the declaration arena is unchanged.

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

Generic struct initializers validate concrete values before coercing numeric fields, including
captured values and handled fallible calls. Incompatible values report the field name and types
at the initializer. Matching reference values keep the normal implicit dereference into a value
field; references to a different value type remain incompatible.
Compiler integrations can use `TypeChecker.expr_compatible(node, actual,
expected)` to apply the same source-specific conversion rules, including translated C conversions.
An invalid initializer expression keeps its primary diagnostic without a second field-type error.
`TypeChecker.expr_subtree_has_node_error(node)` checks diagnostic node identities, so errors from
another generic instantiation at the same source position do not suppress field validation.

When a later struct field or call argument contains an `or` expression, earlier values are
evaluated first. Specialized mutable method receivers keep their original storage, and indexed
callbacks are selected before a later fallible argument can replace them.
Declared function references remain inline so their expected callback adapter type is preserved.
