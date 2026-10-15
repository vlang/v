# Type checking

Compiler stages can retain a checked expression's semantic identity with
`TypeChecker.checked_expr_type_id(node)`. The returned `TypeId` belongs to that checker's
interner; a different worker can assign the same number to a different type.
Use `type_by_id(id)` for the canonical semantic value and `type_name_by_id(id)` for its
interned source spelling. `c_type_by_id(id)` reads the C layout cache by
identity without hashing the semantic type again. Rewriting or retyping a node invalidates its
cached identity. `parse_type_id(source_id)` resolves an interned source spelling;
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

`parse_type`, `parse_type_ref`, `parse_canonical_type`, `parse_canonical_type_cached`,
`parse_resolution_type`, and `parse_resolution_type_in_file` accept these source spelling IDs.
Intern new source text with `flat.source_type_id(text)` at a parsing boundary. An AST annotation
already carries that ID in `node.typ`. Parsing still observes imports, generic bindings, and the
declaration context; identical spelling IDs can resolve to different semantic types.
`Type.source_type_id()`, `TypeChecker.type_name(value)`, and `type_name_by_id(id)` return spelling
IDs. Resolve bytes with `flat.source_type_name(id)` for diagnostics, spelling syntax, or emission.
Builtin spelling constants in `v.flat` avoid repeated interning of scalar names.
`fn_signature_type(name, source_id)` resolves an interned annotation in its declaration context.
Source qualification (`qualify_type_text` and `qualify_resolution_type_text`) and imported-type
resolution also pass spelling IDs. Private recursive spelling parsers work with bytes inside
these boundaries. C ABI encodings and emitted declaration names remain strings.

Alias targets, generic binding values, and original return annotations use `flat.TypeTextBindings`.
Parameter annotations, sum variants, implemented interfaces, and interface embeddings use
`flat.TypeTextListBindings`. Binding keys retain declaration names; values carry source IDs.
`InterfaceImplIndex` retains its ordered implementation names in a `TypeTextList`;
`names_text()` resolves them for diagnostics and emission while preserving interface tag order.
Canonical name caches, recursive-alias stacks, and deferred return annotations also retain IDs.
`TypeTextBindings.value_id(name)` and `NodeTypeTextBindings.value_id(node)` read a stored ID
directly, returning zero for an absent or empty spelling. Use `type_id` or `contains` when the
distinction between an absent binding and a present empty binding matters.

Generic constraints, frozen interface implementation lists, and cached C type representations
also retain numeric spelling identities. AST and worker publication copy these indexes and their
list storage directly; only ordinary declaration payloads require string promotion.

Transformer builders, expected-type coercion, and deferred AST type writes also carry spelling IDs.
Numeric APIs retain identities across stages. Text APIs resolve spellings when parsing source,
reporting diagnostics, or integrating with an emitter that consumes names.
Sum candidates and variants, map component tuples, and alias-equivalence caches retain source IDs.
Cached lists return independent numeric copies. Module and file changes invalidate contextual
caches.

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

Generic specialization requests, signature argument lists, and inferred type bindings carry source
IDs. Generic parameter names remain declaration strings. Numeric list queries preserve caller-owned
array storage; the specialization miss cache invalidates when local or parent bindings grow or its
parent changes. Substitution and generated-name rendering resolve spelling bytes at those
boundaries.
