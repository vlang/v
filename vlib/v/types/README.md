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

Inside a closure that captures a `@[heap]` struct receiver with `[mut receiver]`,
assigning the captured receiver to a new variable retains its reference, including when the
receiver is enclosed in parentheses or is the final value of an `unsafe` block.
Mutations through that variable reach the same heap object. Attributes are resolved on the
receiver's own declaration, including through aliases and generic instantiations; importing a
heap struct with the same short name does not change a non-heap receiver's value-copy behavior.

`TypeChecker.semantic_type_id(value)` interns a semantic descriptor in the same table.
Markused retains declared sum receivers by this identity and keys checked receiver-method
lookups by numeric type and method indexes. Unresolved source spellings use their own indexes,
so a semantic ID must not be substituted for a source-spelling ID.
Markused passes source IDs through local type queries and binding accessors. Its receiver cache
keeps source and semantic IDs in separate maps; method names remain strings.
The C backend retains source IDs through struct-field lookup and source qualification. C type
encodings and generated identifiers remain strings.
SSA type queries and source bindings also carry spelling IDs. SSA `TypeID` belongs to its
`TypeStore` and identifies a lowered layout; it is separate from both spelling IDs and checker
semantic `TypeId`. Resolve spelling bytes when lowering to that layout or looking up declarations.

Nominal `Struct`, `Interface`, `Enum`, `SumType`, and `Alias` descriptors store their source
identity in `name_id`, a `u32` assigned by `flat.source_type_id`. Use `name()` to resolve a
spelling at an integration boundary. Equality, hashing, and owned copies retain the numeric
identity. This spelling identity is shared across workers; semantic `TypeId` values still belong
to a particular checker's interner.

`new_type_reference(value)` retains a shallow descriptor copy in stable reference storage.
Its nested metadata stays borrowed. Use `clone_owned_type(value)` first when that metadata
belongs to an arena that will end. Checker parsing and interning retain canonical recursive
descriptor edges; use those APIs when a reference should share the checker's type identity.

Pass-2 worker publication checks the descriptor's referenced storage before releasing its arena.
Lists, strings and recursive descriptor references are copied when they depend on that arena;
graphs already outside it retain their references.

A `mut value ?T` parameter keeps the `&?&T` calling convention while its body reads `?T`,
including struct payloads and aliases of structs. Mutable payload guards and smartcasts still
refer to the caller's payload. An explicit pointer payload keeps its declared pointer view.

`parse_type`, `parse_type_ref`, `parse_canonical_type`, `parse_canonical_type_cached`,
`parse_resolution_type`, and `parse_resolution_type_in_file` accept these source spelling IDs.
Intern new source text with `flat.source_type_id(text)` at a parsing boundary. An AST annotation
already carries that ID in `node.typ`. Parsing still observes imports, generic bindings, and the
declaration context; identical spelling IDs can resolve to different semantic types.
`Type.source_type_id()`, `TypeChecker.type_name(value)`, and `type_name_by_id(id)` return spelling
IDs. Resolve bytes with `flat.source_type_name(id)` for diagnostics, spelling syntax, or emission.
Builtin spelling constants in `v.flat` avoid repeated interning of scalar names.
Field and interface queries accept source spelling IDs as well, including
`struct_fields_for_type`, `struct_field_type`, `interface_field_list`,
`interface_metadata_name`, and method-signature lookup by concrete or interface type.
The field cache uses those IDs for owners and keeps field names as declaration strings.
Its frozen declaration tables remain shared; private worker entries and misses merge by
owner and field without retaining a worker's mutable table.
`generic_type_name_matches` accepts source IDs and retains recursive spelling grammar internally.
`sum_variant_type_for_pattern` accepts source IDs for the sum and pattern and returns an
optional source ID. Its cache retains the writing file and module when matching a spelling.
`fn_signature_type(name, source_id)` resolves an interned annotation in its declaration context.
Source qualification (`qualify_type_text`, `qualify_resolution_type_text`, and
`qualify_type_name`) and imported-type resolution pass spelling IDs. `qualify_type_name` shares
the file, module, resolution-mode, and declaration-growth cache used by `qualify_name`;
declaration-name callers can retain the string interface.
Private recursive spelling parsers work with bytes inside
these boundaries. C ABI encodings and emitted declaration names remain strings.

Alias targets, generic binding values, and original return annotations use `flat.TypeTextBindings`.
Parameter annotations, sum variants, implemented interfaces, and interface embeddings use
`flat.TypeTextListBindings`. Binding keys retain declaration names; values carry source IDs.
`InterfaceImplIndex` retains its ordered implementation names in a `TypeTextList`;
`names_text()` resolves them for diagnostics and emission while preserving interface tag order.
`interface_impl_names`, `pre_transform_interface_impl_names`, and `ierror_impl_names` return
caller-owned spelling ID arrays. Interface query cache owners, frozen snapshots, and ordered
satisfaction-proof pairs use source IDs. `InterfaceImplIndex.ids`, `interface_type_ids`, and the
stable dispatch allocators map source spelling IDs to runtime integer tags. Those tags retain the
same spelling hash, collision probing, and lexical ordering; they are not source spelling IDs.
Recursive struct declaration queries also accept spelling IDs while field paths and diagnostic
messages retain their rendered text.
Canonical name caches, recursive-alias stacks, and deferred return annotations also retain IDs.
`TypeTextBindings.value_id(name)` and `NodeTypeTextBindings.value_id(node)` read a stored ID
directly, returning zero for an absent or empty spelling. Use `type_id` or `contains` when the
distinction between an absent binding and a present empty binding matters.

Generic constraints, frozen interface implementation lists, and cached C type representations
also retain numeric spelling identities. AST and worker publication copy these indexes and their
list storage directly; only ordinary declaration payloads require string promotion.

Transformer builders, expected-type coercion, and deferred AST type writes also carry spelling IDs.
Callback, tuple-branch, iterator, and local type queries return source IDs, including optional and
list results. Comptime sum variants retain those IDs when constructing reflection metadata.
Option/result queries also return numeric wrapper and payload pairs through coercion and lowering.
Signature preparation and deferred return registration retain source IDs across workers;
parameter and variant lists use numeric binding accessors and copy IDs into owned storage.
Generic substitution and explicit call arguments also use numeric lists. C generation retains
inferred generic arguments in maps of source IDs; private recursive spelling parsers resolve bytes
when matching nested types, and emitted specialization names remain strings.
Receiver pattern caches encode ordered source IDs with their file, module, and generic context;
receiver promotion and factory substitution retain those IDs through recursive lookups.
Sum-constraint traversal also stores source IDs in its visited and active recursion state.
Constrained generic-body checks retain IDs through type choices, branch groups, combinations,
and worker bindings. Allocation analysis also keeps parameter and local annotations as IDs.
The driver's implicit-import type scan and WebAssembly fallback annotations use the same source
IDs; their declaration indexes and dependency-recording keys retain names.
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

Map literals cannot use postfix `!`: they are ordinary map values and do not return a Result.
This rule also applies to map literals in constant declarations.

Generic specialization requests, signature argument lists, and inferred type bindings carry source
IDs. Generic parameter names remain declaration strings. Numeric list queries preserve caller-owned
array storage; the specialization miss cache invalidates when local or parent bindings grow or its
parent changes. Substitution and generated-name rendering resolve spelling bytes at those
boundaries.

Generic struct initializers validate concrete values before coercing numeric fields, including
captured values and handled fallible calls. Incompatible values report the field name and types
at the initializer. Matching reference values keep the normal implicit dereference into a value
field; references to a different value type remain incompatible.
Compiler integrations can use `TypeChecker.expr_compatible(node, actual,
expected)` to apply the same source-specific conversion rules, including translated C conversions.
An invalid initializer expression keeps its primary diagnostic without a second field-type error.
`TypeChecker.expr_subtree_has_node_error(node)` checks diagnostic node identities, so errors from
another generic instantiation at the same source position do not suppress field validation.
Concurrent generic-instance checks finish before a build returns lowering errors, preserving
the primary instance diagnostics.

When a later struct field or call argument contains an `or` expression, earlier values are
evaluated first. Specialized mutable method receivers keep their original storage, and indexed
callbacks are selected before a later fallible argument can replace them.
Declared function references remain inline so their expected callback adapter type is preserved.

Reference parameters such as `&Value` require storage of that same sum type. A reference to
one of its variants cannot be reinterpreted as a reference to the sum. Wrap a getter result
explicitly, for example `read(Value(dict['a']))`, to materialize a sum value for the call.
Existing sum bindings can be passed directly.

Pointer sum variants preserve the referenced object's identity. A smartcast can mutate the
referenced object, but it does not supply a pointer variable for a mutable pointer parameter.
This applies to nominal pointer aliases as well as explicit reference types. To replace an owned
pointer variant, assign the enclosing sum so its previous payload is released and the replacement
retains its own cleanup policy. Explicit unsafe access to the pointer slot remains a low-level
operation; the caller must preserve the enclosing sum's ownership metadata.
Explicit `drop_owned` on a named owned sum destroys its complete storage even after a smartcast.
Borrowed projections retain the usual copy policy. Lowering can query the checker's validated
node decision with `TypeChecker.ownership_sum_drop_consumes_storage(node)`.

Returning a mutable array parameter in a value-array sum variant creates an independent owner.
A selected pointer-array variant keeps borrowing the caller's storage. Option and Result wrappers
preserve the successful payload's ownership and their existing none or error cleanup.

Returning a wrapper of a local promotes that local and transfers its cleanup to the returned sum.
Storing an inner local's pointer in an outer sum similarly transfers cleanup at the store, including
through named sum wrappers.
An outer store with an ambiguous local pointer source is rejected; store each pointer alternative
separately so the checker can identify which allocation supplies its cleanup obligation.
This also applies to outer stores whose local source is hidden behind a borrowed-return call.
Returning an existing local through a known borrowed-return call retains its allocation, while
calls borrowing caller-owned storage remain non-owning. A local declared inside a returned
`if` or `match` expression cannot supply a sum pointer; declare it before that expression.
These lifetime checks also apply inside functions returning `void` and when a sum constructor
wraps the conditional expression. The checker records heap promotion before validating a local
pointer return. Return and assignment lowering preserve the original transfer decision through
their generated nodes, and only the matching runtime pointer acquires the cleanup obligation.
An outer sum assigned inside a loop keeps its cleanup at the outer binding's scope. Replacing its
payload releases the previous owner; values declared inside each iteration still clean up there.
Iteration-local loans end there too; loans held by outer bindings remain active across the loop.
Replacing a pointer sum field or element releases its previous owned payload. A wrapper of the same
pointer retains that cleanup obligation, and the destination expression is evaluated once.
Loop return ownership is summarized before checking callers, including callers checked in parallel
or declared before the factory. Statements within a loop share their lexical declaration context.
Wrapping an existing local's address preserves its identity and borrows its storage. Moving an owned
sum field transfers its runtime cleanup flag; ownership checking rejects later reads of that field.
Aggregate fields and elements cannot retain a pointer to a local from a shorter scope. Returning
a sum field or element that borrows local storage is also rejected, including through a named alias
or an enclosing aggregate. Keep an escaping allocation in a separately owned outer sum before
borrowing it into an aggregate in the same scope.
The same lifetime check applies when replacing the enclosing struct or array literal. Borrowed sum
fields keep their source loans through named aggregate aliases, including nested storage.
Conditional aggregate expressions cannot retain a sum pointer to a local declared inside them.
Moving a sum field or element transfers its cleanup while the remaining aggregate still cleans up.
A function returning either a fresh or borrowed pointer preserves its conditional cleanup and source
loans. Moving that sum transfers both obligations; borrowed-only return values remain copyable.
Wrapping a result in the same sum type preserves those loans and move obligations, including
conditional results and named aliases. Aggregate fields and elements retain conditional ownership
when their initializer may return either fresh storage or a borrowed pointer.
Unwrapping a Result or Option with `or`, `!`, or `?` preserves the successful sum's ownership and
source loans. An `or` fallback also retains its source loans, including named local wrappers.
These return lifetime checks reject a pointer to a local declared inside an `or` fallback.
Fallback aliases keep possible source loans across branches and loop iterations. A later
unconditional replacement clears the prior loan, and inner declarations keep their own scope.
Same-sum wrappers preserve conditional cleanup even when their sources are borrowed parameters.
Loop exits preserve the source loans held at `break` or `continue`, including labeled exits.
Only paths that continue within the body reach a later replacement; replacements after the loop
apply to its exited paths too.
