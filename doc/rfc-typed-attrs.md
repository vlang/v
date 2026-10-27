# RFC: Typed Custom Attributes as Compile-Time Contracts

Status: RFC, docs only. Builds on in-progress typed-attribute
work (`check_attribute_schema_decl`, `check_custom_attribute_uses`,
`attribute_schemas` in `vlib/v/types/checker.v`,
`doc/custom_attributes.md`,
`doc/custom_attribute_arguments.md`) and unifies the contract
RFCs (function, capability, task-group, FFI, config, wire,
overflow, snapshot, non-exhaustive, sendable, guarded-by,
refinement) under one foundation. No new compiler feature here.

## Syntax (illustrative)

```v okfmt
@[attribute]
struct Route {
	path   string
	method HttpMethod
}
```

```v okfmt
@[Route{ path: '/users'; method: .get }]
pub fn list_users() {
}
```

The struct is the schema: field names, types, required values,
and defaults are validated before framework processing. Each
attribute declares its valid targets (fn, method, struct,
field, enum, module); mismatches are compile errors. Built-in
attributes stay compatible; unifying their representation is
separate work.

## Core requirements

1. Typed arguments: checked with ordinary V types and constant
   expressions; invalid args are compile errors, never silent
   strings.
2. Target restrictions: enforced by the compiler per attribute.
3. Compile-time availability: structured metadata for comptime
   evaluation and tooling (routes, serializers, schema
   generators) without general runtime reflection.
4. Semantic levels, explicitly distinct:
   - Metadata: consumed by libs/docs/tools; never changes
     language semantics silently.
   - Contracts: guarantees the compiler actually verifies
     (allocation, concurrency, ownership). Presence alone
     proves nothing; `@[NoAlloc{}]` without a check is an
     error, not a proof.
   - Codegen: supported transforms via explicit mechanisms only;
     no arbitrary AST rewriting in v1.
5. Zero mandatory runtime cost: metadata erased by default;
   reflection tables only on explicit opt-in.
6. Tooling: VLS, `v doc`, formatters, and analyzers discover the
   definition, schema, targets, and docs; malformed attributes
   never produce misleading results.

## Enabled catalog (separate RFCs, not requested here as builtins)

API: `NonExhaustive`, `Since`, `Stability`. Correctness:
`Range`, `Sendable`, `GuardedBy`, `MustUse` (existing
`@[must_use]` accommodated, not duplicated). Memory/FFI:
`ReturnsOwned`, `NoEscape`, `LengthOf`. Execution: `NoAlloc`
(implemented), `NoSuspend`, `Deterministic` (checked or
documented-only). Protocols/config: `WireFormat`, `Config`,
`Secret` (redaction hint, not encryption), `DbIndex`. Tooling:
`Example`, `Fuzz`, `Benchmark` (library-defined).

## Phases

1. Foundation: typed declarations, arg/target validation,
   defaults, structured comptime/tooling access, builtin
   compatibility, no runtime overhead.
2. Library integration: routes, config, serialization, docs;
   VLS support.
3. Contracts: `Sendable`, `GuardedBy`, `NoAlloc`, `NoSuspend`,
   FFI ownership — each with guarantee, coverage, foreign
   behavior, and diagnostics specified separately.
4. Catalog review: which builtins stay compiler-specific vs
   become typed attributes; no gratuitous breaking changes.

## Non-goals and acceptance

No macro language, no safety bypass via attributes, no
mandatory reflection, no builtin flood, no metadata-as-proof,
no adoption mandate, no AST rewriting in v1. Done when:
schemas/targets validated at compile time; metadata visible to
comptime and tools; builtins unaffected; no default runtime
cost; library attributes need no compiler patch; contracts
kept distinct from metadata with a documented path per
attribute.