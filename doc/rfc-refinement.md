# RFC: Opt-In Refinement Types for Constrained Scalar Values

Status: RFC, docs only. No refinement/range-constrained scalar
types in tree (only unrelated "nonzero exit" and internal type
refinement). Distinct from plain `type` aliases, which do not
enforce value invariants.

## Syntax (illustrative)

```v okfmt
@[range: '1..65535']
type Port = u16
```

```v okfmt
@[range: '1..max_u32']
type UserId = u32
```

Exact declaration form follows V's type/attribute conventions.

## Semantics

1. Distinctness: a constrained type is not interchangeable with
   its underlying primitive. Passing a raw `u16` where a `Port`
   is expected is an error.
2. Compile-time validation: statically known out-of-range
   values (e.g. `Port` from literal `70000`) fail compilation,
   including through constant propagation.
3. Dynamic validation: unverifiable inputs go through a checked
   constructor returning `!T` (e.g. `Port.from(raw)!`); success
   establishes the invariant, no repeated per-access checks.
4. No bypass: ordinary casts, direct construction, or generic
   conversion must not silently circumvent the range. Any
   `unsafe` escape is explicit and documented.
5. Initial constraints: inclusive/exclusive integer ranges and
   nonzero; later possibly string/collection length bounds. No
   arbitrary predicates, no theorem proving, no dependent types.

## Codegen and performance

Metadata is erased from runtime representation where possible
(same layout as the primitive). Validation happens once at
construction from unverified input. The compiler does not claim
a range is proven when it only inserted a runtime check.

## Acceptance

Distinctness enforced; static violations rejected; dynamic path
validated with documented `!T` errors; conversions/generics
respect constraints; tests cover literals, constants, runtime
input, casts, and nesting; representation unchanged.
