# RFC: Declarative Memory and Lifetime Contracts for Foreign Functions

Status: RFC, docs only. No `nonnull`/`noescape`/`len_of`/
`returns_owned`/`returns_borrowed` today. Builds on `@[trusted]`
on `C.*` (`vlib/v/parser/parser.v`) and trusted `@[noalloc]`
foreign contracts (`vlib/v/types/checker_noalloc.v`).

## Syntax (illustrative)

```v okfmt
@[nonnull: 'buffer']
@[noescape: 'buffer']
@[len_of: 'buffer=len']
fn C.consume(buffer &u8, len usize)

@[returns_owned: 'C.free_value']
fn C.create_value() &C.Value

@[returns_borrowed: 'buffer']
fn C.find_value(buffer &u8, len usize) &C.Value
```

## Contracts

- `nonnull`: reject literal `nil`/`unsafe{nil}`/unchecked optionals;
  otherwise keep the boundary (no silent proof).
- `noescape`: valid for the call only; integrates with borrowed
  storage, `-autofree`, and `-prealloc` rules.
- `len_of`: bytes vs elements stated; only constants, `.len`,
  and `sizeof` are statically checked.
- Ownership: owned (with named releaser, never assumed `free`),
  borrowed (lives while input lives, no escape, no free),
  external lifetime. Releaser must resolve to a declared fn.

## Trust model

Annotations on `C.*` are trusted claims, not proofs. Call sites
are checked where information exists. Unannotated `C.*` keeps
current behavior. Attributes are validated (param names exist)
and survive the module cache.

## Scope and acceptance

Phase 1: validation plus `nonnull`/`noescape` lint. Phase 2:
`len_of`. Phase 3: owned/borrowed plus `-ownership`/`-autofree`
integration. Tests cover validation, call-site diagnostics,
releaser wiring, and cached dependencies.
