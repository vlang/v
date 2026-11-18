# FFI lifetime contracts

Five declarative attributes describe foreign (and V) function memory and lifetime
expectations: `@[nonnull]`, `@[noescape]`, `@[len_of]`, `@[returns_owned]`, and
`@[returns_borrowed]`. The compiler validates the contract schemas and checks call
sites where the argument is statically known.

```v
@[nonnull: 'buf']
@[len_of: 'buf=n']
fn C.copy_data(buf &u8, n int) int

@[returns_owned: 'free_value']
fn C.make_value(n int) voidptr

fn free_value(p voidptr) {
	unsafe { C.free(p) }
}
```

Attributes support both `name: value` and call-style `name(value)` syntax, with
comma-separated parameter lists such as `@[nonnull: 'a, b']`. A `len_of` entry
pairs a buffer with its length as `buf=len`. `returns_owned` names the single V
or C function that releases the returned value. Each named parameter must exist
on the annotated declaration, and a releaser must resolve to a declared V or C
function. Anything else is an error. The contracts sit only on V and C function
declarations.

`@[nonnull: 'p']` rejects `nil` and `none` arguments, including `unsafe { nil }`,
and rejects passing an unchecked optional where the call has no `or` block. An
argument handled with `or` is accepted. `@[noescape: 'p']` applies the same nil
rejection. Unannotated C declarations keep their current behavior, and calls the
compiler cannot resolve are skipped, so contracts only ever add diagnostics.

Annotations on C declarations are trusted claims: the C implementation, not the
compiler, upholds them. The declaration author must verify them. The standalone
FastC backend rejects V functions carrying these contracts; use the C backend.
Cached dependencies are re-read from source when contracts are present so their
declarations stay visible.

Buffer-length checking beyond schema validation, ownership-transfer enforcement
for `returns_owned`, borrow tracking for `returns_borrowed`, and escape analysis
for `noescape` are deferred to later phases.
