# RFC: Compiler-Verified `@[sendable]` Types for Concurrency Boundaries

Status: RFC, docs only. No `@[sendable]` in tree (only unrelated
`WSASend`/`objc_msgSend`). Builds on `spawn`, channels, and the
optional ownership system (`doc/ownership.md`); complements
runtime race detection rather than replacing it.

## Syntax (illustrative)

```v okfmt
@[sendable]
struct WorkItem {
	id   u64
	name string
}
```

Marking a type requests verification. It must not act as a mere
promise that disables checks.

## Semantics

1. Recursive verification: fields, nested structs, arrays, maps,
   pointers, fn values, and foreign types are checked for safe
   transfer. Mutable references that cannot cross under V's
   concurrency and ownership rules are rejected unless an
   explicit safe-transfer or synchronization mechanism applies.
2. Boundaries: `spawn`/`go` arguments, channel sends, concurrent
   task returns, and values captured by concurrent closures.
   Ordinary calls require nothing.
3. Transfer is not sharing: `@[sendable]` allows moving a value
   across, not concurrent mutation of one instance. A shareable
   concept, if needed, is modeled separately.
4. Unknown/external types fail closed; the first version may
   conservatively reject whatever it cannot prove safe.

## Non-goals

No race-freedom claim, no auto-locking, no annotation mandate,
no runtime overhead beyond existing primitives, no new trait
system for this alone.

## Acceptance

Attribute recognized on supported types; boundaries check marked
types; nesting verified recursively; unsafe/unverifiable
references rejected; transfer distinguished from sharing; tests
cover nested structs, collections, pointers, channels, `spawn`,
and foreign types.