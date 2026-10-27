# RFC: Opt-In Compiler Checking for `@[guarded_by]` Lock Invariants

Status: RFC, docs only. No `@[guarded_by]`/`@[requires_lock]` in
tree (only unrelated DWARF constants). Builds on V's `lock` /
`rlock` scopes and `sync.Mutex`/`RwMutex`; complements the runtime
race detector rather than replacing it.

## Syntax (illustrative)

```v okfmt
struct Cache {
mut:
	mu      sync.Mutex
	entries map[string]Entry @[guarded_by: 'mu']
}
```

```v okfmt
@[requires_lock: 'mu']
fn (mut c Cache) update_locked(key string, value Entry) {
	c.entries[key] = value
}
```

## Semantics

1. Field invariant: every read/write of a guarded field requires
   the designated lock held. Statically provable violations are
   diagnostics.
2. Method requirement: the body is checked under the assumption
   the lock is held on entry; callers must satisfy it.
3. Lock identity, not name: acquiring a different lock that
   happens to share the name does not satisfy the requirement.
   Nested scopes and multiple instances are distinguished where
   the receiver/lock relation is statically identifiable.
4. Conservative and honest: lexically visible scopes, direct
   accesses, and explicit method requirements first. Aliasing or
   indirect calls the checker cannot follow are reported as
   limitations, never silent proof.

## Scope

Opt-in per declaration; existing code is unaffected unless the
checker is enabled or a declaration opts in. Direct accesses and
method requirements before interprocedural analysis. No
annotation mandate for every shared field.

## Acceptance

Both attributes recognized; direct unguarded accesses and
unsatisfied method calls diagnosed; identity/nesting correct for
supported cases; aliasing gaps reported or explicitly out of
scope; tests cover nested locks, helpers, multiple instances,
and invalid assumptions.