# RFC: Opt-In, Compiler-Verified Function Contracts

Status: RFC, docs only. Extends existing `@[noalloc]` with `@[nosuspend]`.

## Background

`@[noalloc]` is implemented (`vlib/v/types/checker_noalloc.v`,
`doc/noalloc.md`, `vlib/v/compiler_tests/noalloc_contract_test.v`).
It checks V bodies transitively, rejects indirect/interface calls
conservatively, treats unannotated `C.*` as allocating, accepts
explicit `@[noalloc]` on `C.*` as a trusted contract, and reports
call chains. This RFC keeps that behavior and adds the missing half.

Note: `fn parse_integer(s string) !i64 { return s.int() }` cannot
satisfy `@[noalloc]` today (`string.int` allocates, `!T` boxes
`IError`). Examples below use non-allocating bodies.

## Proposal

Add `@[nosuspend]` with compiler-enforced, zero-runtime-cost checks:

```v okfmt
@[nosuspend]
fn update_state(mut state State) {
}
```

## Semantics

`@[nosuspend]` rejects, directly or transitively:

- `spawn`, channel send/receive (`<-`, `chan <-`), `select`.
- `lock`/`rlock`, `sync.Mutex`, `WaitGroup.wait`, `Barrier`,
  `Semaphore`, `Cond.wait`, `time.sleep`, blocking OS/net IO via `C`.
- Indirect calls, interface dispatch, bound method values, and
  unannotated `C.*` (fail closed, as `@[noalloc]` does).

Trusted `@[nosuspend]` on a `C.*` declaration is allowed and is a
programmer-supplied assumption, not a proof.

## Verification rules

- Checked at compile time, including generic instantiations.
- Diagnostics carry the call chain, mirroring `@[noalloc]`.
- No new effect system, no inference, no change for unannotated fns.
- FastC backend rejects the contract until it can check it.

## Acceptance criteria

- Direct and transitive violations are compile errors with chains.
- Generics, fn pointers, interface dispatch, foreign calls covered.
- Tests for all supported backends; plain fns unaffected.
