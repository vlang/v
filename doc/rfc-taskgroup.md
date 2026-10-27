# RFC: Structured Concurrency Through `sync.TaskGroup`

Status: RFC, docs only. No `TaskGroup` in tree. Builds on
`sync.WaitGroup` (`vlib/sync/waitgroup.c.v`), `spawn` + `chan`,
and `pool.PoolProcessor` indexed-order results.

## API (illustrative, V-idiomatic)

```v okfmt
import sync

mut group := sync.new_task_group[Response]()
ctx := group.context()

for url in urls {
	group.spawn(fn () !Response {
		return fetch_one(ctx, url)!
	})!
}

return group.wait()!
```

`spawn` takes a closure, matching `spawn fn ...` / `wg.go(...)`
usage, not Go-style `spawn(f, args)`.

## Semantics

1. Lifetime: `wait()` joins all children before reporting success.
   Spawning after `wait` starts is an error.
2. Cancellation is cooperative via `TaskContext`
   (`check_cancelled()!`); no forced preemption.
3. Errors: first error cancels siblings, group waits for children,
   original error is returned; aggregation policy (`!T` vs `[]IError`)
   is documented and deterministic.
4. Results preserve submission order (as `pool` does).
5. Bounded concurrency (`max_concurrency`) is deferred.

Child panics, reuse-after-`wait`, and `spawn`-after-`wait` are
specified as errors. No `async`/`await`, no replacement for
`spawn`/`chan`, no mandatory adoption.

## Acceptance

Tests: early failure, cancellation, normal completion, result
ordering, repeated group use, misuse panics. Unix/Windows where
threads exist; js/wasm excluded where `spawn` is unavailable.