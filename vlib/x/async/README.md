# x.async

`x.async` is a small structured-concurrency layer for V programs.
It composes the primitives that already exist in V: `spawn`,
`sync.WaitGroup`, channels, `context`, and `time`.

The module does not add a scheduler, does not change the language, and does not
implement `async/await`. It must not become a hidden runtime, event loop,
external dependency, or competing concurrency model. Its purpose is narrower:
make common concurrent control-flow easier to write, stop, test, and review.

Safety is the primary design constraint. Public APIs should be defensive, shared
state must be synchronized explicitly, errors must not be lost silently,
channels must not leave workers blocked without a consumer, and behavior must
remain robust in `-prod`.

The current API intentionally centers on five building blocks, plus small
context and function-type helpers:

- `Group`: run related jobs, wait for them, return the first error, and cancel
  sibling jobs cooperatively.
- `Task[T]`: run one value-returning job and wait for its result once.
- `Pool`: run accepted jobs with a fixed concurrency limit and bounded backlog.
- `every`: run a periodic job without overlapping iterations.
- `with_timeout` / `with_timeout_context`: run one job with a bounded deadline.

Ticker objects and server-specific helpers are not part of this API.

## Why

Raw `spawn` plus `sync.WaitGroup` is explicit and fast, but real applications
quickly need the same extra wiring around it:

- one parent cancellation signal shared by all jobs;
- one place to wait for all accepted jobs;
- predictable first-error propagation;
- value-returning tasks without hand-written result channels;
- bounded worker pools for request bursts or CPU-heavy server work;
- periodic cleanup loops that stop through context cancellation;
- fail-fast sibling cancellation;
- timeouts that do not require hand-written result channels in every caller.

`x.async` keeps those mechanics small and visible. A job is still a normal V
function. Cancellation is still exposed as a normal `context.Context`. Waiting
for groups is still backed by `sync.WaitGroup`; value and timeout handoff uses
small bounded channels. Pools use fixed worker slots plus a bounded accepted-job
backlog.

Internally, `x.async` owns the derived cancellation context used by its jobs.
That internal context does not schedule work; it only keeps cancellation and
timeout propagation deterministic for this module.

## Quick Start

```v
import context
import time
import x.async

fn main() {
	parent := context.background()
	mut group := async.new_group(parent)

	group.go(fn (mut ctx context.Context) ! {
		// A long-running job should observe ctx.done().
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			50 * time.millisecond {
				return
			}
		}
	})!

	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('stop the group')
	})!

	group.wait() or { eprintln('group failed: ${err.msg()}') }
}
```

## API

### Context helpers

`async.background()` returns `context.background()`.

`async.with_cancel()` returns a cancellable context derived from background:

```v ignore
ctx, cancel := async.with_cancel()
defer {
	cancel()
}
```

These helpers are convenience wrappers. Code that already has a
`context.Context` should pass it directly to `new_group()` or
`with_timeout_context()`.

### JobFn

```v ignore
pub type JobFn = fn (mut context.Context) !
pub type TaskFn[T] = fn (mut context.Context) !T
```

Every `x.async` job receives a context. This is a deliberate choice:
cancellation is part of the function signature instead of a hidden global side
channel.

## Cooperative Cancellation

Cancellation in `x.async` is cooperative. The module closes the shared context's
`done()` channel, but it does not interrupt, kill, or preempt a running thread.

A job that can run for a long time should check the context:

```v ignore
fn worker(mut ctx context.Context) ! {
	done := ctx.done()
	for {
		select {
			_ := <-done {
				return ctx.err()
			}
			10 * time.millisecond {
				// Do a small unit of work.
			}
		}
	}
}
```

If a job ignores `ctx.done()`, `Group.wait()` will still wait for it to return.
`Pool.close()` and `every()` also wait for running non-cooperative jobs to return.
`with_timeout()` returns when the timeout expires, but the ignored job may keep
running until it finishes naturally.

## Group

`Group` coordinates related jobs.

```v ignore
parent := context.background()
mut group := async.new_group(parent)

group.go(fn (mut ctx context.Context) ! {
	serve_http(mut ctx)!
})!

group.go(fn (mut ctx context.Context) ! {
	cleanup_loop(mut ctx)!
})!

group.wait()!
```

Guarantees:

- `go()` accepts a job only before `wait()` starts.
- `wait()` blocks until every accepted job has finished.
- `wait()` is safe when no job was accepted.
- the first job error is stored once and returned by `wait()`;
- the first job error cancels the shared context so cooperative sibling jobs can
  stop early;
- calling `go()` after `wait()` starts returns an error instead of racing
  `sync.WaitGroup.add()` against `sync.WaitGroup.wait()`.

`wait()` is a one-shot operation. Calling it a second time returns an error. This
keeps the lifecycle simple: create a group, submit jobs, wait once, then discard
the group.

## Task

`Task[T]` represents one concurrent computation that returns either a value or
an error.

```v ignore
mut task := async.run[string](fn (mut ctx context.Context) !string {
	_ = ctx
	return load_config()!
})!

config := task.wait()!
```

Use `run_with_context()` when the task should follow an existing parent context:

```v ignore
parent := context.background()
mut task := async.run_with_context[int](parent, fn (mut ctx context.Context) !int {
	done := ctx.done()
	select {
		_ := <-done {
			return ctx.err()
		}
		50 * time.millisecond {
			return 42
		}
	}
})!

value := task.wait()!
```

Guarantees:

- `run()` and `run_with_context()` reject a nil task function.
- `wait()` blocks until the task publishes its single result.
- `wait()` is one-shot; calling it a second time returns an error.
- the result channel is bounded with capacity 1, so a finished task can publish
  its value or error before the owner calls `wait()`.
- parent cancellation is cooperative; the task function must observe
  `ctx.done()` and return.

`Task[T]` does not expose a kill operation. A task that ignores cancellation may
continue until its function returns naturally.

## Pool

`Pool` limits how many jobs run concurrently and how many pending jobs can wait
in memory.

```v ignore
mut pool := async.new_pool(workers: 4, queue_size: 128)!
defer {
	pool.close() or {}
}

pool.try_submit(fn (mut ctx context.Context) ! {
	process_message(mut ctx)!
})!
```

Use `new_pool_with_context()` when the pool should follow an existing parent
context:

```v ignore
parent := context.background()
mut pool := async.new_pool_with_context(parent, workers: 2, queue_size: 32)!
```

Backpressure is explicit. `try_submit()` never waits for backlog space:

- if the pool is open and the backlog has capacity, the job is accepted;
- if the backlog is full, it returns `async: pool queue is full`;
- if the pool is closed or already waiting, it returns `async: pool is closed`;
- if the job function is nil, it returns `async: job function is nil`.

Lifecycle:

- `workers` and `queue_size` must be positive and are fixed at creation.
- at most `workers` accepted jobs execute user code at the same time.
- at most `workers + queue_size` jobs can be accepted and unfinished at once.
- `close()` stops accepting new jobs, waits for accepted jobs, and returns the
  first job error if any.
- `wait()` has the same behavior as `close()`; both are one-shot.
- a job error is stored once and returned by `close()` / `wait()`.
- `Pool` is not fail-fast: a job error does not kill running jobs, does not
  cancel already accepted sibling jobs, and does not discard accepted backlog.
- parent cancellation is cooperative; jobs must observe `ctx.done()` and return.

`Pool` does not kill running jobs. A non-cooperative job can delay `close()` until
it returns naturally. `close()` and `wait()` drain accepted jobs before returning
the first stored error. User code never runs while the pool lifecycle mutex is
held.

## Periodic Jobs

`every()` runs a job repeatedly until its context is canceled or the job returns
an error.

```v ignore
ctx, cancel := async.with_cancel()
defer {
	cancel()
}

async.every(ctx, 5 * time.second, fn (mut ctx context.Context) ! {
	cleanup_stale_clients(mut ctx)!
})!
```

Guarantees:

- `interval` must be positive; zero or negative intervals return
  `async: interval must be positive`.
- `every()` is blocking and does not start a hidden background loop.
- the first iteration runs after one interval.
- iterations never overlap; a slow iteration delays the next one.
- a job error stops the loop and is returned unchanged.
- context cancellation stops the loop and returns the context error.

`every()` is not a scheduler and does not expose a ticker handle. If a running
job ignores cancellation, `every()` cannot return until that job returns
naturally.

## Timeout

`with_timeout()` runs one job with a background context and a timeout:

```v ignore
async.with_timeout(2 * time.second, fn (mut ctx context.Context) ! {
	done := ctx.done()
	select {
		_ := <-done {
			return ctx.err()
		}
		100 * time.millisecond {
			return
		}
	}
})!
```

`with_timeout_context()` derives the timeout from an existing parent context:

```v ignore
parent := context.background()
async.with_timeout_context(parent, 250 * time.millisecond, fn (mut ctx context.Context) ! {
	fetch_or_compute(mut ctx)!
})!
```

Error behavior:

- if the job returns first, the job error is returned unchanged;
- if the timeout expires first, the public error is `async: timeout`;
- if the parent context is canceled first, including when the parent's own
  deadline expires first, the parent context error is returned;
- a job that observes the local `x.async` timeout by returning
  `context deadline exceeded` is normalized to `async: timeout`.

The result channel used internally is buffered so the spawned job can finish and
publish its result even if the caller has already returned on timeout.

## Safety And Security Notes

`x.async` is about control-flow safety, not sandboxing.

- It does not recover panics from spawned jobs.
- It does not kill work that ignores cancellation.
- It does not validate user input, paths, network data, or files.
- `Task[T].wait()` is one-shot so result ownership is unambiguous.
- `Pool.try_submit()` is non-blocking and returns a stable error when the
  accepted-job backlog is full instead of hiding backpressure.
- `Pool.close()` drains accepted jobs before returning and reports the first
  job error.
- `every()` is blocking and serial, so periodic iterations cannot overlap.
- It uses `sync.Mutex` to protect mutable lifecycle/result state in `Group`,
  `Task[T]`, and `Pool`.
- It keeps `sync.WaitGroup.add()` and `sync.WaitGroup.wait()` separated by a
  lifecycle mutex for `Group` and `Pool` where accepted work can race with
  shutdown.

When jobs process untrusted input, the application must still apply the normal
validation and resource limits for that domain.

## Limits

This milestone does not include:

- blocking pool submission;
- ticker objects or detached periodic handles;
- multi-consumer futures or promise chaining;
- panic recovery;
- scheduler changes;
- `goroutines`, `coroutines`, `x.atomics`, or `sync.stdatomic`.

The current module deliberately stays small. Its job is to structure V's
existing `spawn`, `sync`, channels, `context`, and `time` primitives, not to
replace them with a new runtime.

Possible future additions, if accepted by the project maintainers and backed by
tests, could still fit the current philosophy:

- blocking or timeout-based pool submission, built on existing channels;
- detached periodic handles with explicit `stop()` / `wait()` lifecycle;
- helpers that combine `Group`, `Pool`, `Task[T]`, and timeout for server-style
  code;
- more examples and integration tests for other V modules that already use
  concurrency;
- careful rewrites of existing V modules that need concurrency, if maintainers
  decide `x.async` makes their lifecycle, cancellation, or backpressure simpler
  and safer without breaking compatibility;
- optional error collection helpers, as long as first-error behavior stays
  simple and documented;
- more benchmarks and stress tests that remain bounded and reproducible.

Other ideas would require a separate design because they move beyond this
module's current scope:

- a scheduler or event loop;
- async/await syntax or compiler changes;
- green threads or coroutine/goroutine integration;
- preemptive cancellation or killing running jobs;
- panic recovery across spawned work;
- low-level atomic or lock-free rewrites of the public API.

Those larger features may be useful someday, but they should not be hidden
inside `x.async`. They need separate review so this module can remain minimal,
safe, and easy to reason about.

## Examples

Small runnable examples live in `vlib/x/async/examples/`:

- `basic_group.v`: first error propagation and cooperative sibling cancellation.
- `basic_task.v`: run one value-returning task and consume it with `wait()`.
- `worker_pool.v`: fixed concurrency with explicit `try_submit()` backpressure.
- `periodic.v`: a blocking `every()` loop stopped by context cancellation.
- `timeout.v`: run one cooperative job with a bounded timeout.
- `net_http/`: synthetic `net.http` request/response work through `Pool`.
- `net_websocket/`: in-memory `websocket.Message` processing through `Group`;
  this is not an end-to-end websocket server test.
- `mcp/`: in-memory MCP request/response dispatch through `Task[T]`.
- `veb/`: in-memory `veb.Context` response lifecycle through `Task[T]`.

Each example focuses on one API and uses only local, in-memory work.

## Tests

The targeted test suite lives next to the module:

```sh
v test vlib/x/async
v -prod test vlib/x/async
```

For automated validation, prefer the guarded script:

```sh
sh vlib/x/async/tools/validate.sh
```

It runs formatting verification, dev tests, and `-prod` tests serially with a
fresh `VTMP` and `VCACHE` for that run. Do not run two V validation runners
against the same checkout/cache unless each runner has isolated `VTMP`,
`VCACHE`, and output paths. That isolation protects the validation harness from
V build artefact collisions; it is separate from the runtime guarantees of
`x.async`.

The tests cover successful groups, first-error propagation, empty waits,
rejected submissions after `wait()`, one-shot task waits, task values and
errors, pool worker limits, pool queue backpressure, pool close/wait behavior,
periodic execution, periodic cancellation, non-overlapping periodic iterations,
cooperative cancellation, timeout errors, parent cancellation, nil job rejection,
parent deadline preservation, invalid intervals, zero/already-expired timeouts,
stress cases, and jobs that ignore cancellation. Internal tests also verify
that the derived `AsyncContext` closes `done()` and propagates parent
cancellation.

Module-oriented integration tests live in `vlib/x/async/tests/`. They are
synthetic and local: no external service, fixed port, path dependency, or fragile
server shutdown is required. They currently cover `net.http`, `net.websocket`,
`mcp`, and `veb` boundaries.

## Benchmarks

Small local benchmarks live in `vlib/x/async/benchmarks/`:

```sh
sh vlib/x/async/benchmarks/run_async_benchmark.sh
```

The script uses the local `./v`, runs serially, and isolates `VTMP`, `VCACHE`,
and the benchmark executable output. It measures short default runs for
`Group`, `Task[T]`, `Pool`, `with_timeout()`, and `every()`.

The default sizes are intentionally modest. They can be changed with
`XASYNC_BENCH_GROUP_ROUNDS`, `XASYNC_BENCH_GROUP_JOBS`,
`XASYNC_BENCH_TASK_ROUNDS`, `XASYNC_BENCH_POOL_JOBS`,
`XASYNC_BENCH_POOL_WORKERS`, `XASYNC_BENCH_TIMEOUT_ROUNDS`,
`XASYNC_BENCH_EVERY_ITERATIONS`, and `XASYNC_BENCH_EVERY_INTERVAL_MS`.
Benchmark output is local diagnostic data, not a portable performance claim.
