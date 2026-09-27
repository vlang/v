# x.executor

`x.executor` is a small owner-loop executor for V programs.
It lets callers submit short callbacks to the thread or loop that owns a
resource, without exposing scheduler internals or starting a hidden runtime.

The module is intentionally narrow:

- one explicit executor;
- one explicit bounded queue;
- explicit admission and backpressure;
- explicit owner pumping through `run()`, `run_one()` or `drain_pending()`;
- explicit shutdown through `stop()` and `wait()`;
- no GUI, render, audio, network, or scheduler dependency.

`x.executor` is a sibling of `x.async`, not a layer on top of it. This module
does not import `x.async`, and the examples in this directory use only
synthetic local work, `spawn`, channels, `context`, and `time`.

## Why

Some resources must be touched only by one owner thread or loop:

- UI state owned by a main thread;
- render-context state owned by a render loop;
- FFI handles with same-thread requirements;
- single-owner game, plugin, scripting, cache, or test state.

The safe abstraction is not "move scheduler work to another processor". The
safe abstraction is a bounded queue that the owner loop drains deliberately.
If a callback is slow, it still blocks the owner loop while it runs. Applications
remain responsible for keeping callbacks short.

## Quick Start

```v
import x.executor

fn main() {
	mut ex := executor.new(queue_size: 8)!
	updates := chan string{cap: 1}

	ex.try_post(fn [updates] () ! {
		updates <- 'owner mutation ran'
	})!

	ran := ex.run_one()!
	assert ran
	assert (<-updates) == 'owner mutation ran'

	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
}
```

## API

### JobFn

```v ignore
pub type JobFn = fn () !
```

Jobs are ordinary V functions. They receive no hidden context. If a caller needs
to stop waiting for admission, it should use `post_with_context()` or
`post_with_timeout()`. Once accepted, a job is not preemptively killed.

### Construction

```v ignore
mut ex := executor.new(queue_size: 128)!
```

`queue_size` must be positive and is fixed for the executor lifetime. The queue
is bounded; a full queue returns an explicit backpressure error.

`new()` does not capture owner identity. Owner identity exists only while a
thread is actively pumping callbacks through `run()`, `run_one()`, or
`drain_pending()`.

### Submission

```v ignore
ex.try_post(fn () ! {
	// owner-thread work
})!

ex.post_with_context(parent_ctx, fn () ! {
	// accepted only while parent_ctx remains active
})!

ex.post_with_timeout(50 * time.millisecond, fn () ! {
	// accepted only before the timeout expires
})!
```

`try_post()` never waits for queue capacity. If capacity is unavailable, it
returns `executor: queue is full`.

`post_with_context()` and `post_with_timeout()` bound admission only. If the job
is accepted and later waits in the executor queue, context cancellation or
timeout does not cancel that accepted job.
Active owner-thread submissions are accepted while capacity is available. If
they would need to wait for capacity, they fail with
`executor: owner thread cannot wait for queue capacity`.

There is deliberately no unbounded blocking `post()`.

### Owner Pumping

```v ignore
ex.run()!
ran := ex.run_one()!
count := ex.drain_pending(16)!
```

`run()` blocks and drains accepted jobs until shutdown reaches a terminal state.

`run_one()` executes at most one pending job and returns whether a job ran. It is
useful for tests or host loops that already own their own frame/event pump. It
does not drain the whole accepted queue in one call.

`drain_pending(max_jobs)` executes up to `max_jobs` pending jobs and returns the
number of jobs executed. `max_jobs` must be positive. If `max_jobs` is reached,
accepted jobs may remain queued for a later owner pump.

FIFO ordering is guaranteed for jobs submitted by one producer. For concurrent
producers, order is the order in which submissions are accepted by the executor,
not wall-clock order.

### Shutdown

```v ignore
ex.stop()
// If this executor is driven by run_one() or drain_pending(), pump once more.
assert !ex.run_one()!
ex.wait()!
```

`stop()` is idempotent and non-blocking. It closes admission, wakes callers that
are waiting to submit, and lets already accepted jobs drain. When the executor is
driven by `run_one()` or `drain_pending()`, the owner loop should pump once more
after `stop()` so the executor can observe the closed admission state and publish
its terminal result before `wait()`.

`wait()` returns only after the executor has reached a terminal state. Calling
`wait()` before any owner pump can reach a terminal state returns a stable error
instead of blocking forever. That precondition error, and the owner-callback
precondition error, do not consume the one valid wait. A valid `wait()` remains
one-shot, and a second valid call returns a stable error.

### Job Errors

The first job error closes admission and is stored once. `run()` keeps pumping
until already accepted jobs have drained, then returns the first job error.
Manual pump APIs are bounded: `run_one()` can return a job error after one job,
and `drain_pending(max_jobs)` can return the stored error after reaching
`max_jobs` even when accepted jobs remain queued. Callers that use manual pumps
must keep pumping until the terminal state is reached, or use `run()` when they
want automatic draining. `wait()` returns the stored first job error after the
terminal state is reached. Job errors are not silently discarded.

### Synchronous Calls

```v ignore
ex.run_sync(fn () ! {
	// owner-thread work; caller waits for completion
})!
```

`run_sync()` from a foreign thread submits a wrapper job and waits for that job
to complete. Its job error is returned to the caller.

`run_sync()` from the active owner thread runs inline to avoid deadlock. This
inline execution still requires open admission and is outside executor FIFO
ordering. If the inline call returns an error, that error becomes the executor's
first job error even when the outer callback catches it.

Context-bound or timeout-bound synchronous calls are not part of this first API.

### Owner Checks

```v ignore
if ex.is_owner_thread() {
	// currently pumping callbacks on the owner thread
}

ex.assert_owner_thread()!
```

`is_owner_thread()` is true only while the current thread is actively pumping
callbacks for that executor. Owner identity is cleared before the pump returns.

## Safety Notes

`x.executor` is about owner-affinity control flow, not sandboxing.

- It does not recover panics.
- It does not kill a running callback.
- It does not make slow owner callbacks safe for GUI or render loops.
- It does not provide priorities, work stealing, promises, or a scheduler.
- It does not depend on real GUI, OpenGL, audio, or network resources.

All public module-owned errors use the `executor:` prefix. User job errors are
returned unchanged.

## Examples

Small runnable examples live in `vlib/x/executor/examples/`:

- `basic_post.v`: submit one owner-loop mutation and pump it with `run_one()`.
- `bounded_admission.v`: show queue backpressure and timeout-bounded admission.
- `owner_loop_pump.v`: drain a synthetic frame loop with a per-frame job limit.
- `run_sync.v`: wait for owner-loop execution from a foreign caller.
- `shutdown.v`: stop from an owner callback and reject later submissions.
- `synthetic_render_upload.v`: synthetic render-resource mutation.
- `synthetic_ffi_owner_state.v`: synthetic same-thread FFI handle mutation.

Each example is local and synthetic. None starts a server, opens a window,
touches graphics/audio APIs, or imports `x.async`.

## Tests

The targeted test suite lives next to the module:

```sh
v test vlib/x/executor
v -prod test vlib/x/executor
```

For guarded local validation, prefer:

```sh
sh vlib/x/executor/tools/validate.sh
```

The validation script runs the dependency guard, formatting verification,
examples, dev tests, and `-prod` tests serially with isolated `VTMP` and
`VCACHE`.

## Benchmarks

Small local benchmarks live in `vlib/x/executor/benchmarks/`:

```sh
sh vlib/x/executor/benchmarks/run_executor_benchmark.sh
```

Benchmarks are observation tools, not correctness tests or portable performance
claims. Defaults are intentionally modest and environment overrides are clamped.
