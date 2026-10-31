---
name: v-concurrency
description: V's concurrency primitives - spawn, the sync module (channels, Mutex, RwMutex, WaitGroup, Once, Pool, select), and parallel.amap for parallel map and run. Use when writing code with goroutines, threads, channels, locks, shared mutable state, a WaitGroup, or a race, and when reviewing V code that touches them. Covers how to wait for spawned work and why a detached spawn is a bug. Does not cover the mutability rules a lock protects (see v-lang), the build and test loop (see v-workflow), web servers (see v-veb), scripting a task in V (see v-scripts), or the wider command surface (see v-tools).
license: MIT
---

# Concurrency in V

V has three levels: `parallel.amap` when you have a collection to process, `spawn`
plus a `WaitGroup` when you have a fixed set of tasks, and `sync` channels and
locks when the tasks have to talk to each other.

## Resource Routing

- `references/CHANNELS.md` - Read when passing work or results between threads, or
  when a producer must not block.
- `references/PATTERNS.md` - Read for worker pools, fan-out, a single-writer
  cache, and the shapes that deadlock.

## Reach for these first

| Situation | Use |
| --- | --- |
| Process a slice, collect results | `parallel.amap(items, worker)` |
| Process a slice, discard results | `parallel.run(items, worker)` |
| A fixed number of independent tasks | `spawn f()` plus `sync.WaitGroup` |
| Hand work to a pool | a channel plus a fixed set of consumers |
| Share mutable state | `sync.Mutex`, and keep the critical section small |
| Initialise once, lazily | `sync.Once` |
| Reuse objects | `sync.Pool` |
| Wait for any of several channels | `select` |

Reaching for `spawn` when `amap` would do is the most common mistake. `spawn`
makes you wait for the work; `amap` does it for you.

## parallel: do not hand-roll this

```v ignore
import arrays.parallel

fn test_it_doubles_every_number() {
	items := [1, 2, 3, 4]
	got := parallel.amap(items, fn (n int) int {
		return n * 2
	})
	assert got == [2, 4, 6, 8]
}
```

`amap` preserves order and returns a slice; `run` discards results. Both take an
optional `Params` whose `workers` defaults to `runtime.nr_jobs()`, which reads
`VJOBS`.

This is worth preferring over a loop with a `WaitGroup` even when the loop is
short: there is no way to forget to wait.

## spawn needs a WaitGroup

`spawn` starts a thread and returns immediately. Nothing waits for it, so a
program that ends, or a function that returns, can leave it mid-write.

```v ignore
import sync

fn fetch_all(urls []string) []string {
	mut wg := sync.new_waitgroup()
	mut results := []string{len: urls.len}
	for url, i in urls {
		wg.add(1)
		spawn fn () {
			results[i] = fetch(url)
			wg.done()
		}
	}
	wg.wait()
	return results
}
```

Every rule that matters here is easy to get wrong:

- **`wg.add(1)` before the spawn, `wg.done()` inside it.** A `done()` without a
  matching `add()` panics.
- **All `wg.go()` / `spawn` before `wg.wait()`.** The doc comment says so
  explicitly: adding after waiting is a race.
- **`wait()` or nothing.** A detached spawn is a bug unless it is genuinely
  fire-and-forget, and V is not a language where that is the default answer.
- **Never let a spawned function panic.** It takes the whole process down.

`sync.WaitGroup.go(f)` bundles the `add` and the thread start, which removes two
of the four mistakes:

```v ignore
mut wg := sync.new_waitgroup()
wg.go(fn () { work() })
wg.wait()
```

## Sharing state needs a lock

A struct shared between threads needs a `Mutex`, and the rule is to hold it for
the shortest possible span:

```v ignore
import sync

struct Counter {
mut:
	mu    sync.Mutex
	total int
}

fn (mut c Counter) add(n int) {
	lock c.mu {
		c.total += n
	}
}
```

Do not hold a lock across a channel operation, a `spawn`, or anything that can
block. Two locks taken in different orders by two threads deadlock, so pick one
order and hold to it.

`RwMutex` is for a structure read far more often than it is written. If the write
rate is not clearly lower, `Mutex` is simpler and fast enough.

## Channels are the typed default

A channel moves one value type between threads and gives you a buffer:

```v ignore
mut jobs := chan Job{cap: 16}
mut results := chan Result{cap: 16}

spawn fn () {
	for job in jobs {
		results <- run(job)
	}
	close(results)
}

for r in results {
	println(r)
}
```

Note `chan Job{cap: 16}` — the capacity is part of the type, and a buffered channel
lets a producer get ahead instead of blocking on every send.

See `references/CHANNELS.md` for closing, `select` and timeouts.

## Atomics for counters

For a single value — a counter, a flag, a pointer — a lock is more than needed:

```v ignore
import sync.stdatomic

mut hits := stdatomic.new_atomic(0u64)
hits.add(1u64)
total := hits.load()
```

The type is fixed when the atomic is created, and `load`, `store` and `add` need
`mut`. There are also plain functions taking a pointer (`stdatomic.add_u64(&x, 1)`)
for the cases where an `AtomicVal` field is awkward.

Use atomics for one or two values. The moment you need two of them to change
together there is no transaction, and you want a `Mutex`:

```v ignore
// Two values that must stay consistent with each other.
lock c.mu {
    c.total += n
    c.checks += 1
}
```

## Validation

Concurrency bugs are timing bugs, and reasoning about them is unreliable. Run
under the race detector and read what it reports:

```bash
v -race run main.v
v -race test dir/
```

A clean race run does not prove the absence of races, but a reported one is
real. Never disable a report without explaining it. See `v-workflow` for the rest
of the loop.

## Related Skills

- **Mutability**: see [v-lang](../v-lang/SKILL.md) for `mut` receivers and the
  value-versus-pointer difference a lock protects.
- **The build loop**: see [v-workflow](../v-workflow/SKILL.md) for `-race`,
  `-prod` and the other flags.
- **Web servers**: see [v-veb](../v-veb/SKILL.md) when the shared state is a
  request counter and `veb` already gives you the locking idiom.