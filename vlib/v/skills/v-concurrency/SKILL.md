---
name: v-concurrency
description: V's concurrency primitives - spawn and its thread handles, shared values with lock and rlock, the sync module (channels, Mutex, RwMutex, WaitGroup, Once, select), atomics, and parallel.amap for parallel map and run. Use when writing code with threads, channels, locks, shared mutable state, a WaitGroup, or a race, and when reviewing V code that touches them. Covers how to wait for spawned work and why a detached spawn is a bug. Does not cover the mutability rules a lock protects (see v-lang), the build and test loop (see v-workflow), web servers (see v-veb), scripting a task in V (see v-scripts), or the wider command surface (see v-tools).
license: MIT
---

# Concurrency in V

V has three levels: `parallel.amap` when you have a collection to process, `spawn`
and its thread handles when you have a fixed set of tasks, and channels, `shared`
values and locks when the tasks have to talk to each other.

## Resource Routing

- `references/CHANNELS.md` - Read when passing work or results between threads, or
  when a producer must not block.
- `references/PATTERNS.md` - Read for worker pools, fan-out, lazy initialisation,
  and the shapes that deadlock.

## Reach for these first

| Situation | Use |
| --- | --- |
| Process a slice, collect results | `parallel.amap(items, worker)` |
| Process a slice, discard results | `parallel.run(items, worker)` |
| A fixed number of independent tasks | `spawn f()`, then `.wait()` on the threads |
| Tasks that return nothing to collect | `sync.WaitGroup` |
| Hand work to a pool | a channel plus a fixed set of consumers |
| Share mutable state | a `shared` value with `lock` / `rlock`, or a `sync.Mutex` |
| Initialise once, lazily | `sync.new_once()` |
| Wait for any of several channels | `select` |

Reaching for `spawn` when `amap` would do is the most common mistake. `spawn`
makes you wait for the work; `amap` does it for you.

## parallel: do not hand-roll this

```v ignore
import arrays.parallel

fn main() {
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

This is worth preferring over a hand-written loop of threads even when the loop is
short: there is no way to forget to wait.

## spawn returns a thread: wait for it

`spawn` starts a thread and returns a handle at once. Nothing waits for the thread
unless you do, so a program that ends, or a function that returns, can leave it
mid-write. `wait()` on the handle joins it and returns the function's result, and
an array of handles waits for all of them:

```v ignore
fn square(n int) int {
	return n * n
}

fn main() {
	mut threads := []thread int{}
	for n in [1, 2, 3] {
		threads << spawn square(n)
	}
	assert threads.wait() == [1, 4, 9]

	t := spawn square(4)
	assert t.wait() == 16
}
```

Pass what the thread needs as arguments. A closure only sees the variables it
lists in `fn [x] () {`, and gets a copy of each.

A `sync.WaitGroup` fits tasks that return nothing:

```v ignore
import sync

fn work(mut wg sync.WaitGroup, id int) {
	defer {
		wg.done()
	}
	println('task ${id}')
}

fn main() {
	mut wg := sync.new_waitgroup()
	for id in 0 .. 3 {
		wg.add(1)
		spawn work(mut wg, id)
	}
	wg.wait()
}
```

The rules that matter:

- **`wg.add(1)` before the spawn, `wg.done()` inside it**, in a `defer` so every
  return counts down. A `done()` without a matching `add()` panics.
- **All `add` / `wg.go()` calls before `wg.wait()`.** Adding after waiting is a
  race.
- **Wait, or document why not.** A detached spawn is a bug unless it is genuinely
  fire-and-forget.
- **Never let a spawned function panic.** It takes the whole process down.

`wg.go(f)` does the `add(1)` and starts the thread, which calls `done()` when `f`
returns.

## Sharing state needs a lock

V's own model is a `shared` value. For a shared struct, every access goes
through `lock` (read and write) or `rlock` (read only), and the compiler rejects
an access outside one. A shared array or map also locks each single operation by
itself, like `a[i]++`; a sequence of operations that must stay consistent still
needs a `lock` block around it.

```v ignore
struct Counter {
mut:
	total int
}

fn add(shared c Counter, n int) {
	lock c {
		c.total += n
	}
}

fn main() {
	shared c := Counter{}
	mut threads := []thread{}
	for _ in 0 .. 4 {
		threads << spawn add(shared c, 1)
	}
	threads.wait()
	total := rlock c {
		c.total
	}
	assert total == 4
}
```

A `sync.Mutex` is the library alternative. `lock` / `rlock` blocks only work on
`shared` values; a mutex is taken with its methods, and it must be initialised, so
create it with `sync.new_mutex()` or call `init()`:

```v ignore
import sync

struct Stats {
mut:
	mu    &sync.Mutex = sync.new_mutex()
	total int
}

fn (mut s Stats) add(n int) {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	s.total += n
}

fn main() {
	mut s := &Stats{}
	s.add(2)
	assert s.total == 2
}
```

Hold a lock for the shortest possible span. Do not hold one across a channel
operation, a `spawn`, or anything that can block. Two locks taken in different
orders by two threads deadlock, so pick one order and hold to it.

`RwMutex` is for a structure read far more often than it is written. If the write
rate is not clearly lower, `Mutex` is simpler and fast enough.

## Channels are the typed default

A channel moves one value type between threads, with an optional buffer:

```v ignore
fn worker(jobs chan int, results chan int) {
	for {
		job := <-jobs or { break } // ends once `jobs` is closed and drained
		results <- job * 2
	}
	results.close()
}

fn main() {
	jobs := chan int{cap: 3}
	results := chan int{cap: 3}
	spawn worker(jobs, results)
	for i in 1 .. 4 {
		jobs <- i
	}
	jobs.close()
	mut sum := 0
	for {
		r := <-results or { break }
		sum += r
	}
	assert sum == 12
}
```

`cap` sets the buffer, which lets a producer get ahead instead of blocking on
every send. It is not part of the type: `chan int{}` and `chan int{cap: 16}` are
both `chan int`. Close with the method, `ch.close()`; a `for x in ch` loop does
not compile, so receive with `or { break }`.

See `references/CHANNELS.md` for closing, `select` and timeouts.

## Atomics for counters

For a single value — a counter, a flag — a lock is more than needed:

```v ignore
import sync.stdatomic

fn main() {
	mut hits := stdatomic.new_atomic(u64(0))
	hits.add(1)
	assert hits.load() == 1
}
```

The type is fixed when the atomic is created, and `load`, `store` and `add` need
`mut`. There are also plain functions taking a pointer
(`stdatomic.add_u64(&x, 1)`) for the cases where an `AtomicVal` is awkward.

Use atomics for one or two values. The moment you need two of them to change
together there is no transaction, and you want a lock:

```v ignore
// Two values that must stay consistent with each other.
lock c {
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

- **Mutability**: see [v-lang](../v-lang/SKILL.md) for `mut` parameters and
  receivers, and for what a closure capture copies.
- **The build loop**: see [v-workflow](../v-workflow/SKILL.md) for `-race`,
  `-prod` and the other flags.
- **Web servers**: see [v-veb](../v-veb/SKILL.md) when the shared state is a
  request counter and `veb` already gives you the locking idiom.
