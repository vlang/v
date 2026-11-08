# Concurrency patterns

> The primitives are in the parent skill. This reference covers: the shapes that
> work, and the ones that deadlock.

## Choose the simplest shape that fits

```
A slice to process?          -> parallel.amap
A fixed set of tasks?        -> spawn, then wait() on the threads
Tasks need to talk?          -> channel
Shared mutable state?        -> shared + lock, held as briefly as possible
One value, changed often?    -> AtomicVal
Fire and forget, truly?      -> nothing waits; make it rare and obvious
```

Most "I need concurrency here" is the first row. Reaching for `spawn` when
`amap` fits means collecting the threads yourself and the chance to forget the
`wait()`.

## parallel.amap

```v
import arrays
import arrays.parallel

fn main() {
	items := [1, 2, 3, 4, 5]
	squares := parallel.amap(items, fn (n int) int {
		return n * n
	})
	assert arrays.sum(squares)! == 55
}
```

`amap` keeps the input order in the output. If there is nothing to collect,
`run` takes a `fn (T)` and returns nothing.

For a bounded worker count:

```v ignore
got := parallel.amap(items, worker, parallel.Params{
	workers: 4
})
```

## Threads that return their result

The single-writer pattern without a lock: each thread returns its own result, and
only the thread that waits assembles them.

```v
fn fetch(url string) string {
	return 'fetched ${url}'
}

fn fetch_all(urls []string) []string {
	mut threads := []thread string{}
	for url in urls {
		threads << spawn fetch(url)
	}
	return threads.wait() // in the order the threads were started
}

fn main() {
	assert fetch_all(['a', 'b']) == ['fetched a', 'fetched b']
}
```

No thread touches another's memory, so there is nothing to lock and nothing to
race. Prefer this to threads writing into a shared array.

## WaitGroup: when nothing comes back

```v
import sync

fn work(mut wg sync.WaitGroup, id int) {
	defer {
		wg.done() // defer, so every return counts down
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

Without the `defer`, an early return inside the thread leaves the count high and
`wait()` hangs forever. `wg.go(f)` does the `add(1)` for a `fn ()` and calls
`done()` when it returns.

## Once for lazy initialisation

A `sync.Once` must come from `sync.new_once()`. The closure captures a pointer,
because a captured value would be a copy:

```v
import sync

struct Config {
mut:
	loaded bool
}

fn main() {
	mut once := sync.new_once()
	mut cfg := &Config{}
	for _ in 0 .. 3 {
		once.do(fn [mut cfg] () {
			cfg.loaded = true
		})
	}
	assert cfg.loaded
}
```

`once.do` runs its function exactly once however many threads arrive, and
returns only after it has completed. `once` protects the *setup*; data that is
still written afterwards needs its own lock.

## Lock discipline

The three rules that keep a concurrent program readable:

1. **One lock per piece of state.** A lock protects a named thing, not a function.
2. **One global order.** If two locks are ever needed together, take them in the
   same order in every function.
3. **No I/O, no `spawn`, no channel operation inside a lock.** Copy what you need,
   release, then do the slow thing.

```v ignore
// Wrong: the send can block while holding the lock.
lock state {
	results <- compute(state.x)
}

// Right: copy under the lock, release, then send.
x := rlock state {
	state.x
}
results <- compute(x)
```

## Patterns that deadlock

| Shape | Why |
| --- | --- |
| Send before starting the consumer, beyond the buffer | producer blocks first |
| Two locks taken in different orders | neither thread can proceed |
| A lock held across a channel operation | the consumer needs the same lock |
| `wg.add` after `wg.wait` | the count changes while it is being waited on |
| `wg.done` without a matching `add` | panics on an underflow |
| A receive loop on a channel nobody closes | the consumer waits forever |

The fix for all of them is the same: establish the ordering before the threads
start, close what you opened, and hold locks for the shortest span.

## Validation

```bash
v -race run main.v
v -race test dir/
```

Then run the suite more than once. A race is timing, so a single clean run is
weak evidence; a race that appears on the fifth run is real.
