# Concurrency patterns

> The primitives are in the parent skill. This reference covers: the shapes that
> work, and the ones that deadlock.

## Choose the simplest shape that fits

```
A slice to process?          -> parallel.amap
A fixed set of tasks?        -> spawn + WaitGroup
Tasks need to talk?          -> channel
Shared mutable state?        -> Mutex, held as briefly as possible
One value, changed often?    -> AtomicVal
Fire and forget, truly?      -> nothing waits; make it rare and obvious
```

Most "I need concurrency here" is the first row. Reaching for `spawn` when
`amap` fits means hand-writing a WaitGroup and the chance to forget the `wait()`.

## parallel.amap

```v ignore
import arrays.parallel

fn test_it_sums_in_parallel() {
	items := [1, 2, 3, 4, 5]
	total := parallel.amap(items, fn (n int) int { n } | math.sum)
	assert total == 15
}
```

`amap` keeps the input order in the output. If the order does not matter, `run`
avoids allocating the result slice.

For a bounded worker count:

```v ignore
	got := parallel.amap(items, worker, parallel.Params{
		workers: 4
	})
```

## WaitGroup: the complete form

```v ignore
import sync

fn run_all(items []string) []string {
	mut wg := sync.new_waitgroup()
	mut results := []string{len: items.len}

	for item, i in items {
		wg.go(fn () {
			results[i] = fetch(item)
		})
	}
	wg.wait()
	return results
}
```

`wg.go(f)` does `add(1)`, starts the thread and calls `done()` inside it. That is
strictly safer than `spawn` plus a manual `add`, because it removes the two ways
to get the counting wrong.

Writing it by hand is fine when you need a different arrangement:

```v ignore
	wg.add(1)
	spawn fn () {
		defer {
			wg.done()      // defer, so a panic-free path still counts down
		}
		results[i] = fetch(item)
	}
```

Note the `defer`: without it, any early return inside the thread leaves the count
high and `wait()` hangs forever.

## The single-writer pattern

The easiest way to avoid a lock entirely: give each piece of work its own slot, and
let exactly one thread write the shared structure.

```v ignore
fn process_all(items []Item) []Result {
	mut results := []Result{cap: items.len}
	// Each index is written by exactly one thread, and `results` is not reallocated.
	parallel.run(items, fn (item Item, i int) {
		results[i] = process(item)
	})
	return results
}
```

This is why `parallel.amap` keeps order: the write is to a fixed index, so no two
threads touch the same memory and no lock is needed. Give the slice a `cap` so it
is not reallocated under the workers.

## Once for lazy initialisation

```v ignore
import sync

struct Registry {
mut:
	once sync.Once
	mu   sync.Mutex
	by_name map[string]Entry
}

fn (mut r Registry) lookup(name string) ?Entry {
	r.once.do(fn () {
		r.mu.lock()
		r.by_name = load_all()
		r.mu.unlock()
	})
	r.mu.lock()
	defer {
		r.mu.unlock()
	}
	return r.by_name[name]
}
```

`once.do` runs its function exactly once however many threads arrive. Note that
the map is still protected after initialisation — `once` protects the *setup*, not
the data.

## Lock discipline

The three rules that keep a concurrent program readable:

1. **One lock per piece of state.** A lock protects a named thing, not a function.
2. **One global order.** If two locks are ever needed together, take them in the
   same order in every function.
3. **No I/O, no `spawn`, no channel operation inside a lock.** Copy what you need,
   release, then do the slow thing.

```v ignore
// Wrong: the send can block while holding the lock.
lock mu {
	results << compute(x)
}

// Right: compute, release, then send.
value := compute(x)
results << value
```

## Pool

`sync.Pool` reuses objects across goroutines, which matters when the allocation is
expensive and the objects are large:

```v ignore
mut pool := sync.new_pool[Buf](fn (mut buf Buf) {
	buf.reset()
})

buf := pool.get() or { Buf{} }
defer {
	pool.put(buf)
}
```

Do not reach for it before measuring. A pool with the wrong lifetime is harder to
reason about than the allocation it saves.

## Patterns that deadlock

| Shape | Why |
| --- | --- |
| Send on an unbuffered channel before starting the consumer | producer blocks first |
| Two locks taken in different orders | neither thread can proceed |
| A lock held across a channel operation | the consumer needs the same lock |
| `wg.add` after `wg.wait` | the count changes while it is being waited on |
| `wg.done` without a matching `add` | panics on an underflow |
| A `for` loop over a channel nobody closes | the consumer waits forever |

The fix for all of them is the same: establish the ordering before the threads
start, close what you opened, and hold locks for the shortest span.

## Validation

```bash
v -race run main.v
v -race test dir/
```

Then run the suite more than once. A race is timing, so a single clean run is
weak evidence; a race that appears on the fifth run is real.