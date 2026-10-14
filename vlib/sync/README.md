## Description

`sync` provides cross platform handling of concurrency primitives.

On FreeBSD, mutexes store native `pthread_mutex_t` and `pthread_rwlock_t` values.

On macOS and default pthread platforms, zero-value `RwMutex` try locks initialize the native lock
and wait for concurrent first-use initialization to finish before acquiring it.

Channels can carry function values, including aliases such as `type Task = fn ()`.
Function channels support explicit capacities and default initialization in structs
and globals. Arrays of channels can use an explicit `init` expression to initialize
each channel. A received function value can be called normally.

## Barrier

`new_barrier(n)` creates a reusable rendezvous for a positive, fixed number of
participants. Share the returned pointer between workers. Each participant calls
`wait()` once per round; all wait until the last participant arrives. Exactly one wait
returns `true` for that round, identifying its leader. Other waits return `false`.
A completed round resets the arrival count automatically. With one participant, every
wait returns immediately with `true`.

## Timers

Use a timer when a wait needs to participate in a `select`:

```v
import sync
import time

timer := sync.new_timer(500 * time.millisecond)
defer {
	timer.stop()
}
select {
	fired_at := <-timer.c {
		println('timer fired at ${fired_at}')
	}
	// another channel can be handled here
}
```

### Concurrent maps

`sync.new_concurrent_map[K, V]()` creates a map with 32 independently locked shards.
Use `store`, `load`, `load_or_store`, and `delete` for concurrent access. Key types follow
builtin maps; pointer keys use pointer identity even when their pointed-to data changes.
`load_or_store` atomically returns the existing value (with `true`) or stores its argument
(with `false`). `len` visits shards separately and may reflect concurrent changes.
`range` snapshots each shard before calling the callback, so callbacks can mutate the map.
It visits each snapshot entry until the callback returns `false`; concurrent additions and
removals may or may not be reflected. Locks protect map operations, not objects stored as values.

## Phaser and StampedLock

`new_phaser()` creates a reusable barrier. Register each participating worker before the
phase starts. Each registered party calls `arrive_and_wait()` once per phase, or calls
`arrive_and_deregister()` once to leave. The phase advances only when all remaining
parties have arrived. Both methods return the phase they arrived in. With no parties,
arrival and deregistration leave the phase unchanged. `phase_number()` observes the phase.

`new_stamped_lock()` offers shared `read_lock()`/`read_unlock()` and exclusive
`write_lock()`/`write_unlock()` pairs. A `try_optimistic_read()` stamp is nonzero while no
writer is active, and zero during a write. `validate(stamp)` rejects zero and stamps
invalidated by a write. Stamp operations briefly acquire an internal mutex; they do not
hold a read lock or wait for a writer to finish. Optimistic access does not make ordinary
concurrent reads and writes safe: use atomic data or other synchronization, and validate
the stamp after reading. Use a regular read lock for non-atomic shared data. Locks are
not reentrant and must be released once for each corresponding acquisition.

## Latch

`new_latch(n)` creates a one-shot countdown gate, with a nonnegative initial count.
Each `count_down()` decrements a positive count. `wait()` blocks until the count reaches
zero; all current and future waiters then pass immediately. Further countdowns do
nothing. `count()` reads the remaining count under the latch's lock. Construct the latch
with `new_latch`, and share the returned pointer between workers.

## Parker

Share a `new_parker()` pointer between a worker and its waker. The worker calls `park()`,
and the waker calls `unpark(worker_id)` with the worker's `sync.thread_id()`. An unpark
wakes a registered waiter; otherwise it retains one permit for that thread's next park.
Repeated unparks before a park coalesce into one permit. A wake consumes the current
park's permit, so another unpark may permit the next park even before the worker resumes.
Each thread's permits and waits are independent. Keep a Parker scoped to the workers
using it: permits for exited threads are not automatically removed.

## OnceLock and LazyLock

`new_once_lock[T]()` creates an initially empty cell. `set(value)` stores the first value
and returns `true`; subsequent sets return `false` and keep that value. `get()` returns
`none` before initialization. `get_or_init(f)` runs one initializer and returns its value
to every caller. All three methods acquire the same mutex, so a `get()` during
initialization waits for the initializer to finish. The initializer must not call any
method on the same cell, since that would deadlock.

`new_lazy_lock(f)` binds an initializer to a cell. Its `get()` runs the initializer on
first use and returns the cached value thereafter. Concurrent calls wait for the same
initialization. The initializer must not reenter `get()` on the same LazyLock.

Construct these cells with their factory functions and share the returned pointers.
The locks protect initialization and retrieval; a returned pointer, map, or array may
alias the stored data. Synchronize any later mutation of shared returned data separately.
