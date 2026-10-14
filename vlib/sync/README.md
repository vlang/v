## Description

`sync` provides cross platform handling of concurrency primitives.

On macOS and default pthread platforms, zero-value `RwMutex` try locks initialize the native lock
and wait for concurrent first-use initialization to finish before acquiring it.

Channels can carry function values, including aliases such as `type Task = fn ()`.
Function channels support explicit capacities and default initialization in structs
and globals. Arrays of channels can use an explicit `init` expression to initialize
each channel. A received function value can be called normally.

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
