module sync

// g_park_registry holds the permits and waiters for park/unpark. It is created
// in init() following the same __global pattern as rand.default_rng.
__global g_park_registry &ParkRegistry

fn init() {
	g_park_registry = &ParkRegistry{
		mutex: new_mutex()
	}
}

// ParkRegistry tracks parked threads by thread id (see thread_id).
// It is an internal implementation detail of park/unpark.
struct ParkRegistry {
mut:
	mutex   &Mutex
	permits map[u64]bool
	waiters map[u64]chan bool
}

// park blocks the calling thread until another thread calls `unpark` with its
// thread id (see `thread_id`). If `unpark` was already called for this thread,
// park returns immediately (stored permit), so wakeups are never lost.
// It is the V-idiomatic equivalent of Rust's `std::thread::park`.
pub fn park() {
	tid := thread_id()
	g_park_registry.mutex.lock()
	if tid in g_park_registry.permits {
		g_park_registry.permits.delete(tid)
		g_park_registry.mutex.unlock()
		return
	}
	ch := chan bool{cap: 1}
	g_park_registry.waiters[tid] = ch
	g_park_registry.mutex.unlock()
	_ := <-ch
	ch.close()
}

// unpark wakes the thread currently parked with the given thread id. If no
// thread is parked with that id yet, a permit is stored so its next `park`
// returns immediately.
// It is the V-idiomatic equivalent of Rust's `std::thread::unpark`.
pub fn unpark(tid u64) {
	g_park_registry.mutex.lock()
	defer {
		g_park_registry.mutex.unlock()
	}
	if tid in g_park_registry.waiters {
		mut ch := g_park_registry.waiters[tid]
		g_park_registry.waiters.delete(tid)
		ch <- true
	} else {
		g_park_registry.permits[tid] = true
	}
}
