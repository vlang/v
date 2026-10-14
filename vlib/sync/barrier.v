module sync

// Barrier is a reusable rendezvous point for a fixed number of threads.
// All `n` threads must call `wait` before any of them is released, and
// exactly one of them (the "leader") observes a `true` return value per round.
// Barrier is the V-idiomatic equivalent of Rust's `std::sync::Barrier`.
@[heap]
pub struct Barrier {
mut:
	mutex      &Mutex
	cond       &Cond
	n          int
	count      u64
	generation u64
}

// new_barrier creates a Barrier that releases once `n` threads are waiting.
// `n` must be positive.
pub fn new_barrier(n int) &Barrier {
	assert n > 0, 'sync.new_barrier: n must be positive'
	mut m := new_mutex()
	return &Barrier{
		mutex: m
		cond:  new_cond(m)
		n:     n
	}
}

// wait blocks until `n` threads have called it, then releases all of them.
// It returns `true` for exactly one thread per round (the leader) and `false`
// for the rest. The barrier is reusable: the next round starts immediately.
pub fn (mut b Barrier) wait() bool {
	b.mutex.lock()
	defer {
		b.mutex.unlock()
	}
	gen := b.generation
	b.count++
	if b.count == u64(b.n) {
		b.count = 0
		b.generation++
		b.cond.broadcast()
		return true
	}
	for gen == b.generation {
		b.cond.wait()
	}
	return false
}
