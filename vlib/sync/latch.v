module sync

// Latch is a one-shot countdown gate: threads calling `wait` block until the
// counter reaches zero through `count_down` calls, then all current and future
// waiters pass immediately. Unlike `Barrier` it never resets.
// It is the V-idiomatic equivalent of C++'s `std::latch`.
@[heap]
pub struct Latch {
mut:
	mutex &Mutex
	cond  &Cond
	count int
}

// new_latch creates a Latch with the given counter. A zero counter starts open.
pub fn new_latch(n int) &Latch {
	assert n >= 0, 'sync.new_latch: n must not be negative'
	mut m := new_mutex()
	return &Latch{
		mutex: m
		cond:  new_cond(m)
		count: n
	}
}

// count_down decrements the counter and releases all waiters when it reaches
// zero. Calls after zero are no-ops.
pub fn (mut l Latch) count_down() {
	l.mutex.lock()
	defer {
		l.mutex.unlock()
	}
	if l.count > 0 {
		l.count--
		if l.count == 0 {
			l.cond.broadcast()
		}
	}
}

// wait blocks until the counter reaches zero, then returns immediately.
pub fn (mut l Latch) wait() {
	l.mutex.lock()
	defer {
		l.mutex.unlock()
	}
	for l.count > 0 {
		l.cond.wait()
	}
}

// count returns the current counter value.
pub fn (mut l Latch) count() int {
	l.mutex.lock()
	defer {
		l.mutex.unlock()
	}
	return l.count
}
