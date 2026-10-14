module sync

// StampedLock is a lock with optimistic reads: readers can take a stamp and
// validate it afterwards without holding a read lock, falling back to a
// regular read lock when validation fails. It is the V-idiomatic equivalent
// of Java's `java.util.concurrent.locks.StampedLock`.
@[heap]
pub struct StampedLock {
mut:
	mutex   &Mutex
	cond    &Cond
	readers int
	writing bool
	version u64
}

// new_stamped_lock creates an unlocked StampedLock.
pub fn new_stamped_lock() &StampedLock {
	mut m := new_mutex()
	return &StampedLock{
		mutex:   m
		cond:    new_cond(m)
		version: 1
	}
}

// try_optimistic_read returns a nonzero stamp, or zero while a writer is active.
// It briefly acquires the internal mutex but does not wait for a writer to finish.
// Optimistic access must use atomic data or other synchronization to avoid data races.
pub fn (mut l StampedLock) try_optimistic_read() u64 {
	l.mutex.lock()
	defer {
		l.mutex.unlock()
	}
	return if l.writing { u64(0) } else { l.version }
}

// validate reports whether no write happened since stamp was taken and no
// write is in progress.
pub fn (mut l StampedLock) validate(stamp u64) bool {
	l.mutex.lock()
	defer {
		l.mutex.unlock()
	}
	return stamp != 0 && !l.writing && l.version == stamp
}

// read_lock acquires shared read access, blocking while a writer is active.
// Any number of readers may hold the lock at once.
pub fn (mut l StampedLock) read_lock() {
	l.mutex.lock()
	defer {
		l.mutex.unlock()
	}
	for l.writing {
		l.cond.wait()
	}
	l.readers++
}

// read_unlock releases one shared read hold.
pub fn (mut l StampedLock) read_unlock() {
	l.mutex.lock()
	defer {
		l.mutex.unlock()
	}
	l.readers--
	if l.readers == 0 {
		l.cond.broadcast()
	}
}

// write_lock acquires exclusive access, blocking while readers hold or
// another writer is active.
pub fn (mut l StampedLock) write_lock() {
	l.mutex.lock()
	defer {
		l.mutex.unlock()
	}
	for l.writing || l.readers > 0 {
		l.cond.wait()
	}
	l.writing = true
}

// write_unlock releases exclusive access, invalidating outstanding optimistic
// stamps, and wakes waiters.
pub fn (mut l StampedLock) write_unlock() {
	l.mutex.lock()
	defer {
		l.mutex.unlock()
	}
	l.writing = false
	l.version++
	if l.version == 0 {
		l.version = 1
	}
	l.cond.broadcast()
}
