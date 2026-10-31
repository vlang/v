module sync

// Parker is a caller-owned registry for parking and unparking threads by
// thread id (see `thread_id`). It is the V-idiomatic equivalent of Rust's
// `std::thread::{park, unpark}`: unlike free functions, it needs no globals,
// so it cannot disturb other code in the module.
//
// A Parker is typically shared between a worker thread, which calls `park`,
// and the thread that later wakes it with `unpark`.
@[heap]
pub struct Parker {
mut:
	mutex   &Mutex
	permits map[u64]bool
	waiters map[u64]chan bool
}

// new_parker creates an empty Parker.
pub fn new_parker() &Parker {
	mut m := new_mutex()
	return &Parker{
		mutex: m
	}
}

// park blocks the calling thread until another thread calls `unpark` with its
// thread id. If `unpark` was already called for this thread, park returns
// immediately (stored permit), so wakeups are never lost.
pub fn (mut p Parker) park() {
	tid := thread_id()
	p.mutex.lock()
	if tid in p.permits {
		p.permits.delete(tid)
		p.mutex.unlock()
		return
	}
	ch := chan bool{cap: 1}
	p.waiters[tid] = ch
	p.mutex.unlock()
	_ := <-ch
	ch.close()
}

// unpark wakes the thread currently parked with the given thread id. If no
// thread is parked with that id yet, a permit is stored so its next `park`
// returns immediately. Repeated unparks without a waiting thread retain one permit.
pub fn (mut p Parker) unpark(tid u64) {
	p.mutex.lock()
	defer {
		p.mutex.unlock()
	}
	if tid in p.waiters {
		mut ch := p.waiters[tid]
		p.waiters.delete(tid)
		ch <- true
	} else {
		p.permits[tid] = true
	}
}
