module sync

@[heap]
pub struct Cond {
mut:
	// Externally provided mutex for shared resource protection
	mutex &Mutex
	// Internal lock for protecting wait queue access
	inner_mutex Mutex
	// Queue of waiting channels
	waiters []chan bool
}

// new_cond creates new condition variable associated with given mutex
pub fn new_cond(m &Mutex) &Cond {
	return &Cond{
		mutex:       m
		inner_mutex: new_mutex()
		waiters:     []chan bool{}
	}
}

// wait waits for condition notification.
// NOTE: Spurious wakeups are possible; always use in a loop:
// mutex.lock()
// for !condition {
//     cond.wait()
// }
// mutex.unlock()
@[direct_array_access]
pub fn (mut c Cond) wait() {
	// Create a channel for this waiting operation with capacity 1
	ch := chan bool{cap: 1}
	defer {
		ch.close()
	}

	// Add this channel to the waiters queue
	c.inner_mutex.lock()
	c.waiters << ch
	c.inner_mutex.unlock()

	// Release external lock and suspend
	c.mutex.unlock()
	_ := <-ch // Block until signaled

	c.inner_mutex.lock()
	for i := c.waiters.len - 1; i >= 0; i-- {
		if c.waiters[i] == ch {
			c.waiters.delete(i)
			break
		}
	}
	c.inner_mutex.unlock()
	// Re-acquire external lock before returning
	c.mutex.lock()
}

// signal wakes one waiting thread.
@[direct_array_access]
pub fn (mut c Cond) signal() {
	c.inner_mutex.lock()
	defer { c.inner_mutex.unlock() }
	if c.waiters.len > 0 {
		// Remove first waiter from queue
		mut waiter := c.waiters[0]
		c.waiters.delete(0)
		if !waiter.closed {
			waiter <- true // Wake up the thread
		}
	}
}

// broadcast wakes all waiting threads.
@[direct_array_access]
pub fn (mut c Cond) broadcast() {
	c.inner_mutex.lock()
	defer { c.inner_mutex.unlock() }
	// Release all waiting ch
	for i in 0 .. c.waiters.len {
		mut waiter := c.waiters[i]
		if !waiter.closed {
			waiter <- true // Wake up the thread
		}
	}
	c.waiters.clear()
}
