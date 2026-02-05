// BEAM backend condition variable implementation
// Uses BEAM-compatible synchronization primitives
module sync

import time

// Cond represents a condition variable for BEAM backend
@[heap]
pub struct Cond {
mut:
	// Externally provided mutex for shared resource protection
	mutex &Mutex
	// Internal lock for protecting wait queue access
	inner_mutex Mutex
	// Queue of waiting semaphores (instead of channels)
	waiters []&Semaphore
}

// new_cond creates new condition variable associated with given mutex
pub fn new_cond(m &Mutex) &Cond {
	mut c := &Cond{
		mutex:       m
		inner_mutex: Mutex{}
		waiters:     []&Semaphore{}
	}
	c.inner_mutex.init()
	return c
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
	// Create a semaphore for this waiting operation
	mut sem := new_semaphore()
	defer {
		sem.destroy()
	}

	// Add this semaphore to the waiters queue
	c.inner_mutex.lock()
	c.waiters << sem
	c.inner_mutex.unlock()

	// Release external lock and suspend
	c.mutex.unlock()
	sem.wait() // Block until signaled

	c.inner_mutex.lock()
	for i := c.waiters.len - 1; i >= 0; i-- {
		if c.waiters[i] == sem {
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
	defer {
		c.inner_mutex.unlock()
	}
	if c.waiters.len > 0 {
		// Remove first waiter from queue
		mut waiter := c.waiters[0]
		c.waiters.delete(0)
		waiter.post() // Wake up the thread
	}
}

// broadcast wakes all waiting threads.
@[direct_array_access]
pub fn (mut c Cond) broadcast() {
	c.inner_mutex.lock()
	defer {
		c.inner_mutex.unlock()
	}
	// Release all waiting semaphores
	for i in 0 .. c.waiters.len {
		mut waiter := c.waiters[i]
		waiter.post() // Wake up the thread
	}
	c.waiters.clear()
}

// timed_wait waits for condition notification with a timeout.
// Returns true if signaled, false if timed out.
pub fn (mut c Cond) timed_wait(timeout time.Duration) bool {
	// Create a semaphore for this waiting operation
	mut sem := new_semaphore()
	defer {
		sem.destroy()
	}

	// Add this semaphore to the waiters queue
	c.inner_mutex.lock()
	c.waiters << sem
	c.inner_mutex.unlock()

	// Release external lock and suspend
	c.mutex.unlock()
	result := sem.timed_wait(timeout)

	c.inner_mutex.lock()
	for i := c.waiters.len - 1; i >= 0; i-- {
		if c.waiters[i] == sem {
			c.waiters.delete(i)
			break
		}
	}
	c.inner_mutex.unlock()
	// Re-acquire external lock before returning
	c.mutex.lock()
	return result
}
