// BEAM backend sync primitives
// Provides synchronization using Erlang's concurrency primitives
//
// On BEAM, synchronization is fundamentally different from C:
// - BEAM processes don't share mutable memory
// - Instead, they communicate via message passing
// - Mutex-like behavior can be emulated using gen_server or atomics
//
// These implementations provide API compatibility with V's sync module
// while adapting to BEAM's actor-based concurrency model.
module sync

import time

// SpinLock - On BEAM, this uses atomics for the lock flag
// In practice, BEAM's scheduler handles concurrency, so this is mainly
// for API compatibility.
@[noinit]
pub struct SpinLock {
mut:
	locked  u8     // Lock state: 0 = unlocked, 1 = locked
	padding [63]u8 // Cache line padding (fills to 64 bytes total)
}

// new_spin_lock creates and returns a new SpinLock instance
pub fn new_spin_lock() &SpinLock {
	return &SpinLock{
		locked: 0
	}
}

// lock acquires the spin lock
@[inline]
pub fn (s &SpinLock) lock() {
	// On BEAM: Use atomics for compare-and-swap
	// Stub implementation - BEAM's process model means contention is rare
	mut spin_count := 0
	max_spins := 100
	base_delay := 100 // nanoseconds

	for {
		// Try to acquire lock
		unsafe {
			mut sp := s
			if sp.locked == 0 {
				sp.locked = 1
				return
			}
		}

		spin_count++
		if spin_count > max_spins {
			// Back off with sleep
			time.sleep(base_delay * time.nanosecond)
		}
	}
}

// try_lock tries to acquire the lock and returns immediately
@[inline]
pub fn (s &SpinLock) try_lock() bool {
	unsafe {
		mut sp := s
		if sp.locked == 0 {
			sp.locked = 1
			return true
		}
	}
	return false
}

// unlock releases the spin lock
@[inline]
pub fn (s &SpinLock) unlock() {
	unsafe {
		mut sp := s
		sp.locked = 0
	}
}

// destroy frees the resources associated with the spin lock
pub fn (s &SpinLock) destroy() {
	// No-op on BEAM
}

// Mutex - mutual exclusion lock
// On BEAM, implemented as a simple flag (processes don't share memory)
@[heap]
pub struct Mutex {
mut:
	locked bool
}

// new_mutex creates and initializes a new mutex instance
pub fn new_mutex() &Mutex {
	mut m := &Mutex{}
	m.init()
	return m
}

// init initializes the mutex
pub fn (mut m Mutex) init() {
	m.locked = false
}

// lock locks the mutex
@[inline]
pub fn (mut m Mutex) lock() {
	// On BEAM: Simple flag since processes don't share memory
	for m.locked {
		// Yield to other processes
		time.sleep(1 * time.microsecond)
	}
	m.locked = true
}

// try_lock tries to lock the mutex and returns immediately
@[inline]
pub fn (mut m Mutex) try_lock() bool {
	if !m.locked {
		m.locked = true
		return true
	}
	return false
}

// unlock unlocks the mutex
@[inline]
pub fn (mut m Mutex) unlock() {
	m.locked = false
}

// destroy frees the resources associated with the mutex
pub fn (mut m Mutex) destroy() {
	// No-op on BEAM
}

// RwMutex - read/write mutex
// Allows multiple readers or a single writer
@[heap]
pub struct RwMutex {
mut:
	readers   int
	writer    bool
}

// new_rwmutex creates a new read/write mutex
pub fn new_rwmutex() &RwMutex {
	mut m := &RwMutex{}
	m.init()
	return m
}

// init initializes the RwMutex
pub fn (mut m RwMutex) init() {
	m.readers = 0
	m.writer = false
}

// rlock locks the mutex for reading
@[inline]
pub fn (mut m RwMutex) rlock() {
	// Wait for any writer to finish
	for m.writer {
		time.sleep(1 * time.microsecond)
	}
	m.readers++
}

// lock locks the mutex for writing
@[inline]
pub fn (mut m RwMutex) lock() {
	// Wait for readers and other writers to finish
	for m.writer || m.readers > 0 {
		time.sleep(1 * time.microsecond)
	}
	m.writer = true
}

// try_rlock tries to lock for reading and returns immediately
@[inline]
pub fn (mut m RwMutex) try_rlock() bool {
	if !m.writer {
		m.readers++
		return true
	}
	return false
}

// try_wlock tries to lock for writing and returns immediately
@[inline]
pub fn (mut m RwMutex) try_wlock() bool {
	if !m.writer && m.readers == 0 {
		m.writer = true
		return true
	}
	return false
}

// runlock unlocks the RwMutex from reading
@[inline]
pub fn (mut m RwMutex) runlock() {
	if m.readers > 0 {
		m.readers--
	}
}

// unlock unlocks the RwMutex from writing
@[inline]
pub fn (mut m RwMutex) unlock() {
	if m.writer {
		m.writer = false
	} else if m.readers > 0 {
		m.readers--
	}
}

// destroy frees the resources associated with the RwMutex
pub fn (mut m RwMutex) destroy() {
	// No-op on BEAM
}

// Semaphore - counting semaphore
@[heap]
pub struct Semaphore {
mut:
	count u32
}

// new_semaphore creates a new semaphore with initial count 0
pub fn new_semaphore() &Semaphore {
	return new_semaphore_init(0)
}

// new_semaphore_init creates a new semaphore with initial count n
pub fn new_semaphore_init(n u32) &Semaphore {
	mut sem := &Semaphore{}
	sem.init(n)
	return sem
}

// init initializes the semaphore with count n
pub fn (mut sem Semaphore) init(n u32) {
	sem.count = n
}

// post increases the semaphore count by 1
pub fn (mut sem Semaphore) post() {
	sem.count++
}

// wait decreases the semaphore count, blocking if count is 0
pub fn (mut sem Semaphore) wait() {
	for sem.count == 0 {
		time.sleep(1 * time.microsecond)
	}
	sem.count--
}

// try_wait tries to decrease the count and returns immediately
pub fn (mut sem Semaphore) try_wait() bool {
	if sem.count > 0 {
		sem.count--
		return true
	}
	return false
}

// timed_wait waits with a timeout
pub fn (mut sem Semaphore) timed_wait(timeout time.Duration) bool {
	start := time.now()
	for sem.count == 0 {
		if time.since(start) >= timeout {
			return false
		}
		time.sleep(1 * time.microsecond)
	}
	sem.count--
	return true
}

// destroy frees the resources associated with the semaphore
pub fn (mut sem Semaphore) destroy() {
	// No-op on BEAM
}

// WaitGroup - waits for a collection of processes to finish
// Do not copy an instance of WaitGroup, use a ref instead.
//
// usage: in main thread:
// `wg := sync.new_waitgroup()`
// `wg.add(nr_jobs)` before starting jobs with `spawn ...`
// `wg.wait()` to wait for all jobs to have finished
//
// in each parallel job:
// `wg.done()` when finished
@[heap]
pub struct WaitGroup {
mut:
	task_count u32       // current task count
	wait_count u32       // current wait count
	sem        Semaphore // blocks wait() until task_count is zero
}

// new_waitgroup creates a new WaitGroup
pub fn new_waitgroup() &WaitGroup {
	mut wg := WaitGroup{}
	wg.init()
	return &wg
}

// init initializes a WaitGroup
pub fn (mut wg WaitGroup) init() {
	wg.sem.init(0)
}

// add increments (+ve delta) or decrements (-ve delta) task count
// and unblocks any wait() calls if task count becomes zero
pub fn (mut wg WaitGroup) add(delta int) {
	old_nrjobs := int(wg.task_count)
	if delta >= 0 {
		wg.task_count += u32(delta)
	} else {
		if wg.task_count >= u32(-delta) {
			wg.task_count -= u32(-delta)
		} else {
			panic('Negative number of jobs in waitgroup')
		}
	}
	new_nrjobs := old_nrjobs + delta

	if new_nrjobs < 0 {
		panic('Negative number of jobs in waitgroup')
	}

	if new_nrjobs == 0 && wg.wait_count > 0 {
		// Signal all waiters
		num_waiters := wg.wait_count
		wg.wait_count = 0
		for _ in 0 .. num_waiters {
			wg.sem.post()
		}
	}
}

// done is a convenience fn for add(-1)
pub fn (mut wg WaitGroup) done() {
	wg.add(-1)
}

// wait blocks until all tasks are done (task count becomes zero)
pub fn (mut wg WaitGroup) wait() {
	if wg.task_count == 0 {
		// no need to wait
		return
	}
	wg.wait_count++
	wg.sem.wait() // blocks until task_count becomes 0
}

// go starts f in a new thread, with proper add/done handling
pub fn (mut wg WaitGroup) go(f fn ()) {
	wg.add(1)
	spawn fn (mut wg WaitGroup, f fn ()) {
		f()
		wg.done()
	}(mut wg, f)
}

// Helper functions

fn int_min(a int, b int) int {
	if a < b {
		return a
	}
	return b
}
