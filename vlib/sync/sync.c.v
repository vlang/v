module sync

import time

@[noreturn]
fn cpanic(res int) {
	panic(unsafe { tos_clone(&u8(C.strerror(res))) })
}

@[noreturn]
fn cpanic_errno() {
	cpanic(C.errno)
}

fn should_be_zero(res int) {
	if res != 0 {
		cpanic(res)
	}
}

// SpinLock is a mutual exclusion lock that busy-waits (spins) when locked.
// When one thread holds the lock, any other thread attempting to acquire it
// will loop repeatedly until the lock becomes available.
@[noinit]
pub struct SpinLock {
mut:
	locked  u8     // Lock state: 0 = unlocked, 1 = locked
	padding [63]u8 // Cache line padding (fills to 64 bytes total)
}

// new_spin_lock creates and returns a new SpinLock instance initialized to unlocked state.
pub fn new_spin_lock() &SpinLock {
	mut the_lock := &SpinLock{
		locked: 0
	}
	// Ensure initialization visibility across threads
	C.atomic_thread_fence(C.memory_order_release)
	$if valgrind ? {
		C.ANNOTATE_RWLOCK_CREATE(&the_lock.locked)
	}
	return the_lock
}

// lock acquires the spin lock. If the lock is currently held by another thread,
// this function will spin (busy-wait) until the lock becomes available.
@[inline]
pub fn (s &SpinLock) lock() {
	// Expected value starts as unlocked (0)
	mut expected := u8(0)
	mut spin_count := 0
	max_spins := 100
	base_delay := 100 // nanosecond
	max_delay := 10000 // nanoseconds (10μs)

	// Busy-wait until lock is acquired
	for {
		// Attempt atomic compare-and-swap:
		// Succeeds if current value matches expected (0),
		// then swaps to locked (1)
		if C.atomic_compare_exchange_weak_byte(&s.locked, &expected, 1) {
			$if valgrind ? {
				C.ANNOTATE_RWLOCK_ACQUIRED(&s.locked, 1) // 1 = write lock
			}
			// Prevent critical section reordering
			C.atomic_thread_fence(C.memory_order_acquire)
			return
		}

		spin_count++
		// Exponential backoff after max_spins
		if spin_count > max_spins {
			// Calculate delay with cap: 100ns to 10μs
			exponent := int_min(spin_count / max_spins, 10)
			delay := int_min(base_delay * (1 << exponent), max_delay)
			time.sleep(delay * time.nanosecond)
		} else {
			// Reduce power/bus contention during spinning
			C.cpu_relax()
		}

		expected = 0
	}
}

// try_lock try to lock the spin lock instance and return immediately.
// If the spin lock was already locked, it will return false.
@[inline]
pub fn (s &SpinLock) try_lock() bool {
	// First do a relaxed load to check if lock is free in order to prevent
	// unnecessary cache misses if someone does while(!try_lock())
	// TODO: make a `relaxed` load
	if C.atomic_load_byte(&s.locked) == 0 {
		mut expected := u8(0)
		if C.atomic_compare_exchange_weak_byte(&s.locked, &expected, 1) {
			$if valgrind ? {
				C.ANNOTATE_RWLOCK_ACQUIRED(&s.locked, 1)
			}
			C.atomic_thread_fence(C.memory_order_acquire)
			return true
		}
	}
	return false
}

// unlock releases the spin lock, making it available to other threads.
// IMPORTANT: Must only be called by the thread that currently holds the lock.
@[inline]
pub fn (s &SpinLock) unlock() {
	$if valgrind ? {
		C.ANNOTATE_RWLOCK_RELEASED(&s.locked, 1) // 1 = write lock
	}
	// Ensure critical section completes before release
	C.atomic_thread_fence(C.memory_order_release)

	// Atomically reset to unlocked state
	C.atomic_store_byte(&s.locked, 0)
}

// destroy frees the resources associated with the spin lock instance.
pub fn (s &SpinLock) destroy() {
	$if valgrind ? {
		C.ANNOTATE_RWLOCK_DESTROY(&s.locked)
	}
}
