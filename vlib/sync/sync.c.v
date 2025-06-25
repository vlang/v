module sync

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
// [Uses 1 byte of memory for lock state]
pub struct SpinLock {
mut:
	locked u8 // Lock state: 0 = unlocked, 1 = locked
}

// new_spin_lock creates and returns a new SpinLock instance initialized to unlocked state
pub fn new_spin_lock() &SpinLock {
	mut the_lock := &SpinLock{
		locked: 0
	}
	C.atomic_thread_fence(C.memory_order_release)
	return the_lock
}

// lock acquires the spin lock. If the lock is currently held by another thread,
// this function will spin (busy-wait) until the lock becomes available.
@[inline]
pub fn (s &SpinLock) lock() {
	// Expected value starts as unlocked (0)
	mut expected := u8(0)

	// Busy-wait loop continues until lock is successfully acquired
	for {
		// Attempt atomic compare-and-swap:
		// - If current value matches expected (0), swap to locked (1)
		// - If swap succeeds, break out of loop and acquire lock
		if C.atomic_compare_exchange_weak_byte(&s.locked, &expected, 1) {
			// Memory barrier: Prevents critical section operations
			// from being reordered before this point
			C.atomic_thread_fence(C.memory_order_acquire)
			return
		}
		// Hint to CPU that we're in spin-wait loop
		// - Reduces power consumption
		// - Minimizes contention on bus/memory
		C.cpu_relax()
		// Reload current lock state before next attempt
		// - Required to detect when lock becomes available
		// - Updates expected value to latest observed state
		expected = C.atomic_load_byte(&s.locked)
	}
}

// unlock releases the spin lock, making it available to other threads.
// IMPORTANT: Must only be called by the thread that currently holds the lock.
@[inline]
pub fn (s &SpinLock) unlock() {
	// Memory barrier: Ensures all critical section operations
	// complete before lock is released
	C.atomic_thread_fence(C.memory_order_release)

	// Atomically unlock by setting state to 0
	C.atomic_store_byte(&s.locked, 0)
}
