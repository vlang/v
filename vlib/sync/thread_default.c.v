module sync

fn C.pthread_self() usize

// thread_id returns a unique identifier for the caller thread.
// All *currently* running threads in the same process, will have *different* thread identifiers.
// Note: if a thread finishes, and another starts, the identifier of the old thread may be
// reused for the newly started thread.
// In other words, thread IDs are guaranteed to be unique only within a process.
// A thread ID may be reused after a terminated thread has been joined (with `t.wait()`),
// or when the thread has terminated.

pub fn thread_id() u64 {
	return u64(C.pthread_self())
}

// Pthread TLS API functions (via C interface)
fn C.pthread_key_create(key &u32, voidptr) int
fn C.pthread_key_delete(key u32) int
fn C.pthread_setspecific(key u32, const_ptr voidptr) int
fn C.pthread_getspecific(key u32) voidptr

// new_tls creates new TLS storage initialized with the given `value`
pub fn new_tls[T](value T) !&ThreadLocalStorage[T] {
	// Enforce 64-bit storage limit (size of void*)
	$if sizeof(T) > 8 {
		$compile_error('new_tls: Type size exceeds maximum TLS capacity (64 bits)')
	}
	mut t := ThreadLocalStorage[T]{
		in_use: true
	}

	// Validate key allocation
	if C.pthread_key_create(&t.key, 0) == 0 {
		// Initialize storage and verify success
		if C.pthread_setspecific(t.key, voidptr(u64(value))) == 0 {
			return &t
		} else {
			return error('new_tls: Failed to initialize TLS value: ${value}')
		}
	}

	// Handle allocation failure
	return error('new_tls: Failed to allocate TLS index')
}

// set updates the `value` in TLS storage.
@[inline]
pub fn (mut t ThreadLocalStorage[T]) set(value T) ! {
	if t.in_use {
		if C.pthread_setspecific(t.key, voidptr(u64(value))) != 0 {
			return error('set: Failed to update TLS value: ${value}')
		}
	} else {
		return error('set: TLS storage is already destroyed')
	}
}

// get retrieves the current value from TLS storage.
@[inline]
pub fn (mut t ThreadLocalStorage[T]) get() !T {
	if t.in_use {
		return T(u64(C.pthread_getspecific(t.key)))
	}
	return error('get: TLS storage is already destroyed')
}

// destroy releases TLS resources (must be called manually).
@[inline]
pub fn (mut t ThreadLocalStorage[T]) destroy() ! {
	if C.pthread_key_delete(t.key) == 0 {
		t.in_use = false
	} else {
		return error('destroy: Failed to release TLS resources')
	}
}
