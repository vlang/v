module sync

// Get current thread ID from Windows API (via C interface)
fn C.GetCurrentThreadId() u32

// thread_id returns a unique identifier for the caller thread.
pub fn thread_id() u64 {
	return u64(C.GetCurrentThreadId())
}

// Windows TLS API functions (via C interface)
fn C.TlsAlloc() u32
fn C.TlsSetValue(key u32, voidptr) bool
fn C.TlsGetValue(key u32) voidptr
fn C.TlsFree(key u32) bool

// new_tls creates new TLS storage initialized with the given `value`
pub fn new_tls[T](value T) !&ThreadLocalStorage[T] {
	// Enforce 64-bit storage limit (size of void*)
	$if sizeof(T) > 8 {
		$compile_error('new_tls: Type size exceeds maximum TLS capacity (64 bits)')
	}

	key := C.TlsAlloc()
	// Validate key allocation
	if key != C.TLS_OUT_OF_INDEXES {
		// Initialize storage and verify success
		if C.TlsSetValue(key, voidptr(u64(value))) {
			return &ThreadLocalStorage[T]{
				key:    key
				in_use: true
			}
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
		if !C.TlsSetValue(t.key, voidptr(u64(value))) {
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
		$if T is $pointer {
			// avoid v compile warning
			return T(voidptr(C.TlsGetValue(t.key)))
		} $else {
			return T(u64(C.TlsGetValue(t.key)))
		}
	}
	return error('get: TLS storage is already destroyed')
}

// destroy releases TLS resources (must be called manually).
@[inline]
pub fn (mut t ThreadLocalStorage[T]) destroy() ! {
	if C.TlsFree(t.key) {
		t.in_use = false
	} else {
		return error('destroy: Failed to release TLS resources')
	}
}
