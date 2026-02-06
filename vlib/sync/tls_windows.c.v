module sync

// Windows TLS API functions (via C interface)
fn C.TlsAlloc() u32
fn C.TlsSetValue(key u32, voidptr) bool
fn C.TlsGetValue(key u32) voidptr
fn C.TlsFree(key u32) bool

// new_tls creates new TLS storage initialized with the given `value`
pub fn new_tls[T](value T) !ThreadLocalStorage[T] {
	$if T !in [i8, i16, i32, i64, u8, u16, u32, u64, isize, usize, f32, f64, rune, int, voidptr,
		$pointer] {
		return error('new_tls: invalid type ${T.name}')
	}

	key := C.TlsAlloc()
	// Validate key allocation
	if key != C.TLS_OUT_OF_INDEXES {
		// Initialize storage and verify success
		if C.TlsSetValue(key, convert_t_to_voidptr(value)!) {
			return ThreadLocalStorage[T]{
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
		if !C.TlsSetValue(t.key, convert_t_to_voidptr(value)!) {
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
		return convert_voidptr_to_t[T](C.TlsGetValue(t.key))
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
