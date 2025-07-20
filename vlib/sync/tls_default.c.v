module sync

// Pthread TLS API functions (via C interface)
fn C.pthread_key_create(key voidptr, voidptr) int
fn C.pthread_key_delete(key u64) int
fn C.pthread_setspecific(key u64, const_ptr voidptr) int
fn C.pthread_getspecific(key u64) voidptr

// new_tls creates new TLS storage initialized with the given `value`
pub fn new_tls[T](value T) !ThreadLocalStorage[T] {
	$if T !in [i8, i16, i32, i64, u8, u16, u32, u64, isize, usize, f32, f64, rune, int, voidptr,
		$pointer] {
		return error('new_tls: invalid type ${T.name}')
	}
	mut key := u64(0)
	// Validate key allocation
	if C.pthread_key_create(voidptr(&key), 0) == 0 {
		// Initialize storage and verify success
		if C.pthread_setspecific(key, convert_t_to_voidptr(value)!) == 0 {
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
		if C.pthread_setspecific(t.key, convert_t_to_voidptr(value)!) != 0 {
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
		return convert_voidptr_to_t[T](C.pthread_getspecific(t.key))
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
