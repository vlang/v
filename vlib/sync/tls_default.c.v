module sync

#flag @VEXEROOT/vlib/sync/thread_helper.o

// The native helper converts pthread_key_t to/from the shared u64 key storage.
// Do not redeclare pthread APIs with a key type that differs from the platform.
fn C.v_sync_tls_create(key voidptr) i32
fn C.v_sync_tls_delete(const_key voidptr) i32
fn C.v_sync_tls_set(const_key voidptr, const_value voidptr) i32
fn C.v_sync_tls_get(const_key voidptr) voidptr

// new_tls creates new TLS storage initialized with the given `value`
pub fn new_tls[T](value T) !ThreadLocalStorage[T] {
	$if T !in [i8, i16, i32, i64, u8, u16, u32, u64, int, uint, f32, f64, rune, isize, voidptr, $pointer] {
		return error('new_tls: invalid type ${T.name}')
	}
	mut key := u64(0)
	// Validate key allocation
	if C.v_sync_tls_create(&key) == 0 {
		// Initialize storage and verify success
		if C.v_sync_tls_set(&key, convert_t_to_voidptr(value)!) == 0 {
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
		if C.v_sync_tls_set(&t.key, convert_t_to_voidptr(value)!) != 0 {
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
		return convert_voidptr_to_t[T](C.v_sync_tls_get(&t.key))
	}
	return error('get: TLS storage is already destroyed')
}

// destroy releases TLS resources (must be called manually).
@[inline]
pub fn (mut t ThreadLocalStorage[T]) destroy() ! {
	if C.v_sync_tls_delete(&t.key) == 0 {
		t.in_use = false
	} else {
		return error('destroy: Failed to release TLS resources')
	}
}
