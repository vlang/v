module builtin

fn C.wyhash(&u8, u64, u64, &u64) u64

fn C.wyhash64(u64, u64) u64

// Default secret parameters from wyhash
const wyp = [u64(0x2d358dccaa6c78a5), 0x8bb84b93962eacc9, 0x4b33a62ed433d4a3, 0x4d5a2da51de1aa47]!

// fast_string_eq is intended to be fast when
// the strings are very likely to be equal
// TODO: add branch prediction hints
@[inline]
fn fast_string_eq(a string, b string) bool {
	if a.len != b.len {
		return false
	}
	unsafe {
		return C.memcmp(a.str, b.str, b.len) == 0
	}
}

fn map_hash_string(pkey voidptr) u64 {
	key := *unsafe { &string(pkey) }
	return C.wyhash(key.str, u64(key.len), 0, &u64(&wyp[0]))
}

// Integer hash functions using wyhash64 with a fixed seed for consistency
@[inline]
fn map_hash_int_1(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u8(pkey) }, wyp[0])
}

@[inline]
fn map_hash_int_2(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u16(pkey) }, wyp[0])
}

@[inline]
fn map_hash_int_4(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u32(pkey) }, wyp[0])
}

@[inline]
fn map_hash_int_8(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u64(pkey) }, wyp[0])
}

// map_enum_fn returns the appropriate function pointer for hash, eq, or clone
// based on kind and element size
fn map_enum_fn(kind int, esize int) voidptr {
	if kind !in [1, 2, 3] {
		panic('map_enum_fn: invalid kind')
	}
	if esize > 8 || esize < 0 {
		panic('map_enum_fn: invalid esize')
	}
	if kind == 1 {
		if esize > 4 {
			return voidptr(map_hash_int_8)
		}
		if esize > 2 {
			return voidptr(map_hash_int_4)
		}
		if esize > 1 {
			return voidptr(map_hash_int_2)
		}
		if esize > 0 {
			return voidptr(map_hash_int_1)
		}
	}
	if kind == 2 {
		if esize > 4 {
			return voidptr(map_eq_int_8)
		}
		if esize > 2 {
			return voidptr(map_eq_int_4)
		}
		if esize > 1 {
			return voidptr(map_eq_int_2)
		}
		if esize > 0 {
			return voidptr(map_eq_int_1)
		}
	}
	if kind == 3 {
		if esize > 4 {
			return voidptr(map_clone_int_8)
		}
		if esize > 2 {
			return voidptr(map_clone_int_4)
		}
		if esize > 1 {
			return voidptr(map_clone_int_2)
		}
		if esize > 0 {
			return voidptr(map_clone_int_1)
		}
	}
	return unsafe { nil }
}

// Move all zeros (deleted entries) to the end of the array and resize
// Optimized to minimize allocations and use memmove where possible
fn (mut d DenseArray) zeros_to_end() {
	mut count := 0
	for i in 0 .. d.len {
		if d.has_index(i) {
			if count != i {
				unsafe {
					// Move key
					C.memmove(d.key(count), d.key(i), d.key_bytes)
					// Move value
					C.memmove(d.value(count), d.value(i), d.value_bytes)
				}
			}
			count++
		}
	}
	d.deletes = 0
	// TODO: reallocate instead as more deletes are likely
	unsafe {
		free(d.all_deleted)
	}
	d.len = count
	old_cap := d.cap
	d.cap = if count < 8 { 8 } else { count }
	unsafe {
		d.values = realloc_data(d.values, d.value_bytes * old_cap, d.value_bytes * d.cap)
		d.keys = realloc_data(d.keys, d.key_bytes * old_cap, d.key_bytes * d.cap)
		// Zero out the new space if needed, but realloc_data may handle it
		if d.cap > old_cap {
			C.memset(d.key(old_cap), 0, d.key_bytes * (d.cap - old_cap))
			C.memset(d.value(old_cap), 0, d.value_bytes * (d.cap - old_cap))
		}
	}
}
