module builtin

fn C.wyhash(&byte, u64, u64, &u64) u64

fn C.wyhash64(u64, u64) u64

// fast_string_eq is intended to be fast when
// the strings are very likely to be equal
// TODO: add branch prediction hints
[inline]
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
	return C.wyhash(key.str, u64(key.len), 0, &u64(C._wyp))
}

fn map_hash_int_1(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u8(pkey) }, 0)
}

fn map_hash_int_2(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u16(pkey) }, 0)
}

fn map_hash_int_4(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u32(pkey) }, 0)
}

fn map_hash_int_8(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u64(pkey) }, 0)
}

// Move all zeros to the end of the array and resize array
fn (mut d DenseArray) zeros_to_end() {
	// TODO alloca?
	mut tmp_value := unsafe { malloc(d.value_bytes) }
	mut tmp_key := unsafe { malloc(d.key_bytes) }
	mut count := 0
	for i in 0 .. d.len {
		if d.has_index(i) {
			// swap (TODO: optimize)
			unsafe {
				if count != i {
					// Swap keys
					C.memcpy(tmp_key, d.key(count), d.key_bytes)
					C.memcpy(d.key(count), d.key(i), d.key_bytes)
					C.memcpy(d.key(i), tmp_key, d.key_bytes)
					// Swap values
					C.memcpy(tmp_value, d.value(count), d.value_bytes)
					C.memcpy(d.value(count), d.value(i), d.value_bytes)
					C.memcpy(d.value(i), tmp_value, d.value_bytes)
				}
			}
			count++
		}
	}
	unsafe {
		free(tmp_value)
		free(tmp_key)
		d.deletes = 0
		// TODO: reallocate instead as more deletes are likely
		free(d.all_deleted)
	}
	d.len = count
	old_cap := d.cap
	d.cap = if count < 8 { 8 } else { count }
	unsafe {
		d.values = realloc_data(d.values, d.value_bytes * old_cap, d.value_bytes * d.cap)
		d.keys = realloc_data(d.keys, d.key_bytes * old_cap, d.key_bytes * d.cap)
	}
}
