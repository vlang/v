module builtin

fn C.wyhash(&u8, u64, u64, &u64) u64

fn C.wyhash64(u64, u64) u64

fn C._wymix(u64, u64) u64

// fast_string_eq is intended to be fast when
// the strings are very likely to be equal
// TODO: add branch prediction hints
@[inline]
fn fast_string_eq(a string, b string) bool {
	if a.len != b.len {
		return false
	}
	$if prod && !tinyc && !msvc && !native {
		return string_bytes_equal(a.str, b.str, b.len)
	}
	return unsafe { C.memcmp(a.str, b.str, b.len) == 0 }
}

@[inline]
fn map_hash_string(pkey voidptr) u64 {
	key := unsafe { &string(pkey) }
	$if prod && !tinyc && !msvc {
		return unsafe { map_hash_bytes(key.str, key.len) }
	} $else {
		// map_hash_bytes counts on a C compiler that folds its byte loads and has a
		// 128-bit product. Without both, the 64-bit multiplication that `wyhash`
		// makes for 8 bytes of a key costs less: a build that is not optimized, and
		// one with a compiler that has no 128-bit integer, keep it.
		// XTODO remove voidptr cast once virtual C.consts can be declared
		return C.wyhash(key.str, u64(key.len), 0, &u64(voidptr(C._wyp)))
	}
}

// The multipliers of wyhash.
const map_hash_k0 = u64(0x2d358dccaa6c78a5)
const map_hash_k1 = u64(0x8bb84b93962eacc9)
const map_hash_k2 = u64(0x4b33a62ed433d4a3)

// map_hash_word returns the 8 bytes at `p` as a little-endian word. Assembling the
// word from its bytes is defined for any alignment of the key, and gives the same
// hash for any byte order; an optimizing compiler folds it into one load.
@[inline; unsafe]
fn map_hash_word(p &u8) u64 {
	unsafe {
		return u64(p[0]) | (u64(p[1]) << 8) | (u64(p[2]) << 16) | (u64(p[3]) << 24) | (u64(p[4]) << 32) | (u64(p[5]) << 40) | (u64(p[6]) << 48) | (u64(p[7]) << 56)
	}
}

// map_hash_half returns the 4 bytes at `p` as a little-endian word.
@[inline; unsafe]
fn map_hash_half(p &u8) u64 {
	unsafe {
		return u64(p[0]) | (u64(p[1]) << 8) | (u64(p[2]) << 16) | (u64(p[3]) << 24)
	}
}

// map_hash_bytes hashes the `len` bytes at `key` with wyhash's 128-bit product
// mix (`_wymix`): one product per 16 bytes, the last 16 bytes overlapping the
// ones before, and one product for a key of up to 16 bytes. The length enters
// through a multiplied seed, so keys that differ only in their last bytes and
// length still mix differently. Compared with full wyhash this drops the
// separate finalizing product and the three-lane loop for long keys: map keys
// are short, and the lookup cost is in the few products, not in bandwidth.
// The hash is only for the maps of a program: `hash.wyhash_c` does not change.
// It stays a leaf call: inlined into `key_to_index` it would make the C
// optimizer stop inlining the lookup helpers into the map operations instead.
// Its products and sums wrap by design, also under `-check-overflow`.
@[ignore_overflow; noinline; unsafe]
fn map_hash_bytes(key &u8, len int) u64 {
	unsafe {
		mut p := key
		mut seed := map_hash_k0 ^ (u64(len) * map_hash_k2)
		if len <= 16 {
			mut a := u64(0)
			mut b := u64(0)
			if len >= 8 {
				// These words overlap for a key under 16 bytes and still cover it.
				a = map_hash_word(p)
				b = map_hash_word(p + len - 8)
			} else if len >= 4 {
				a = map_hash_half(p)
				b = map_hash_half(p + len - 4)
			} else if len > 0 {
				a = (u64(p[0]) << 16) | (u64(p[len >> 1]) << 8) | u64(p[len - 1])
			}
			return C._wymix(a ^ map_hash_k1, b ^ seed)
		}
		mut left := len
		for left > 16 {
			seed = C._wymix(map_hash_word(p) ^ map_hash_k1, map_hash_word(p + 8) ^ seed)
			p += 16
			left -= 16
		}
		// The last 16 bytes of the key, which overlap the ones already mixed.
		return C._wymix(map_hash_word(p + left - 16) ^ map_hash_k1, map_hash_word(p + left - 8) ^ seed)
	}
}

fn map_hash_int_1(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u8(pkey) }, 0)
}

fn map_hash_int_2(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u16(pkey) }, 0)
}

@[inline]
fn map_hash_int_4(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u32(pkey) }, 0)
}

@[inline]
fn map_hash_int_8(pkey voidptr) u64 {
	return C.wyhash64(*unsafe { &u64(pkey) }, 0)
}

fn map_hash_int_16(pkey voidptr) u64 {
	// A 128-bit key is two u64 halves. They are copied out rather than read
	// through the pointer, because a key in a map is only as aligned as the
	// storage it was moved into.
	mut halves := [2]u64{}
	unsafe { C.memcpy(&halves[0], pkey, 16) }
	return C.wyhash64(halves[0], halves[1])
}

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

// Build a compact dense table before committing a deletion. Keys and values
// are moved by bytes, so resident objects retain identity and keys are not cloned.
fn (d &DenseArray) compacted_without(removed int) DenseArray {
	count := d.len - int(d.deletes) - 1
	capacity := int_max(8, count)
	mut result := DenseArray{
		...*d
		cap:         capacity
		len:         0
		deletes:     0
		keys:        unsafe { nil }
		values:      unsafe { nil }
		all_deleted: unsafe { nil }
	}
	mut published := false
	defer {
		if !published { unsafe { result.free_storage() } }
	}
	$if v3_backend ? {
		result.keys = dense_array_allocate(u64(capacity) * u64(d.key_bytes), d.key_alignment)
		result.values = dense_array_allocate(u64(capacity) * u64(d.value_bytes), d.value_alignment)
	} $else {
		result.keys = unsafe { malloc(__at_least_one(u64(capacity) * u64(d.key_bytes))) }
		result.values = unsafe { malloc(__at_least_one(u64(capacity) * u64(d.value_bytes))) }
	}
	for i in 0 .. d.len {
		if i == removed || !d.has_index(i) { continue }
		unsafe {
			vmemcpy(result.key(result.len), d.key(i), d.key_bytes)
			if d.value_bytes != 0 { vmemcpy(result.value(result.len), d.value(i), d.value_bytes) }
		}
		result.len++
	}
	published = true
	return result
}

// Free backing buffers without freeing the keys or values moved into another table.
@[unsafe]
fn (d &DenseArray) free_storage() {
	$if v3_backend ? {
		unsafe {
			if d.keys != nil { dense_array_free(d.keys, d.key_alignment) }
			if d.values != nil { dense_array_free(d.values, d.value_alignment) }
		}
	} $else {
		unsafe {
			free(d.keys)
			free(d.values)
		}
	}
	unsafe { free(d.all_deleted) }
}

@[inline]
fn (d &DenseArray) compacts_after_delete(index int) bool {
	if d.len <= 32 || int(d.deletes) + 1 < (d.len >> 1) { return false }
	mut length := d.len
	mut deleted := int(d.deletes)
	if index == length - 1 {
		length--
		for length > 0 && !d.has_index(length - 1) {
			length--
			deleted--
		}
	} else {
		deleted++
	}
	return length > 32 && deleted >= (length >> 1)
}
