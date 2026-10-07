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
	unsafe {
		return C.memcmp(a.str, b.str, b.len) == 0
	}
}

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
const map_hash_k3 = u64(0x4d5a2da51de1aa47)

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

// map_hash_bytes hashes the `len` bytes at `key` the way wyhash does: 16 bytes for
// each 128-bit product, which `_wymix` folds into 64 bits, and three such products
// side by side for a long key. A key of up to 16 bytes takes two products.
// The hash is only for the maps of a program: `hash.wyhash_c` does not change.
@[unsafe]
fn map_hash_bytes(key &u8, len int) u64 {
	unsafe {
		mut p := key
		mut seed := map_hash_k0
		mut a := u64(0)
		mut b := u64(0)
		if len <= 16 {
			if len >= 4 {
				// The first and the last 4 bytes, and the 4 bytes that follow or
				// precede them in a key of 8 bytes or more: they overlap in a shorter one.
				mid := (len >> 3) << 2
				a = (map_hash_half(p) << 32) | map_hash_half(p + mid)
				b = (map_hash_half(p + len - 4) << 32) | map_hash_half(p + len - 4 - mid)
			} else if len > 0 {
				a = (u64(p[0]) << 16) | (u64(p[len >> 1]) << 8) | u64(p[len - 1])
			}
		} else {
			mut left := len
			if left > 48 {
				mut seed1 := seed
				mut seed2 := seed
				for left > 48 {
					seed = C._wymix(map_hash_word(p) ^ map_hash_k1, map_hash_word(p + 8) ^ seed)
					seed1 = C._wymix(map_hash_word(p + 16) ^ map_hash_k2, map_hash_word(p + 24) ^ seed1)
					seed2 = C._wymix(map_hash_word(p + 32) ^ map_hash_k3, map_hash_word(p + 40) ^ seed2)
					p += 48
					left -= 48
				}
				seed ^= seed1 ^ seed2
			}
			for left > 16 {
				seed = C._wymix(map_hash_word(p) ^ map_hash_k1, map_hash_word(p + 8) ^ seed)
				p += 16
				left -= 16
			}
			// The last 16 bytes of the key, which overlap the ones already mixed.
			a = map_hash_word(p + left - 16)
			b = map_hash_word(p + left - 8)
		}
		return C._wymix(C._wymix(a ^ map_hash_k1, b ^ seed) ^ map_hash_k0 ^ u64(len), map_hash_k1)
	}
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

// Move all zeros to the end of the array and resize array
fn (mut d DenseArray) zeros_to_end() {
	// TODO: alloca?
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
					if d.value_bytes != 0 {
						C.memcpy(tmp_value, d.value(count), d.value_bytes)
						C.memcpy(d.value(count), d.value(i), d.value_bytes)
						C.memcpy(d.value(i), tmp_value, d.value_bytes)
					}
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
		d.all_deleted = nil
	}
	d.len = count
	old_cap := d.cap
	if count < 8 {
		d.cap = 8
	} else {
		d.cap = count
	}
	unsafe {
		if d.value_bytes != 0 {
			d.values = realloc_data(d.values, d.value_bytes * old_cap, d.value_bytes * d.cap)
		}
		d.keys = realloc_data(d.keys, d.key_bytes * old_cap, d.key_bytes * d.cap)
	}
}
