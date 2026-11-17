module mime

// Lookup code for the static tables generated into db.v by `build.vsh`.
// Keep `ph_fold`, `ph_w8` and the mix constants in sync with build.vsh.

// fixed mix constants for the last window, the middle window and the length
const ph_m2 = u64(0x8BB84B93962EACC9)
const ph_m3 = u64(0x4B33A62ED433D4A3)
const ph_m4 = u64(0x9E3779B97F4A7C15)

// longest extension the three 8-byte hash windows can cover
const ph_max_ext_len = 24

// ph_masks[l] covers the low 8*min(l, 8) bits of a folded window word
const ph_masks = [u64(0), 0xFF, 0xFFFF, 0xFFFFFF, 0xFFFFFFFF, 0xFFFFFFFFFF, 0xFFFFFFFFFFFF,
	0xFFFFFFFFFFFFFF, max_u64, max_u64, max_u64, max_u64, max_u64, max_u64, max_u64, max_u64, max_u64,
	max_u64, max_u64, max_u64, max_u64, max_u64, max_u64, max_u64, max_u64]

// ph_fold lowercases the ASCII letters 'A'..'Z' in every byte of `w`
// branchlessly: bytes in the range get bit 0x20 set, every other byte
// (digits, '-', '_', '.', and all non-ASCII bytes) is left untouched.
@[inline]
fn ph_fold(w u64) u64 {
	a := w & 0x7F7F7F7F7F7F7F7F
	ge_a := (a + 0x3F3F3F3F3F3F3F3F) & 0x8080808080808080
	gt_z := (a + 0x2525252525252525) & 0x8080808080808080
	return w | ((ge_a & ~gt_z & ~w) >> 2)
}

// ph_w8 returns the 8 bytes at `p` as a little-endian word. It is
// assembled from single bytes, which is defined for any alignment, and
// an optimizing compiler folds it into one load.
@[direct_array_access; inline; unsafe]
fn ph_w8(p &u8) u64 {
	return unsafe {
		u64(p[0]) | u64(p[1]) << 8 | u64(p[2]) << 16 | u64(p[3]) << 24 |
			u64(p[4]) << 32 | u64(p[5]) << 40 | u64(p[6]) << 48 | u64(p[7]) << 56
	}
}

// lookup returns the MIME type for the given file extension, or '' when
// unknown. The extension may be given with or without a leading dot,
// and is compared ASCII case-insensitively without allocating.
//
// The extension is hashed and compared as up to three 8-byte windows,
// ASCII-case-folded branchlessly, so the cost does not grow with its
// length. After the single table probe the finish is branchless, so
// unknown extensions take no mispredicted early exits.
@[direct_array_access; inline]
pub fn lookup(ext string) string {
	mut i := 0
	if ext.len > 0 && ext[0] == `.` {
		i = 1
	}
	l := ext.len - i
	if l == 0 || l > ph_max_ext_len {
		return ''
	}
	mut w1 := u64(0)
	mut wl := u64(0)
	mut wm := u64(0)
	if l >= 8 {
		// unsafe: word reads of ext[i..], all within its `l >= 8` bytes
		p := unsafe { ext.str + i }
		w1 = ph_fold(unsafe { ph_w8(p) })
		if l > 8 {
			wl = ph_fold(unsafe { ph_w8(p + l - 8) })
		}
		if l > 16 {
			wm = ph_fold(unsafe { ph_w8(p + 8) })
		}
	} else {
		// gather the bytes into one little-endian word with a rolling
		// place multiplier, so no runtime shift (and its check) is needed
		mut w := u64(0)
		mut pow := u64(1)
		for j in 0 .. l {
			w += u64(ext[i + j]) * pow
			pow <<= 8
		}
		w1 = ph_fold(w)
	}
	x := (w1 * ph_m1) ^ (wl * ph_m2) ^ (wm * ph_m3) ^ (u64(l) * ph_m4)
	h := u32(x ^ (x >> 32))
	slot := ph_slots[((h >> ph_slot_shift) + ph_disp[h & ph_bmask]) & ph_tmask]
	// branchless finish: read every window of the stored key, compare, and
	// select the result; db_names[0] is the empty string for a miss
	el := int((slot >> ph_mbits) & 31)
	off := int(slot >> (ph_mbits + 5)) - 1
	// unsafe: ph_blob has 8 leading and 16 trailing padding bytes, so all
	// three windows of any stored key (or of the empty slot, off = -1)
	// are in bounds
	p := unsafe { ph_blob.str + off + 9 }
	m := ph_masks[l]
	// slot == 0 needs no test of its own: it stores el = 0, and l >= 1
	mut eq := u64(el == l)
	eq &= u64((w1 & m) == (unsafe { ph_w8(p) } & m))
	if l > 8 {
		eq &= u64(wl == unsafe { ph_w8(p + el - 8) })
	}
	if l > 16 {
		eq &= u64(wm == unsafe { ph_w8(p + 8) })
	}
	idx := if eq != 0 { slot & ((u32(1) << ph_mbits) - 1) } else { u32(0) }
	return db_names[idx]
}

// name_hash hashes a whole MIME type name as 8-byte little-endian words
// (the last one overlapping for lengths that are not a multiple of 8)
@[direct_array_access; inline]
fn name_hash(s string) u32 {
	mut h := u64(s.len) * ph_m4
	mut i := 0
	// unsafe: every word read is within s[0..s.len]
	for i + 8 <= s.len {
		h = (h ^ unsafe { ph_w8(s.str + i) }) * ph_m3
		h ^= h >> 29
		i += 8
	}
	if i < s.len {
		mut w := u64(0)
		if s.len >= 8 {
			w = unsafe { ph_w8(s.str + s.len - 8) }
		} else {
			mut pow := u64(1)
			for j in 0 .. s.len {
				w += u64(s[j]) * pow
				pow <<= 8
			}
		}
		h = (h ^ w) * ph_m3
		h ^= h >> 29
	}
	h *= ph_m2
	return u32(h ^ (h >> 32))
}

// db_index returns the index of the MIME type `mt` in `db_names`, or 0
// (the empty sentinel entry) when it is unknown: one perfect hash probe
// and one string comparison
@[direct_array_access; inline]
fn db_index(mt string) int {
	h := name_hash(mt)
	idx := int(nh_slots[((h >> nh_shift) + nh_disp[h & nh_bmask]) & nh_tmask])
	return if db_names[idx] == mt { idx } else { 0 }
}
