module net

// Byte-level readers for C structures. They keep platform files free
// of exotic pointer casts; only plain byte indexing is used. Bases are
// `&u8` so no conversion happens inside.

// read_u16_at reads a little-endian u16 at byte offset off from base.
fn read_u16_at(p &u8, off int) u16 {
	unsafe {
		return u16(p[off]) | u16(p[off + 1]) << 8
	}
}

// read_u32_at reads a little-endian u32 at byte offset off from base.
fn read_u32_at(p &u8, off int) u32 {
	unsafe {
		return u32(p[off]) | u32(p[off + 1]) << 8 | u32(p[off + 2]) << 16 | u32(p[off + 3]) << 24
	}
}

// read_u64_at reads a little-endian u64 at byte offset off from base.
fn read_u64_at(p &u8, off int) u64 {
	unsafe {
		mut v := u64(0)
		for i in 0 .. 8 {
			v |= u64(p[off + i]) << u32(i * 8)
		}
		return v
	}
}

// read_ptr_at reads a native pointer at byte offset off from base.
fn read_ptr_at(p &u8, off int) u64 {
	if sizeof(voidptr) == 8 {
		return read_u64_at(p, off)
	}
	return u64(read_u32_at(p, off))
}

// copy_bytes_at copies n bytes at byte offset off from base.
fn copy_bytes_at(p &u8, off int, n int) []u8 {
	unsafe {
		mut out := []u8{len: n}
		for i in 0 .. n {
			out[i] = p[off + i]
		}
		return out
	}
}
