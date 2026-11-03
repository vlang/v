// Module fnv implements FNV-1 and FNV-1a hash functions, similar to Go's
// `hash/fnv`.
module fnv

// 32-bit constants.
pub const offset32 = u32(2166136261)
pub const prime32 = u32(16777619)

// 64-bit constants.
pub const offset64 = u64(14695981039346656037)
pub const prime64 = u64(1099511628211)

// new32 returns a new 32-bit FNV-1 hasher state (offset).
pub fn new32() u32 {
	return offset32
}

// new32a returns a new 32-bit FNV-1a hasher state.
pub fn new32a() u32 {
	return offset32
}

// write32 updates h with data using FNV-1.
pub fn write32(h u32, data []u8) u32 {
	mut x := h
	for b in data {
		x *= prime32
		x ^= u32(b)
	}
	return x
}

// write32a updates h with data using FNV-1a.
pub fn write32a(h u32, data []u8) u32 {
	mut x := h
	for b in data {
		x ^= u32(b)
		x *= prime32
	}
	return x
}

// sum32 computes FNV-1 of data.
pub fn sum32(data []u8) u32 {
	return write32(offset32, data)
}

// sum32a computes FNV-1a of data.
pub fn sum32a(data []u8) u32 {
	return write32a(offset32, data)
}

// new64 returns a new 64-bit FNV-1 hasher state (offset).
pub fn new64() u64 {
	return offset64
}

// new64a returns a new 64-bit FNV-1a hasher state.
pub fn new64a() u64 {
	return offset64
}

// write64 updates h with data using FNV-1.
pub fn write64(h u64, data []u8) u64 {
	mut x := h
	for b in data {
		x *= prime64
		x ^= u64(b)
	}
	return x
}

// write64a updates h with data using FNV-1a.
pub fn write64a(h u64, data []u8) u64 {
	mut x := h
	for b in data {
		x ^= u64(b)
		x *= prime64
	}
	return x
}

// sum64 computes FNV-1 of data.
pub fn sum64(data []u8) u64 {
	return write64(offset64, data)
}

// sum64a computes FNV-1a of data.
pub fn sum64a(data []u8) u64 {
	return write64a(offset64, data)
}
