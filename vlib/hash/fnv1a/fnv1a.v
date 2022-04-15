module fnv1a

// This module implements a FNV-1a hash.
// (see https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function)

const (
	fnv64_prime        = u64(1099511628211)
	fnv64_offset_basis = u64(14695981039346656037)
	fnv32_offset_basis = u32(2166136261)
	fnv32_prime        = u32(16777619)
)

// sum32 returns a fnv1a hash of the string, described by `data`
[direct_array_access; inline]
pub fn sum32_string(data string) u32 {
	mut hash := fnv1a.fnv32_offset_basis
	for i in 0 .. data.len {
		hash = (hash ^ u32(data[i])) * fnv1a.fnv32_prime
	}
	return hash
}

// sum32 returns a fnv1a hash of the memory block, described by the dynamic
// byte array `data`.
[direct_array_access; inline]
pub fn sum32(data []u8) u32 {
	mut hash := fnv1a.fnv32_offset_basis
	for i in 0 .. data.len {
		hash = (hash ^ u32(data[i])) * fnv1a.fnv32_prime
	}
	return hash
}

// sum32_bytes returns a fnv1a hash of the struct `s`.
[direct_array_access; inline]
pub fn sum32_struct<T>(s &T) u32 {
	bp := unsafe { &u8(s) }
	sz := int(sizeof(T))
	mut hash := fnv1a.fnv32_offset_basis
	for i in 0 .. sz {
		hash = unsafe { (hash ^ u32(bp[i])) * fnv1a.fnv32_prime }
	}
	return hash
}

// sum32_bytes returns a fnv1a hash of `data_len` bytes starting at
// the address in the given &byte pointer `data`.
[direct_array_access; inline; unsafe]
pub fn sum32_bytes(data &u8, data_len int) u32 {
	mut hash := fnv1a.fnv32_offset_basis
	for i in 0 .. data_len {
		hash = unsafe { (hash ^ u32(data[i])) * fnv1a.fnv32_prime }
	}
	return hash
}

// sum64 returns a fnv1a hash of the string, described by `data`
[direct_array_access; inline]
pub fn sum64_string(data string) u64 {
	mut hash := fnv1a.fnv64_offset_basis
	for i in 0 .. data.len {
		hash = (hash ^ u64(data[i])) * fnv1a.fnv64_prime
	}
	return hash
}

// sum64 returns a fnv1a hash of the memory block, described by the dynamic
// byte array `data`.
[direct_array_access; inline]
pub fn sum64(data []u8) u64 {
	mut hash := fnv1a.fnv64_offset_basis
	for i in 0 .. data.len {
		hash = (hash ^ u64(data[i])) * fnv1a.fnv64_prime
	}
	return hash
}

// sum64_bytes returns a fnv1a hash of `data_len` bytes starting at
// the address in the given &byte pointer `data`.
[direct_array_access; inline; unsafe]
pub fn sum64_bytes(data &u8, data_len int) u64 {
	mut hash := fnv1a.fnv64_offset_basis
	for i in 0 .. data_len {
		hash = unsafe { (hash ^ u64(data[i])) * fnv1a.fnv64_prime }
	}
	return hash
}

// sum64_bytes returns a fnv1a hash of the struct `s`.
[direct_array_access; inline]
pub fn sum64_struct<T>(s &T) u64 {
	bp := unsafe { &u8(s) }
	sz := int(sizeof(T))
	mut hash := fnv1a.fnv64_offset_basis
	for i in 0 .. sz {
		hash = unsafe { (hash ^ u64(bp[i])) * fnv1a.fnv64_prime }
	}
	return hash
}
