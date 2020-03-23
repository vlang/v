module fnv1a

const (
	fnv64_prime = u64(1099511628211)
	fnv64_offset_basis = u64(14695981039346656037)
	fnv32_offset_basis = u32(2166136261)
	fnv32_prime = u32(16777619)
)

[inline]
pub fn sum32_string(data string) u32 {
	mut hash := fnv32_offset_basis
	for i in 0..data.len {
		hash = (hash ^ u32(data[i])) * fnv32_prime
	}
	return hash
}

[inline]
pub fn sum32(data []byte) u32 {
	mut hash := fnv32_offset_basis
	for i in 0..data.len {
		hash = (hash ^ u32(data[i])) * fnv32_prime
	}
	return hash
}

[inline]
pub fn sum64_string(data string) u64 {
	mut hash := fnv64_offset_basis
	for i in 0..data.len {
		hash = (hash ^ u64(data[i])) * fnv64_prime
	}
	return hash
}

[inline]
pub fn sum64(data []byte) u64 {
	mut hash := fnv64_offset_basis
	for i in 0..data.len {
		hash = (hash ^ u64(data[i])) * fnv64_prime
	}
	return hash
}
