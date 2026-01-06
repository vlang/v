module builtin

// String: This is a minimal implementation of the "string" builtin until the WASM backend
// is able to compile the original builtin implementation

pub struct string {
pub:
	str &u8
	len int
}

// Concatenation operator: var += str / str + str
// Note: This will alloc a new string with the content of these two strings
pub fn (s string) + (other string) string {
	if s.len == 0 {
		return other
	}
	if other.len == 0 {
		return s
	}
	total_len := s.len + other.len
	result_ptr := unsafe { malloc(total_len) }
	if result_ptr == 0 {
		panic('string.+: malloc failed')
	}
	unsafe {
		vmemcpy(result_ptr, s.str, s.len)
		vmemcpy(result_ptr + s.len, other.str, other.len)
	}
	return string{
		str: result_ptr
		len: total_len
	}
}

// Equality comparison: checks if two strings are identical
pub fn (s string) == (other string) bool {
	if s.len != other.len {
		return false
	}

	for i in 0 .. s.len {
		if s[i] != other[i] {
			return false
		}
	}

	return true
}

// Less-than comparison: lexicographically compares two strings
pub fn (s string) < (other string) bool {
	// Taken from the C Backend
	for i in 0 .. s.len {
		if i >= other.len || s[i] > other[i] {
			return false
		} else if s[i] < other[i] {
			return true
		}
	}
	if s.len < other.len {
		return true
	}
	return false
}
