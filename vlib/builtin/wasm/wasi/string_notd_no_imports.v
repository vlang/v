module builtin

// tos creates a V string, given a C style pointer to a 0 terminated block.
// Note: the memory block pointed by s is *reused, not copied*!
// It will panic, when the pointer `s` is 0.
// See also `tos_clone`.
@[unsafe]
pub fn tos(s &u8, len int) string {
	if s == 0 {
		panic('tos(): nil string')
	}
	return string{
		str: unsafe { s }
		len: len
	}
}

// Concatenation: var += str / str + str
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

pub fn (s string) < (other string) bool {
	// Stolen from C Backend
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
