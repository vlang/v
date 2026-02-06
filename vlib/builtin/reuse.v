module builtin

// reuse_data_as_string provides a way to treat the memory of a []u8 `buffer` as a string value.
// It does not allocate or copy the memory block for the `buffer`, but instead creates a string descriptor,
// that will point to the same memory as the input.
// The intended use of that function, is to allow calling string search methods (defined on string),
// on []u8 values too, without having to copy/allocate by calling .bytestr() (that can be too slow and unnecessary in loops).
// Note: unlike normal V strings, the return value *is not* guaranteed to have a terminating `0` byte,
// since this function does not allocate or modify the input in any way. This is not a problem usually,
// since V methods and functions do not require it, but be careful, if you want to pass that string to call a C. function,
// that expects 0 termination. If you have to do it, make a `tmp := s.clone()` beforehand, and free the cloned `tmp` string
// after you have called the C. function with it.
// The .len field of the result value, will be the same as the buffer.len.
// Note: avoid storing or returning that resulting string,
// and avoid calling the fn with a complex expression (prefer using a temporary variable as an argument).
@[unsafe]
pub fn reuse_data_as_string(buffer []u8) string {
	return string{
		str:    buffer.data
		len:    buffer.len
		is_lit: 1 // prevent freeing the string, since its memory is owned by the input buffer
	}
}

// reuse_string_as_data provides a way to treat the memory of a string `s`, as a []u8 buffer.
// It does not allocate or copy the memory block for the string `s`, but instead creates an array descriptor,
// that will point to the same memory as the input.
// The intended use of that function, is to allow calling array methods (defined on []u8),
// on string values too, without having to copy/allocate by calling .bytes() (that can be too slow and unnecessary in loops).
// Note: since there are no allocations, the buffer *will not* contain the terminating `0` byte, that V strings have usually.
// The .len field of the result value, will be the same as s.len .
// Note: avoid storing or returning that resulting byte buffer,
// and avoid calling the fn with a complex expression (prefer using a temporary variable as an argument).
@[unsafe]
pub fn reuse_string_as_data(s string) []u8 {
	mut res := unsafe {
		array{
			data:         s.str
			len:          s.len
			element_size: 1
			flags:        .nogrow | .noshrink | .nofree // prevent freeing/resizing the array, since its memory is owned by the input string
		}
	}
	return res
}
