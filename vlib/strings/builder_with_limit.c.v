module strings

// BuilderLimitedByFixedArray
struct BuilderLimitedByFixedArray {
	buf              &u8
	fixed_array_size int
mut:
	char_count int
}

// new_builder_limited_by_fixed_array
pub fn new_builder_limited_by_fixed_array[F](fixed_array F) BuilderLimitedByFixedArray {
	$if F is $array_fixed {
		return BuilderLimitedByFixedArray{
			buf: &fixed_array // [it is static don't need dynamic alloc]
			fixed_array_size: sizeof(fixed_array)
		}
	} $else {
		$compile_error('fixed_buf need be a fixed array')
	}
}

// write_string
@[direct_array_access]
pub fn (mut b BuilderLimitedByFixedArray) write_string(s string) {
	if s.len == 0 {
		return
	}
	// try keep at leat one zero at end
	if b.char_count + s.len + 1 > b.fixed_array_size {
		panic('string overflow increase fixed array from [${b.fixed_array_size}]u8 to [${
			b.char_count + s.len + 1}]u8 or more')
	}

	for i, c in s {
		unsafe {
			b.buf.vbytes(b.fixed_array_size)[b.char_count + i] = c
		}
	}
	b.char_count += s.len
}

// str
pub fn (mut b BuilderLimitedByFixedArray) str() string {
	s := unsafe { b.buf.vstring_with_len(b.char_count) }

	return s
}
