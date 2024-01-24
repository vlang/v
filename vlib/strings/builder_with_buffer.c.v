module strings

// BuilderWithBuffer
struct BuilderWithBuffer {
	initial_buffer_cap int
mut:
	buf        []u8
	char_count int
}

// new_builder_limited_by_fixed_array
pub fn new_builder_with_buffer(buffer []u8) BuilderWithBuffer {
	return BuilderWithBuffer{
		buf: buffer // [array already allocate]
		initial_buffer_cap: buffer.cap
	}
}

// write_string
@[direct_array_access]
pub fn (mut b BuilderWithBuffer) write_string(s string) ! {
	if s.len == 0 {
		return
	}
	// try keep at leat one zero at end
	if b.char_count + s.len + 1 > b.initial_buffer_cap {
		error('string overflowing inital capacity. To performance improvement increase array capacity from []u8{cap: ${b.initial_buffer_cap}} to []u8{cap: ${
			b.char_count + s.len + 1}} or more')
	}

	unsafe { b.buf.push_many(s.str, s.len) }

	b.char_count += s.len
}

// str
pub fn (mut b BuilderWithBuffer) str() string {
	// TODO - include 0 at end
	s := unsafe { b.buf.bytestr() }

	return s
}

// free
@[unsafe]
pub fn (mut b BuilderWithBuffer) free() {
	unsafe { b.buf.free() }
}
