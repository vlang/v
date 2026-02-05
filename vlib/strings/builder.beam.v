// BEAM string Builder implementation
// V strings.Builder for the BEAM backend
module strings

// Builder accumulates string content efficiently
// Like the C backend, Builder is a type alias for []u8
// On BEAM: iolist would be more efficient, but []u8 is simpler for compatibility
pub type Builder = []u8

// new_builder returns a new string builder, with an initial capacity of `initial_size`
// Codegen: {builder, []}
pub fn new_builder(initial_size int) Builder {
	return Builder([]u8{cap: initial_size})
}

// write_ptr writes `len` bytes provided byteptr to the accumulated buffer
@[unsafe]
pub fn (mut b Builder) write_ptr(ptr &u8, len int) {
	if len == 0 {
		return
	}
	// Write bytes one at a time for BEAM compatibility
	for i in 0 .. len {
		b << unsafe { ptr[i] }
	}
}

// write_string appends the string `s` to the buffer
// Codegen: vbeam_strings:builder_write_string(B, S)
pub fn (mut b Builder) write_string(s string) {
	for c in s {
		b << c
	}
}

// write_byte appends a single `data` byte to the accumulated buffer
// Codegen: vbeam_strings:builder_write_byte(B, Data)
@[inline]
pub fn (mut b Builder) write_byte(data u8) {
	b << data
}

// write_u8 appends a single `data` byte to the accumulated buffer
// Codegen: vbeam_strings:builder_write_u8(B, Data)
@[inline]
pub fn (mut b Builder) write_u8(data u8) {
	b << data
}

// write_rune appends a single rune to the accumulated buffer
// Codegen: vbeam_strings:builder_write_rune(B, R)
pub fn (mut b Builder) write_rune(r rune) {
	// Simplified UTF-8 encoding for BEAM stub
	if r < 0x80 {
		b << u8(r)
	} else if r < 0x800 {
		b << u8(0xC0 | (r >> 6))
		b << u8(0x80 | (r & 0x3F))
	} else if r < 0x10000 {
		b << u8(0xE0 | (r >> 12))
		b << u8(0x80 | ((r >> 6) & 0x3F))
		b << u8(0x80 | (r & 0x3F))
	} else {
		b << u8(0xF0 | (r >> 18))
		b << u8(0x80 | ((r >> 12) & 0x3F))
		b << u8(0x80 | ((r >> 6) & 0x3F))
		b << u8(0x80 | (r & 0x3F))
	}
}

// writeln appends the string `s`, and then a newline character
// Codegen: vbeam_strings:builder_writeln(B, S)
pub fn (mut b Builder) writeln(s string) {
	b.write_string(s)
	b << u8(`\n`)
}

// str returns a copy of all of the accumulated buffer content
// Note: after a call to b.str(), the builder b will be empty
// Codegen: vbeam_strings:builder_str(B)
pub fn (mut b Builder) str() string {
	s := (*b).bytestr()
	b.clear()
	return s
}

// clear clears the builder buffer
// Codegen: vbeam_strings:builder_clear(B)
pub fn (mut b Builder) clear() {
	(*b).clear()
}

// go_back discards the last `n` bytes from the buffer
// Codegen: vbeam_strings:builder_go_back(B, N)
pub fn (mut b Builder) go_back(n int) {
	if n > b.len {
		b.clear()
	} else {
		b.trim(b.len - n)
	}
}

// go_back_to truncates the buffer to the given position
// Codegen: vbeam_strings:builder_go_back_to(B, Pos)
pub fn (mut b Builder) go_back_to(pos int) {
	if pos <= 0 {
		b.clear()
	} else if pos < b.len {
		b.trim(pos)
	}
	// If pos >= b.len, do nothing (already at or past that position)
}

// byte_at returns a byte at position n
// Codegen: vbeam_strings:builder_byte_at(B, N)
pub fn (b &Builder) byte_at(n int) u8 {
	return (*b)[n]
}

// last_n returns the last n characters
// Codegen: vbeam_strings:builder_last_n(B, N)
pub fn (b &Builder) last_n(n int) string {
	if n > b.len {
		return ''
	}
	return (*b)[b.len - n..].bytestr()
}

// after returns the string after position n
// Codegen: vbeam_strings:builder_after(B, N)
pub fn (b &Builder) after(n int) string {
	if n >= b.len {
		return ''
	}
	return (*b)[n..].bytestr()
}

// free frees the buffer (no-op on BEAM)
@[unsafe]
pub fn (mut b Builder) free() {
	// No-op on BEAM, memory is GC'd
}

// reuse_as_plain_u8_array allows using the Builder instance as a plain []u8 return value.
@[unsafe]
pub fn (mut b Builder) reuse_as_plain_u8_array() []u8 {
	return *b
}
