// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module strings

// strings.Builder is used to efficiently append many strings to a large
// dynamically growing buffer, then use the resulting large string. Using
// a string builder is much better for performance/memory usage than doing
// constantly string concatenation.
pub struct Builder {
pub mut:
	buf []byte
}

// new_builder returns a new string builder, with an initial capacity of `initial_size`
pub fn new_builder(initial_size int) Builder {
	return Builder{
		buf: []byte{cap: initial_size}
	}
}

[inline]
pub fn (b &Builder) len() int {
	return b.buf.len
}

// write_bytes appends `bytes` to the accumulated buffer
[deprecated: 'use Builder.write_ptr() instead']
[deprecated_after: '2021-04-18']
[unsafe]
pub fn (mut b Builder) write_bytes(bytes &byte, len int) {
	unsafe { b.write_ptr(bytes, len) }
}

// write_ptr writes `len` bytes provided byteptr to the accumulated buffer
[unsafe]
pub fn (mut b Builder) write_ptr(ptr &byte, len int) {
	unsafe { b.buf.push_many(ptr, len) }
}

// write_b appends a single `data` byte to the accumulated buffer
pub fn (mut b Builder) write_b(data byte) {
	b.buf << data
}

// write implements the Writer interface
pub fn (mut b Builder) write(data []byte) ?int {
	b.buf << data
	return data.len
}

// write appends the string `s` to the buffer
[inline]
pub fn (mut b Builder) write_string(s string) {
	if s == '' {
		return
	}
	unsafe { b.buf.push_many(s.str, s.len) }
	// for c in s {
	// b.buf << c
	// }
	// b.buf << []byte(s)  // TODO
}

// go_back discards the last `n` bytes from the buffer
pub fn (mut b Builder) go_back(n int) {
	b.buf.trim(b.buf.len - n)
}

// cut_last cuts the last `n` bytes from the buffer and returns them
pub fn (mut b Builder) cut_last(n int) string {
	cut_pos := b.buf.len - n
	res := b.buf[cut_pos..].bytestr()
	b.buf.trim(cut_pos)
	return res
}

// cut_to cuts the string after `pos` and returns it.
// if `pos` is superior to builder length, returns an empty string
// and cancel further operations
pub fn (mut b Builder) cut_to(pos int) string {
	if pos > b.buf.len {
		return ''
	}
	return b.cut_last(b.buf.len - pos)
}

// go_back_to resets the buffer to the given position `pos`
// NB: pos should be < than the existing buffer length.
pub fn (mut b Builder) go_back_to(pos int) {
	b.buf.trim(pos)
}

// writeln appends the string `s`, and then a newline character.
[inline]
pub fn (mut b Builder) writeln(s string) {
	// for c in s {
	// b.buf << c
	// }
	unsafe { b.buf.push_many(s.str, s.len) }
	// b.buf << []byte(s)  // TODO
	b.buf << byte(`\n`)
}

// buf == 'hello world'
// last_n(5) returns 'world'
pub fn (b &Builder) last_n(n int) string {
	if n > b.buf.len {
		return ''
	}
	return b.buf[b.buf.len - n..].bytestr()
}

// buf == 'hello world'
// after(6) returns 'world'
pub fn (b &Builder) after(n int) string {
	if n >= b.buf.len {
		return ''
	}
	return b.buf[n..].bytestr()
}

// str returns a copy of all of the accumulated buffer content.
// NB: after a call to b.str(), the builder b should not be
// used again, you need to call b.free() first, or just leave
// it to be freed by -autofree when it goes out of scope.
// The returned string *owns* its own separate copy of the
// accumulated data that was in the string builder, before the
// .str() call.
pub fn (mut b Builder) str() string {
	b.buf << byte(0)
	bcopy := unsafe { &byte(memdup(b.buf.data, b.buf.len)) }
	s := unsafe { bcopy.vstring_with_len(b.buf.len - 1) }
	b.buf.trim(0)
	return s
}

// free - manually free the contents of the buffer
[unsafe]
pub fn (mut b Builder) free() {
	unsafe { free(b.buf.data) }
}
