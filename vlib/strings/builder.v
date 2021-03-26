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
	buf          []byte
	len          int
	initial_size int = 1
}

// new_builder returns a new string builder, with an initial capacity of `initial_size`
pub fn new_builder(initial_size int) Builder {
	return Builder{
		// buf: make(0, initial_size)
		buf: []byte{cap: initial_size}
		len: 0
		initial_size: initial_size
	}
}

// write_bytes appends `bytes` to the accumulated buffer
//[deprecated: 'use Builder.write_ptr() instead']
[unsafe]
pub fn (mut b Builder) write_bytes(bytes byteptr, len int) {
	unsafe { b.write_ptr(bytes, len) }
}

// write_ptr writes `len` bytes provided byteptr to the accumulated buffer
[unsafe]
pub fn (mut b Builder) write_ptr(ptr byteptr, len int) {
	unsafe { b.buf.push_many(ptr, len) }
	b.len += len
}

// write_b appends a single `data` byte to the accumulated buffer
pub fn (mut b Builder) write_b(data byte) {
	b.buf << data
	b.len++
}

// write implements the Writer interface
pub fn (mut b Builder) write(data []byte) ?int {
	b.buf << data
	b.len += data.len
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
	b.len += s.len
}

// go_back discards the last `n` bytes from the buffer
pub fn (mut b Builder) go_back(n int) {
	b.buf.trim(b.buf.len - n)
	b.len -= n
}

fn bytes2string(b []byte) string {
	mut copy := b.clone()
	copy << byte(`\0`)
	return unsafe { tos(copy.data, copy.len - 1) }
}

// cut_last cuts the last `n` bytes from the buffer and returns them
pub fn (mut b Builder) cut_last(n int) string {
	res := bytes2string(b.buf[b.len - n..])
	b.buf.trim(b.buf.len - n)
	b.len -= n
	return res
}

/*
pub fn (mut b Builder) cut_to(pos int) string {
	res := bytes2string( b.buf[pos..] )
	b.buf.trim(pos)
	b.len = pos
	return res
}
*/
// go_back_to resets the buffer to the given position `pos`
// NB: pos should be < than the existing buffer length.
pub fn (mut b Builder) go_back_to(pos int) {
	b.buf.trim(pos)
	b.len = pos
}

// writeln appends the string `s`, and then a newline character.
[inline]
pub fn (mut b Builder) writeln(s string) {
	// for c in s {
	// b.buf << c
	// }
	unsafe { b.buf.push_many(s.str, s.len) }
	// b.buf << []byte(s)  // TODO
	b.buf << `\n`
	b.len += s.len + 1
}

// buf == 'hello world'
// last_n(5) returns 'world'
pub fn (b &Builder) last_n(n int) string {
	if n > b.len {
		return ''
	}
	return bytes2string(b.buf[b.len - n..])
}

// buf == 'hello world'
// after(6) returns 'world'
pub fn (b &Builder) after(n int) string {
	if n >= b.len {
		return ''
	}
	return bytes2string(b.buf[n..])
}

// str returns a copy of all of the accumulated buffer content.
// NB: after a call to b.str(), the builder b should not be
// used again, you need to call b.free() first, or just leave
// it to be freed by -autofree when it goes out of scope.
// The returned string *owns* its own separate copy of the
// accumulated data that was in the string builder, before the
// .str() call.
pub fn (mut b Builder) str() string {
	b.buf << `\0`
	s := unsafe { byteptr(memdup(b.buf.data, b.len)).vstring_with_len(b.len) }
	b.len = 0
	b.buf.trim(0)
	return s
}

// free - manually free the contents of the buffer
[unsafe]
pub fn (mut b Builder) free() {
	unsafe { free(b.buf.data) }
	b.len = 0
}
