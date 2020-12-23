// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
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
	str_calls    int
	len          int
	initial_size int = 1
}

// new_builder returns a new string builder, with an initial capacity of `initial_size`
pub fn new_builder(initial_size int) Builder {
	return Builder{
		// buf: make(0, initial_size)
		buf: []byte{cap: initial_size}
		str_calls: 0
		len: 0
		initial_size: initial_size
	}
}

// write_bytes appends `bytes` to the accumulated buffer
pub fn (mut b Builder) write_bytes(bytes byteptr, howmany int) {
	b.buf.push_many(bytes, howmany)
	b.len += howmany
}

// write_b appends a single `data` byte to the accumulated buffer
pub fn (mut b Builder) write_b(data byte) {
	b.buf << data
	b.len++
}

// write appends the string `s` to the buffer
[inline]
pub fn (mut b Builder) write(s string) {
	if s == '' {
		return
	}
	b.buf.push_many(s.str, s.len)
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
	res := tos(copy.data, copy.len - 1)
	return res
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
	b.buf.push_many(s.str, s.len)
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

// str returns all of the accumulated content of the buffer.
// NB: in order to avoid memleaks and additional memory copies, after a call to b.str(),
// the builder b will be empty. The returned string *owns* the accumulated data so far.
pub fn (mut b Builder) str() string {
	b.str_calls++
	if b.str_calls > 1 {
		panic('builder.str() should be called just once.\nIf you want to reuse a builder, call b.free() first.')
	}
	b.buf << `\0`
	s := tos(b.buf.data, b.len)
	bis := b.initial_size
	// free(b.buf.data)
	b.buf = []byte{cap: bis}
	b.len = 0
	return s
}

// manually free the contents of the buffer
pub fn (mut b Builder) free() {
	unsafe { free(b.buf.data) }
	// b.buf = []byte{cap: b.initial_size}
	b.len = 0
	b.str_calls = 0
}
