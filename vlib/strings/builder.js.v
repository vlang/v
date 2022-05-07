// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module strings

/*
pub struct Builder {
mut:
	buf []u8
pub mut:
	len          int
	initial_size int = 1
}*/

pub type Builder = []u8

pub fn new_builder(initial_size int) Builder {
	return []u8{cap: initial_size}
}

pub fn (mut b Builder) write_byte(data byte) {
	b << data
}

pub fn (mut b Builder) clear() {
	b = []u8{cap: b.cap}
}

pub fn (mut b Builder) write_u8(data u8) {
	b << data
}

pub fn (mut b Builder) write(data []u8) ?int {
	if data.len == 0 {
		return 0
	}
	b << data
	return data.len
}

pub fn (b &Builder) byte_at(n int) u8 {
	unsafe {
		return b[n]
	}
}

pub fn (mut b Builder) write_string(s string) {
	if s.len == 0 {
		return
	}

	for c in s {
		b << c
	}
}

pub fn (mut b Builder) writeln(s string) {
	if s.len > 0 {
		b.write_string(s)
	}

	b << 10
}

pub fn (mut b Builder) str() string {
	s := ''

	#for (const c of b.val.arr.arr)
	#s.str += String.fromCharCode(+c)
	b.trim(0)
	return s
}

pub fn (mut b Builder) cut_last(n int) string {
	cut_pos := b.len - n
	x := b[cut_pos..]
	res := x.bytestr()
	b.trim(cut_pos)
	return res
}

pub fn (mut b Builder) go_back_to(pos int) {
	b.trim(pos)
}

// go_back discards the last `n` bytes from the buffer
pub fn (mut b Builder) go_back(n int) {
	b.trim(b.len - n)
}

// cut_to cuts the string after `pos` and returns it.
// if `pos` is superior to builder length, returns an empty string
// and cancel further operations
pub fn (mut b Builder) cut_to(pos int) string {
	if pos > b.len {
		return ''
	}
	return b.cut_last(b.len - pos)
}

pub fn (mut b Builder) write_runes(runes []rune) {
	for r in runes {
		res := r.str()
		#res.str = String.fromCharCode(r.val)
		b << res.bytes()
	}
}

// after(6) returns 'world'
// buf == 'hello world'
pub fn (mut b Builder) after(n int) string {
	if n >= b.len {
		return ''
	}

	x := b.slice(n, b.len)
	return x.bytestr()
}

// last_n(5) returns 'world'
// buf == 'hello world'
pub fn (b &Builder) last_n(n int) string {
	if n >= b.len {
		return ''
	}

	x := b.slice(b.len - n, b.len)
	return x.bytestr()
}
