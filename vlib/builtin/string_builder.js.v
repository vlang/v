// Copyright (c) 2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

pub type StringBuilder = []u8

pub fn new_builder(initial_size int) StringBuilder {
	return []u8{cap: initial_size}
}

pub fn (mut b StringBuilder) write_byte(data u8) {
	b << data
}

pub fn (mut b StringBuilder) write_u8(data u8) {
	b << data
}

pub fn (mut b StringBuilder) write(data []u8) ?int {
	if data.len == 0 {
		return 0
	}
	b << data
	return data.len
}

pub fn (b &StringBuilder) byte_at(n int) u8 {
	unsafe {
		return b[n]
	}
}

pub fn (mut b StringBuilder) write_string(s string) {
	if s == '' {
		return
	}

	for c in s {
		b << c
	}
}

pub fn (mut b StringBuilder) writeln(s string) {
	if s != '' {
		b.write_string(s)
	}

	b << 10
}

pub fn (mut b StringBuilder) str() string {
	s := ''

	#for (const c of b.val.arr.arr)
	#s.str += String.fromCharCode(+c)
	b.clear()
	return s
}

pub fn (mut b StringBuilder) cut_last(n int) string {
	cut_pos := b.len - n
	x := unsafe { b[cut_pos..] }
	res := x.bytestr()
	b.trim(cut_pos)
	return res
}

pub fn (mut b StringBuilder) go_back_to(pos int) {
	b.trim(pos)
}

// go_back discards the last `n` bytes from the buffer.
pub fn (mut b StringBuilder) go_back(n int) {
	b.trim(b.len - n)
}

// cut_to cuts the string after `pos` and returns it.
// if `pos` is superior to builder length, returns an empty string
// and cancel further operations
pub fn (mut b StringBuilder) cut_to(pos int) string {
	if pos > b.len {
		return ''
	}
	return b.cut_last(b.len - pos)
}

pub fn (mut b StringBuilder) write_runes(runes []rune) {
	for r in runes {
		res := r.str()
		#res.str = String.fromCharCode(r.val)
		b << res.bytes()
	}
}

// after(6) returns 'world'
// buf == 'hello world'
pub fn (mut b StringBuilder) after(n int) string {
	if n >= b.len {
		return ''
	}

	x := unsafe { b[n..b.len] }
	return x.bytestr()
}

// last_n(5) returns 'world'
// buf == 'hello world'
pub fn (mut b StringBuilder) last_n(n int) string {
	if n >= b.len {
		return ''
	}

	x := unsafe { b[b.len - n..b.len] }
	return x.bytestr()
}
