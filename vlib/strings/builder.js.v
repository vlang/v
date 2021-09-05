// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module strings

pub struct Builder {
mut:
	buf []byte
pub mut:
	len          int
	initial_size int = 1
}

pub fn new_builder(initial_size int) Builder {
	return Builder{[]byte{cap: initial_size}, 0, initial_size}
}

pub fn (mut b Builder) write_b(data byte) {
	b.buf << data
}

pub fn (mut b Builder) write(data []byte) ?int {
	if data.len == 0 {
		return 0
	}
	b.buf << data
	return data.len
}

pub fn (b &Builder) byte_at(n int) byte {
	return b.buf[n]
}

pub fn (mut b Builder) write_string(s string) {
	if s.len == 0 {
		return
	}

	for c in s {
		b.buf << c
	}
}

pub fn (mut b Builder) writeln(s string) {
	if s.len > 0 {
		b.write_string(s)
	}

	b.buf << 10
}

pub fn (mut b Builder) str() string {
	b.buf << byte(0)
	s := ''

	#for (const c of b.val.buf.arr)
	#s.str += String.fromCharCode(+c)

	return s
}
