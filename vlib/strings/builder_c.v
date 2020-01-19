// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module strings

pub struct Builder {
mut:
	buf          []byte
pub mut:
	initial_size int=1
}

pub fn new_builder(initial_size int) Builder {
	return Builder{
		buf: make(0, initial_size, 1)
		initial_size: initial_size
	}
}

pub fn (b mut Builder) write_bytes(bytes byteptr, howmany int) {
	b.buf.push_many(bytes, howmany)
}

pub fn (b mut Builder) write_b(data byte) {
	b.buf << data
}

pub fn (b mut Builder) write(s string) {
	if s == '' {
		return
	}
	b.buf.push_many(s.str, s.len)
	// for c in s {
	// b.buf << c
	// }
	// b.buf << []byte(s)  // TODO
}

pub fn (b mut Builder) writeln(s string) {
	// for c in s {
	// b.buf << c
	// }
	b.buf.push_many(s.str, s.len)
	// b.buf << []byte(s)  // TODO
	b.buf << `\n`
}

pub fn (b mut Builder) str() string {
	b.buf << `\0`
	return string(b.buf, b.buf.len)
}

pub fn (b mut Builder) cut(n int) {
    b.buf.cut(n)
}

pub fn (b mut Builder) clear() {
    b.buf.clear()
}

pub fn (b mut Builder) free() {
	unsafe{
		free(b.buf.data)
	}
	b.buf = make(0, b.initial_size, 1)
}

