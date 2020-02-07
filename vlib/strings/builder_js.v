// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module strings

pub struct Builder {
mut:
	buf []byte
pub mut:
	len int
	initial_size int = 1
}

pub fn new_builder(initial_size int) Builder {
	return Builder {
		buf: make(0, initial_size, sizeof(byte))
		initial_size: initial_size
	}
}

pub fn (b mut Builder) write_b(data byte) {
	b.buf << data
	b.len++
}

pub fn (b mut Builder) write(s string) {
	b.buf.push_many(s.str, s.len)
	//b.buf << []byte(s)  // TODO
	b.len += s.len
}

pub fn (b mut Builder) writeln(s string) {
	b.buf.push_many(s.str, s.len)
	//b.buf << []byte(s)  // TODO
	b.buf << `\n`
	b.len += s.len + 1
}

pub fn (b Builder) str() string {
	return string(b.buf, b.len)
}

pub fn (b mut Builder) cut(n int) {
	b.len -= n
}

pub fn (b mut Builder) free() {
	b.buf = make(0, b.initial_size, 1)
	b.len = 0
}
