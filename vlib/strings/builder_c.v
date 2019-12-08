// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module strings

pub struct Builder {
mut:
	buf []byte
pub:
	len int
}

pub fn new_builder(initial_size int) Builder {
	return Builder {
		buf: make(0, initial_size, 1)
	}
}

pub fn (b mut Builder) write_b(data byte) {
	b.buf << data
	b.len++
}

pub fn (b mut Builder) write(s string) {
	b.buf.push_many(s.str, s.len)
	//for c in s {
		//b.buf << c
	//}
	//b.buf << []byte(s)  // TODO
	b.len += s.len
}

pub fn (b mut Builder) writeln(s string) {
	//for c in s {
		//b.buf << c
	//}
	b.buf.push_many(s.str, s.len)
	//b.buf << []byte(s)  // TODO
	b.buf << `\n`
	b.len += s.len + 1
}

pub fn (b mut Builder) str() string {
	b.buf << `\0`
	return string(b.buf, b.len)
}

pub fn (b mut Builder) free() {
	unsafe{ free(b.buf.data) }
	b.buf = make(0, 1, 1)
	b.len = 0
}
