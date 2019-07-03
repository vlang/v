// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module strings 

struct Builder {
mut: 
	buf []byte
	len int
}

pub fn new_builder(initial_size int) Builder {
	return Builder {
		buf: _make(0, initial_size, sizeof(byte))
	}
}

pub fn (b mut Builder) write(s string) {
	b.buf._push_many(s.str, s.len)
	//b.buf << []byte(s)  // TODO 
	b.len += s.len
}

pub fn (b mut Builder) writeln(s string) {
	b.buf._push_many(s.str, s.len)
	//b.buf << []byte(s)  // TODO 
	b.buf << `\n`
	b.len += s.len + 1
}

pub fn (b Builder) str() string {
	return tos(b.buf.data, b.len)
}

pub fn (b Builder) cut(n int) {
	b.len -= n
}

pub fn (b mut Builder) free() {
	C.free(b.buf.data)
}
