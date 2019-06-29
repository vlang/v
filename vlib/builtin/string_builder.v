// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

struct StringBuilder {
	buf []byte
	len int
}

pub fn new_string_builder(initial_size int) StringBuilder {
	return StringBuilder {
		buf: new_array(0, initial_size, sizeof(byte))
	}
}

pub fn (b mut StringBuilder) write(s string) {
	b.buf._push_many(s.str, s.len)
	b.len += s.len
}

pub fn (b mut StringBuilder) writeln(s string) {
	b.buf._push_many(s.str, s.len)
	b.buf << `\n`
	b.len += s.len + 1
}

pub fn (b StringBuilder) str() string {
	return tos(b.buf.data, b.len)
}

pub fn (b StringBuilder) cut(n int) {
	b.len -= n
}

pub fn (b mut StringBuilder) free() {
	C.free(b.buf.data)
}
