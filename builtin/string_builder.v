// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

struct StringBuilder {
	buf []byte
	len int
}

fn new_string_builder(initial_size int) StringBuilder {
	return StringBuilder {
		buf: new_array(0, initial_size, sizeof(byte))
	}
}

fn (b mut StringBuilder) write(s string) {
	b.buf._push_many(s.str, s.len)
	b.len += s.len
}

fn (b mut StringBuilder) writeln(s string) {
	b.buf._push_many(s.str, s.len)
	b.buf << `\n`
	b.len += s.len + 1
}

fn (b StringBuilder) str() string {
	return tos(b.buf.data, b.len)
}

fn (b StringBuilder) cut(n int) {
	b.len -= n
}

fn (b mut StringBuilder) free() {
	C.free(b.buf.data)
}
