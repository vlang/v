module bytes

// Buffer is a variable-sized buffer of bytes with Read and Write methods.
// Mirrors Go's bytes.Buffer (simplified).
pub struct Buffer {
mut:
	buf []u8
}

pub fn new_buffer() Buffer {
	return Buffer{}
}

pub fn new_buffer_with_bytes(b []u8) Buffer {
	return Buffer{ buf: b.clone() }
}

pub fn (mut b Buffer) write(p []u8) int {
	b.buf << p
	return p.len
}

pub fn (mut b Buffer) write_byte(c u8) {
	b.buf << c
}

pub fn (mut b Buffer) write_string(s string) int {
	b.buf << s.bytes()
	return s.len
}

pub fn (b Buffer) bytes() []u8 {
	return b.buf.clone()
}

pub fn (b Buffer) str() string {
	return b.buf.bytestr()
}

pub fn (b Buffer) len() int {
	return b.buf.len
}

pub fn (mut b Buffer) reset() {
	b.buf = []u8{}
}

pub fn (mut b Buffer) read(mut p []u8) int {
	n := if p.len < b.buf.len { p.len } else { b.buf.len }
	for i in 0 .. n {
		p[i] = b.buf[i]
	}
	b.buf = b.buf[n..].clone()
	return n
}

pub fn (mut b Buffer) read_byte() !u8 {
	if b.buf.len == 0 {
		return error('bytes.Buffer: empty')
	}
	c := b.buf[0]
	b.buf = b.buf[1..].clone()
	return c
}
