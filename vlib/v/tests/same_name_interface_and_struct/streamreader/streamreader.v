module streamreader

// Reader mirrors `io.Reader`: an interface whose `read` takes a buffer.
pub interface Reader {
mut:
	read(mut buf []u8) !int
}

// ByteSource is an in-memory `Reader` implementation.
pub struct ByteSource {
mut:
	data []u8
	pos  int
}

// new_byte_source returns a `ByteSource` reading from `s`.
pub fn new_byte_source(s string) &ByteSource {
	return &ByteSource{
		data: s.bytes()
	}
}

// read copies up to `buf.len` bytes into `buf`.
pub fn (mut s ByteSource) read(mut buf []u8) !int {
	if s.pos >= s.data.len {
		return error('eof')
	}
	mut n := 0
	for n < buf.len && s.pos < s.data.len {
		buf[n] = s.data[s.pos]
		n++
		s.pos++
	}
	return n
}

// read_all_through dispatches `read` through the `Reader` interface.
pub fn read_all_through(mut r Reader) string {
	mut out := []u8{}
	mut buf := []u8{len: 3}
	for {
		n := r.read(mut buf) or { break }
		out << buf[..n]
	}
	return out.bytestr()
}
