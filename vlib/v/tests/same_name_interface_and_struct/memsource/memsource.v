module memsource

// Source is an in-memory byte source that satisfies `io.Reader`.
pub struct Source {
	data []u8
mut:
	pos int
}

// new returns a `Source` reading from `s`.
pub fn new(s string) &Source {
	return &Source{
		data: s.bytes()
	}
}

// read copies up to `buf.len` bytes into `buf`.
pub fn (mut s Source) read(mut buf []u8) !int {
	if s.pos >= s.data.len {
		return error('eof')
	}
	n := copy(mut buf, s.data[s.pos..])
	s.pos += n
	return n
}
