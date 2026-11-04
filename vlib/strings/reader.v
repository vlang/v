module strings

import io

// Reader implements io.Reader for a string.
pub struct Reader {
mut:
	s   string
	pos int
}

pub fn new_reader(s string) Reader {
	return Reader{ s: s }
}

pub fn (mut r Reader) read(mut buf []u8) !int {
	if r.pos >= r.s.len {
		return 0
	}
	n := if buf.len < r.s.len - r.pos { buf.len } else { r.s.len - r.pos }
	for i in 0 .. n {
		buf[i] = r.s[r.pos + i]
	}
	r.pos += n
	return n
}

pub fn (r Reader) len() int {
	return r.s.len - r.pos
}

pub fn (mut r Reader) reset(s string) {
	r.s = s
	r.pos = 0
}
