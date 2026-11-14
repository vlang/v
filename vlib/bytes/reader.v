module bytes

// Reader implements io.Reader for a byte slice.
pub struct Reader {
mut:
	data []u8
	pos  int
}

pub fn new_reader(b []u8) Reader {
	return Reader{ data: b.clone() }
}

pub fn (mut r Reader) read(mut buf []u8) int {
	if r.pos >= r.data.len {
		return 0
	}
	n := if buf.len < r.data.len - r.pos { buf.len } else { r.data.len - r.pos }
	for i in 0 .. n {
		buf[i] = r.data[r.pos + i]
	}
	r.pos += n
	return n
}

pub fn (r Reader) len() int {
	return r.data.len - r.pos
}

pub fn (mut r Reader) reset(b []u8) {
	r.data = b.clone()
	r.pos = 0
}
