module io

struct MemReader {
mut:
	data []u8
	pos  int
}

fn (mut r MemReader) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return Eof{}
	}
	n := if buf.len < r.data.len - r.pos { buf.len } else { r.data.len - r.pos }
	for i in 0 .. n {
		buf[i] = r.data[r.pos + i]
	}
	r.pos += n
	return n
}

struct MemWriter {
mut:
	data []u8
}

fn (mut w MemWriter) write(buf []u8) !int {
	w.data << buf
	return buf.len
}

fn test_copy() {
	mut src := MemReader{ data: 'hello world'.bytes() }
	mut dst := MemWriter{}
	n := copy(mut dst, mut src)!
	assert n == 11
	assert dst.data.bytestr() == 'hello world'
}

fn test_copy_n() {
	mut src := MemReader{ data: 'hello world'.bytes() }
	mut dst := MemWriter{}
	n := copy_n(mut dst, mut src, 5)!
	assert n == 5
	assert dst.data.bytestr() == 'hello'
}

fn test_copy_n_unexpected_eof() {
	mut src := MemReader{ data: 'hi'.bytes() }
	mut dst := MemWriter{}
	if _ := copy_n(mut dst, mut src, 5) {
		assert false
	} else {
		assert err.msg().contains('unexpected EOF')
	}
}

fn test_copy_buffer() {
	mut src := MemReader{ data: 'abc'.bytes() }
	mut dst := MemWriter{}
	n := copy_buffer(mut dst, mut src, mut []u8{len: 2})!
	assert n == 3
	assert dst.data.bytestr() == 'abc'
}
