module io

struct TestBytesReader {
	data []u8
mut:
	pos int
}

fn (mut r TestBytesReader) read(mut buf []u8) !int {
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

fn test_read_at_least_ok() {
	mut r := TestBytesReader{ data: 'hello world'.bytes() }
	mut buf := []u8{len: 5}
	n := read_at_least(mut r, mut buf, 3)!
	assert n >= 3
	assert buf.bytestr() == 'hello'
}

fn test_read_at_least_eof() {
	mut r := TestBytesReader{ data: 'hi'.bytes() }
	mut buf := []u8{len: 5}
	if _ := read_at_least(mut r, mut buf, 3) {
		assert false
	} else {
		assert err.msg().contains('EOF') || err.msg().contains('unexpected')
	}
}
