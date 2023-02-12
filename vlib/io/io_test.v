import io

struct Buf {
pub:
	bytes []u8
mut:
	i int
}

struct Writ {
pub mut:
	bytes []u8
}

fn (mut b Buf) read(mut buf []u8) !int {
	if !(b.i < b.bytes.len) {
		return io.Eof{}
	}
	n := copy(mut buf, b.bytes[b.i..])
	b.i += n
	return n
}

fn (mut w Writ) write(buf []u8) !int {
	if buf.len <= 0 {
		return error('none')
	}
	w.bytes << buf
	return buf.len
}

fn test_copy() {
	mut src := Buf{
		bytes: 'abcdefghij'.repeat(10).bytes()
	}
	mut dst := Writ{
		bytes: []u8{}
	}
	io.cp(mut src, mut dst) or { assert false }
	assert dst.bytes == src.bytes
}
