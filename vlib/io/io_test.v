import io

struct Buf {
pub:
	bytes []byte
mut:
	i int
}

struct Writ {
pub mut:
	bytes []byte
}

fn (mut b Buf) read(mut buf []byte) ?int {
	if !(b.i < b.bytes.len) {
		return none
	}
	n := copy(mut buf, b.bytes[b.i..])
	b.i += n
	return n
}

fn (mut w Writ) write(buf []byte) ?int {
	if buf.len <= 0 {
		return none
	}
	w.bytes << buf
	return buf.len
}

fn test_copy() {
	mut src := Buf{
		bytes: 'abcdefghij'.repeat(10).bytes()
	}
	mut dst := Writ{
		bytes: []byte{}
	}
	io.cp(mut src, mut dst) or { assert false }
	assert dst.bytes == src.bytes
}
