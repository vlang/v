module io

struct Buf {
pub:
	bytes []byte
mut:
	i     int
}

fn (mut b Buf) read(mut buf []byte) ?int {
	if !(b.i < b.bytes.len) {
		return eof
	}
	n := copy(buf, b.bytes[b.i..b.bytes.len])
	b.i += n
	return n
}

fn test_read_all() {
	buf := Buf{
		bytes: '123'.repeat(10).bytes()
	}
	res := read_all(buf) or {
		assert false
		''.bytes()
	}
	assert res == '123'.repeat(10).bytes()
}

/*
TODO: This test failed by a bug of read_all
fn test_read_all_huge() {
	buf := Buf{bytes: "123".repeat(100000).bytes()}
	res := read_all(buf) or {
		assert false
		"".bytes()
	}
	assert res == "123".repeat(100000).bytes()
}
*/
