module io

struct Buf {
pub:
	bytes []u8
mut:
	i int
}

fn (mut b Buf) read(mut buf []u8) !int {
	eprintln('try read ${b.i} ${b.bytes.len}')
	if !(b.i < b.bytes.len) {
		return Eof{}
	}
	n := copy(mut buf, b.bytes[b.i..])
	b.i += n
	return n
}

fn test_read_all() {
	mut reader := StringReader.new(Buf{}, 0)

	if _ := reader.read_all(false) {
		assert false, 'should return io.Eof'
	} else {
		assert err is Eof
	}

	contents := 'test string'
	buf := Buf{
		bytes: contents.bytes()
	}

	reader = StringReader.new(buf, 0)
	data := reader.read_all(false)!

	assert data == contents
}

fn test_read_bytes() {
	mut reader := StringReader.new(Buf{}, 0)

	if _ := reader.read_bytes(4) {
		assert false, 'should return io.Eof'
	} else {
		assert err is Eof
	}

	buf := Buf{
		bytes: '12345678'.bytes()
	}

	reader = StringReader.new(buf, 0)
	mut data := reader.read_bytes(4)!
	assert data == buf.bytes[..4]

	data = reader.read_bytes(4)!
	assert data == buf.bytes[4..8]
}

fn test_read_line() {
	mut reader := StringReader.new(Buf{}, 0)

	if _ := reader.read_line() {
		assert false, 'should return io.Eof'
	} else {
		assert err is Eof
	}

	buf := Buf{
		bytes: 'first line\r\nsecond line\n'.bytes()
	}

	reader = StringReader.new(buf, 0)

	assert reader.read_line()! == 'first line'
	assert reader.read_line()! == 'second line'
}

fn test_from_string() {
	buf := StringReader.new(Buf{}, 0)

	mut reader := StringReader.from_string(buf, 'test')
	assert reader.read_all(false)! == 'test'
}
