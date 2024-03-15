module string_reader

import io

struct Buf {
pub:
	bytes []u8
mut:
	i int
}

fn (mut b Buf) read(mut buf []u8) !int {
	if !(b.i < b.bytes.len) {
		return io.Eof{}
	}
	n := copy(mut buf, b.bytes[b.i..])
	b.i += n
	return n
}

fn test_read_all() {
	mut reader := StringReader.new()

	if _ := reader.read_all(false) {
		assert false, 'should return io.Eof'
	} else {
		assert err is io.Eof
	}

	contents := 'test string'
	buf := Buf{
		bytes: contents.bytes()
	}

	reader = StringReader.new(reader: buf)
	data := reader.read_all(false)!

	assert data == contents
}

fn test_read_bytes() {
	mut reader := StringReader.new()

	if _ := reader.read_bytes(4) {
		assert false, 'should return io.Eof'
	} else {
		assert err.msg() == 'reader is not set'
	}

	buf := Buf{
		bytes: '12345678'.bytes()
	}

	reader = StringReader.new(reader: buf)
	mut data := reader.read_bytes(4)!
	assert data == buf.bytes[..4]

	data = reader.read_bytes(4)!
	assert data == buf.bytes[4..8]
}

fn test_read_line() {
	mut reader := StringReader.new()

	if _ := reader.read_line() {
		assert false, 'should return io.Eof'
	} else {
		assert err is io.Eof
	}

	buf := Buf{
		bytes: 'first line\r\nsecond line\n'.bytes()
	}

	reader = StringReader.new(reader: buf)

	assert reader.read_line()! == 'first line'
	assert reader.read_line()! == 'second line'
}

fn test_from_string() {
	mut reader := StringReader.new(source: 'test')
	assert reader.read_all(false)! == 'test'

	if _ := reader.read_all(false) {
		assert false, 'should return Eof'
	} else {
		assert err is io.Eof
	}
}

fn test_from_string_read_byte_one_by_one() {
	mut reader := StringReader.new(source: 'test')
	assert reader.read_bytes(1)![0].ascii_str() == 't'
	assert reader.read_bytes(1)![0].ascii_str() == 'e'
	assert reader.read_bytes(1)![0].ascii_str() == 's'
	assert reader.read_bytes(1)![0].ascii_str() == 't'

	if _ := reader.read_all(false) {
		assert false, 'should return Eof'
	} else {
		assert err is io.Eof
	}
}

fn test_from_string_and_reader() {
	buf := Buf{
		bytes: 'buffer'.bytes()
	}

	mut reader := StringReader.new(reader: buf, source: 'string')

	assert reader.read_all(false)! == 'stringbuffer'
}

fn test_flush() {
	mut reader := StringReader.new(source: 'flushed data')

	str := reader.flush()
	assert str == 'flushed data'

	assert reader.offset == 0
	assert reader.builder.len == 0
}
