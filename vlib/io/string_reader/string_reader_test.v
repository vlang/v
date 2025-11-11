module string_reader

import io

struct Buf {
pub:
	bytes []u8
mut:
	i int
}

struct TwoByteReader {
mut:
	data string
	pos  int
}

fn (mut b Buf) read(mut buf []u8) !int {
	if !(b.i < b.bytes.len) {
		return io.Eof{}
	}
	n := copy(mut buf, b.bytes[b.i..])
	b.i += n
	return n
}

fn (mut r TwoByteReader) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return io.Eof{}
	}
	min := int_min(int_min(r.data.len - r.pos, 2), buf.len)
	for i in 0 .. min {
		buf[i] = r.data[r.pos]
		r.pos++
	}
	return min
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

fn test_fill_buffer_true() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.fill_buffer(true)! == 5
	assert reader.get_string() == '12345'
}

fn test_fill_buffer_false() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.fill_buffer(false)! == 2
	assert reader.get_string() == '12'
}

fn test_fill_buffer_until() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.fill_buffer_until(1)! == 1
	assert reader.builder.len == 1
	assert reader.fill_buffer_until(2)! == 2
	assert reader.builder.len == 3
	assert reader.fill_buffer_until(2)! == 2
	assert reader.builder.len == 5
	assert reader.get_string() == '12345'
}

fn test_fill_buffer_until_one() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.fill_buffer_until(1)! == 1
	assert reader.builder.len == 1
	assert reader.fill_buffer_until(1)! == 1
	assert reader.builder.len == 2
	assert reader.fill_buffer_until(1)! == 1
	assert reader.builder.len == 3
	assert reader.fill_buffer_until(1)! == 1
	assert reader.builder.len == 4
	assert reader.fill_buffer_until(1)! == 1
	assert reader.builder.len == 5
	assert reader.get_string() == '12345'
}

fn test_fill_buffer_until_many() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.fill_buffer_until(1)! == 1
	assert reader.builder.len == 1
	assert reader.fill_buffer_until(12)! == 4
	assert reader.builder.len == 5
	assert reader.fill_buffer_until(123) or { -1 } == -1
	assert reader.builder.len == 5
	assert reader.get_string() == '12345'
}

fn test_read_all_bytes_false() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_all_bytes(false)! == [u8(49), 50]
	assert reader.read_all_bytes(false)! == [u8(51), 52]
	assert reader.read_all_bytes(false)! == [u8(53)]
}

fn test_read_all_bytes_true() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_all_bytes(false)! == [u8(49), 50]
	assert reader.read_all_bytes(true)! == [u8(51), 52, 53]
}

fn test_read_all_false() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_all(false)! == '12'
	assert reader.read_all(false)! == '34'
	assert reader.read_all(false)! == '5'
}

fn test_read_all_true() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_all(false)! == '12'
	assert reader.read_all(true)! == '345'
}

fn test_read_bytes_1() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_bytes(1)! == [u8(49)]
	assert reader.read_bytes(2)! == [u8(50), 51]
	assert reader.read_bytes(2)! == [u8(52), 53]
	assert reader.read_bytes(2) or { [u8(255)] } == [u8(255)]
}

fn test_read_bytes_many() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_bytes(1)! == [u8(49)]
	assert reader.builder.len == 1
	assert reader.offset == 1
	assert reader.read_bytes(123)! == [u8(50), 51, 52, 53]
	assert reader.builder.len == 5
	assert reader.offset == 5
}

fn test_read_string() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_string(1)! == '1'
	assert reader.read_string(2)! == '23'
	assert reader.read_string(3)! == '45'
}

fn test_read() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	mut o1 := []u8{len: 1}
	mut o5 := []u8{len: 5}
	mut res := reader.read(mut o1)!
	assert res == 1
	assert o1[..res] == [u8(49)]
	res = reader.read(mut o5)!
	assert res == 4
	assert o5[..res] == [u8(50), 51, 52, 53]
}

fn test_read_many() {
	mut two := TwoByteReader{
		data: '12345'
	}
	mut reader := StringReader.new(reader: two)
	mut o4 := []u8{len: 4}
	mut res := reader.read(mut o4)!
	assert res == 4
	assert o4[..res] == [u8(49), 50, 51, 52]
	res = reader.read(mut o4)!
	assert res == 1
	assert o4[..res] == [u8(53)]
	res = reader.read(mut o4) or { -1 }
	assert res == -1
	assert o4 == [u8(53), 50, 51, 52]
}

fn test_read_line_2() {
	mut two := TwoByteReader{
		data: '12345\n67890'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_line()! == '12345'
	assert reader.read_line()! == '67890'
}

fn test_read_line_3() {
	mut two := TwoByteReader{
		data: '12345\t67890'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_line(delim: `\t`)! == '12345'
	assert reader.read_line()! == '67890'
}

fn test_read_line_4() {
	mut two := TwoByteReader{
		data: '12345\t67890'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_line(delim: `\t`)! == '12345'
	assert reader.read_line(delim: `\t`)! == '67890'
}

fn test_read_line_5() {
	mut two := TwoByteReader{
		data: '\n12345\n67890'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_line()! == ''
	assert reader.read_line()! == '12345'
	assert reader.read_line()! == '67890'
}

fn test_read_line_6() {
	mut two := TwoByteReader{
		data: '\r\n12345\r\n67890'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_line()! == ''
	assert reader.read_line()! == '12345'
	assert reader.read_line()! == '67890'
}

fn test_read_line_7() {
	mut two := TwoByteReader{
		data: '1234567890'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_line(delim: `6`)! == '12345'
	assert reader.read_line(delim: `6`)! == '7890'
	assert reader.read_line() or { '777' } == '777'
}

fn test_read_line_8() {
	mut two := TwoByteReader{
		data: '12345\r\n67890'
	}
	mut reader := StringReader.new(reader: two)
	assert reader.read_bytes(6)! == [u8(49), 50, 51, 52, 53, 13]
	reader.read_line()! // \n
	assert reader.read_line()! == '67890'
}

fn test_write() {
	mut two := TwoByteReader{
		data: '1234567890'
	}
	mut reader := StringReader.new(reader: two)
	reader.read_bytes(1)!
	reader.write('a'.bytes())!
	reader.read_bytes(1)!
	assert reader.get_data() == [u8(49), 97, 50]
}
