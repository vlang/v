module io

struct Buf {
pub:
	bytes []u8
mut:
	i int
}

fn (mut b Buf) read(mut buf []u8) !int {
	if !(b.i < b.bytes.len) {
		return Eof{}
	}
	n := copy(mut buf, b.bytes[b.i..])
	b.i += n
	return n
}

fn test_read_all() {
	buf := Buf{
		bytes: '123'.repeat(10).bytes()
	}
	res := read_all(reader: buf) or {
		assert false
		''.bytes()
	}
	assert res == '123'.repeat(10).bytes()
}

fn test_read_all_huge() {
	buf := Buf{
		bytes: '123'.repeat(100000).bytes()
	}
	res := read_all(reader: buf) or {
		assert false
		''.bytes()
	}
	assert res == '123'.repeat(100000).bytes()
}

struct StringReader {
	text string
mut:
	place int
}

fn (mut s StringReader) read(mut buf []u8) !int {
	if s.place >= s.text.len {
		return Eof{}
	}
	read := copy(mut buf, s.text[s.place..].bytes())
	s.place += read
	return read
}

const (
	newline_count = 100000
)

fn test_stringreader() {
	text := '12345\n'.repeat(io.newline_count)
	mut s := StringReader{
		text: text
	}
	mut r := new_buffered_reader(reader: s)
	for i := 0; true; i++ {
		if _ := r.read_line() {
		} else {
			assert i == io.newline_count
			break
		}
	}
	if _ := r.read_line() {
		assert false
	}
	leftover := read_all(reader: r) or {
		assert false
		panic('bad')
	}
	if leftover.len > 0 {
		assert false
	}
}

fn test_stringreader2() {
	text := '12345\r\n'.repeat(io.newline_count)
	mut s := StringReader{
		text: text
	}
	mut r := new_buffered_reader(reader: s)
	for i := 0; true; i++ {
		if _ := r.read_line() {
		} else {
			assert i == io.newline_count
			break
		}
	}
	if _ := r.read_line() {
		assert false
	}
	leftover := read_all(reader: r) or {
		assert false
		panic('bad')
	}
	if leftover.len > 0 {
		assert false
	}
}

fn test_leftover() {
	text := 'This is a test\r\nNice try!'
	mut s := StringReader{
		text: text
	}
	mut r := new_buffered_reader(reader: s)
	_ := r.read_line() or {
		assert false
		panic('bad')
	}
	line2 := r.read_line() or {
		assert false
		panic('bad')
	}
	assert line2 == 'Nice try!'
	if _ := r.read_line() {
		assert false
		panic('bad')
	}
	assert r.end_of_stream()
}
