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

struct StringReaderTest {
	text string
mut:
	place int
}

fn (mut s StringReaderTest) read(mut buf []u8) !int {
	if s.place >= s.text.len {
		return Eof{}
	}
	read := copy(mut buf, s.text[s.place..].bytes())
	s.place += read
	return read
}

const newline_count = 100000

fn test_stringreadertest() {
	text := '12345\n'.repeat(newline_count)
	mut s := StringReaderTest{
		text: text
	}
	mut r := new_buffered_reader(reader: s)
	for i := 0; true; i++ {
		if _ := r.read_line() {
		} else {
			assert i == newline_count
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

fn test_stringreadertest2() {
	text := '12345\r\n'.repeat(newline_count)
	mut s := StringReaderTest{
		text: text
	}
	mut r := new_buffered_reader(reader: s)
	for i := 0; true; i++ {
		if _ := r.read_line() {
		} else {
			assert i == newline_count
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
	mut s := StringReaderTest{
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

fn test_totalread_read() {
	text := 'Some testing text'
	mut s := StringReaderTest{
		text: text
	}
	mut r := new_buffered_reader(reader: s)

	mut buf := []u8{len: text.len}
	total := r.read(mut buf) or {
		assert false
		panic('bad')
	}

	assert r.total_read == total
}

fn test_totalread_readline() {
	text := 'Some testing text\nmore_enters'
	mut s := StringReaderTest{
		text: text
	}
	mut r := new_buffered_reader(reader: s)

	_ := r.read_line() or {
		assert false
		panic('bad')
	}
	_ := r.read_line() or {
		assert false
		panic('bad')
	}

	assert r.total_read == text.len
}

fn test_read_line_until_zero_terminated() {
	text := 'This is a test\0Nice try!\0'
	mut s := StringReaderTest{
		text: text
	}
	mut r := new_buffered_reader(reader: s)
	line1 := r.read_line(delim: `\0`) or {
		assert false
		panic('bad')
	}
	assert line1 == 'This is a test'
	line2 := r.read_line(delim: `\0`) or {
		assert false
		panic('bad')
	}
	assert line2 == 'Nice try!'
	if _ := r.read_line(delim: `\0`) {
		assert false
		panic('bad')
	}
	assert r.end_of_stream()
}
