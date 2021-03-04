import io

struct StringReader {
	text string
mut:
	place int
}

fn imin(a int, b int) int {
	return if a < b { a } else { b }
}

fn (mut s StringReader) read(mut buf []byte) ?int {
	$if debug {
		eprintln('>>>> StringReader.read output buf.len: $buf.len')
	}
	if s.place > s.text.len + 1 {
		return none
	}
	mut howmany := imin(buf.len, s.text.len - s.place)
	xxx := s.text[s.place..s.place + howmany].bytes()
	read := copy(buf, xxx)
	s.place += read
	return read
}

fn read_from_string(text string, capacity int) []byte {
	mut str := StringReader{
		text: text
	}
	mut stream := io.new_buffered_reader(reader: io.make_reader(str), cap: capacity)
	//
	mut buf := []byte{len: 1}
	mut res := []byte{}
	mut i := 0
	for {
		z := stream.read(mut buf) or { break }
		res << buf
		$if debug {
			println('capacity: $capacity, i: $i, buf: $buf | z: $z')
		}
		i++
	}
	return res
}

pub fn test_reading_from_a_string() {
	for capacity in 1 .. 1000 {
		assert read_from_string('a', capacity) == [byte(`a`)]
		assert read_from_string('ab', capacity) == [byte(`a`), `b`]
		assert read_from_string('abc', capacity) == [byte(`a`), `b`, `c`]
		assert read_from_string('abcde', capacity) == [byte(`a`), `b`, `c`, `d`, `e`]
		large_string_bytes := []byte{len: 1000, init: `x`}
		large_string := large_string_bytes.bytestr()
		assert read_from_string(large_string, capacity) == large_string_bytes
	}
}
