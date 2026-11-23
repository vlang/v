import io

struct StringReader {
	data []u8
mut:
	place int
}

fn (mut s StringReader) read(mut buf []u8) !int {
	$if debug {
		eprintln('>>>> StringReader.read output buf.len: ${buf.len}')
	}
	// An exhausted input returns zero bytes to exercise the buffered reader's retries.
	read := copy(mut buf, s.data[s.place..])
	s.place += read
	return read
}

fn read_one_byte_at_a_time(data []u8, capacity int) []u8 {
	mut stream := io.new_buffered_reader(reader: StringReader{ data: data }, cap: capacity)

	mut buf := []u8{len: 1}
	mut res := []u8{cap: data.len}
	mut i := 0
	for {
		z := stream.read(mut buf) or { break }
		res << buf[0]
		$if debug {
			println('capacity: ${capacity}, i: ${i}, buf: ${buf} | z: ${z}')
		}
		i++
	}
	return res
}

// test_reading_from_a_string checks byte-by-byte reads across a range of buffer capacities.
pub fn test_reading_from_a_string() {
	inputs := ['a', 'ab', 'abc', 'abcde', 'x'.repeat(1000)].map(it.bytes())
	for capacity in 1 .. 1000 {
		for input in inputs {
			assert read_one_byte_at_a_time(input, capacity) == input, 'capacity ${capacity}, input of ${input.len} bytes'
		}
	}
}

// test_buffered_reader_readline checks line splitting and end of input with a custom reader.
pub fn test_buffered_reader_readline() {
	text := 'hello\nworld'
	want := text.split_into_lines()
	mut stream := io.new_buffered_reader(reader: StringReader{ data: text.bytes() })
	mut i := 0
	for {
		line := stream.read_line() or {
			assert err is io.Eof
			break
		}
		assert i < want.len
		assert want[i] == line
		i++
	}
}
