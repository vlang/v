module chunked

import io

const max_read_len = 8 * 1024

struct ChunkScanner {
	text string
mut:
	pos int
}

fn (mut s ChunkScanner) read(mut buf []u8) !int {
	if s.pos >= s.text.len {
		return io.Eof{}
	}
	end := if s.pos + chunked.max_read_len >= s.text.len {
		s.text.len
	} else {
		s.pos + chunked.max_read_len
	}
	n := copy(mut buf, s.text[s.pos..end].bytes())
	s.pos += n
	return n
}

fn reader(s string) &io.BufferedReader {
	return io.new_buffered_reader(
		reader: &ChunkScanner{
			text: s
		}
	)
}

pub fn decode(text string) string {
	mut reader_ := reader(text)
	return io.read_all(reader: reader_) or { ''.bytes() }.bytestr()
}
