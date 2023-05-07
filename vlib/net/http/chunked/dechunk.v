module chunked

import io
// See: https://en.wikipedia.org/wiki/Chunked_transfer_encoding
// /////////////////////////////////////////////////////////////
// The chunk size is transferred as a hexadecimal number
// followed by \r\n as a line separator,
// followed by a chunk of data of the given size.
// The end is marked with a chunk with size 0.

struct ChunkScanner {
	text string
mut:
	pos int
}

fn (mut s ChunkScanner) read(mut buf []u8) !int {
	if s.pos >= s.text.len {
		return io.Eof{}
	}
	max_bytes := 100
	end := if s.pos + max_bytes >= s.text.len { s.text.len } else { s.pos + max_bytes }
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
	sb := io.read_all(reader: reader_) or { ''.bytes() }
	return sb.bytestr()
}
