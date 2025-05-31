module chunked

import strings
// See: https://en.wikipedia.org/wiki/Chunked_transfer_encoding
// /////////////////////////////////////////////////////////////
// The chunk size is transferred as a hexadecimal number
// followed by \r\n as a line separator,
// followed by a chunk of data of the given size.
// The end is marked with a chunk with size 0.

struct ChunkScanner {
mut:
	pos  int
	text string
}

fn (mut s ChunkScanner) read_chunk_size() u32 {
	mut n := u32(0)
	for {
		if s.pos >= s.text.len {
			break
		}
		c := s.text[s.pos]
		if !c.is_hex_digit() {
			break
		}
		n = n << 4
		n += u32(unhex(c))
		s.pos++
	}
	return n
}

fn unhex(c u8) u8 {
	if `0` <= c && c <= `9` {
		return c - `0`
	} else if `a` <= c && c <= `f` {
		return c - `a` + 10
	} else if `A` <= c && c <= `F` {
		return c - `A` + 10
	}
	return 0
}

fn (mut s ChunkScanner) skip_crlf() {
	s.pos += 2
}

fn (mut s ChunkScanner) read_chunk(chunksize u32) !string {
	startpos := s.pos
	s.pos += int(chunksize)
	if s.pos > s.text.len {
		return error('invalid chunksize')
	}
	return s.text[startpos..s.pos]
}

pub fn decode(text string) !string {
	mut sb := strings.new_builder(100)
	mut cscanner := ChunkScanner{
		pos:  0
		text: text
	}
	for {
		csize := cscanner.read_chunk_size()
		if 0 == csize {
			break
		}
		cscanner.skip_crlf()
		ch := cscanner.read_chunk(csize)!
		sb.write_string(ch)
		cscanner.skip_crlf()
	}
	cscanner.skip_crlf()
	return sb.str()
}
