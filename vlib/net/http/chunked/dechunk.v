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

fn (mut s ChunkScanner) read_chunk_size() !u32 {
	mut n := u64(0)
	mut digits := 0
	for {
		if s.pos >= s.text.len {
			break
		}
		c := s.text[s.pos]
		if !c.is_hex_digit() {
			break
		}
		n = n << 4 | u64(unhex(c))
		// a size that does not fit u32 could never be satisfied from the
		// buffer; reject it here instead of wrapping around silently (a
		// wrap to 0 would even look like the terminating chunk)
		if n > u64(0xFFFF_FFFF) {
			return error('invalid chunk size: overflow')
		}
		s.pos++
		digits++
	}
	// no digits at all is malformed input, not the zero-size terminator
	if digits == 0 {
		return error('invalid chunk size: expected hexadecimal number')
	}
	return u32(n)
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

fn (mut s ChunkScanner) read_crlf() ! {
	if s.pos + 2 > s.text.len || s.text[s.pos..s.pos + 2] != '\r\n' {
		return error('invalid chunk: expected CRLF')
	}
	s.pos += 2
}

fn (mut s ChunkScanner) read_size_line_end() ! {
	// Chunk extensions are ignored, but the complete size line still needs CRLF.
	if s.pos < s.text.len && s.text[s.pos] == `;` {
		for s.pos < s.text.len && s.text[s.pos] != `\r` && s.text[s.pos] != `\n` {
			s.pos++
		}
	}
	s.read_crlf()!
}

fn (mut s ChunkScanner) read_chunk(chunksize u32) !string {
	if u64(chunksize) > u64(s.text.len - s.pos) {
		return error('invalid chunksize')
	}
	startpos := s.pos
	s.pos += int(chunksize)
	return s.text[startpos..s.pos]
}

// decode returns the body of a chunked message and rejects malformed sizes and chunk separators.
// Chunk extensions are ignored.
pub fn decode(text string) !string {
	mut sb := strings.new_builder(100)
	mut cscanner := ChunkScanner{
		pos:  0
		text: text
	}
	for {
		csize := cscanner.read_chunk_size()!
		cscanner.read_size_line_end()!
		if 0 == csize {
			break
		}
		ch := cscanner.read_chunk(csize)!
		sb.write_string(ch)
		cscanner.read_crlf()!
	}
	return sb.str()
}
