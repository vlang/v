module io

// LimitedReader reads at most n further bytes from an underlying reader;
// after that it reports Eof. It is the V-idiomatic equivalent of Go's
// io.LimitedReader.
@[heap]
pub struct LimitedReader {
	Reader
mut:
	r Reader
	n i64
}

// limited_reader wraps r so that at most n further bytes can be read.
pub fn limited_reader(r Reader, n i64) LimitedReader {
	return LimitedReader{
		r: r
		n: n
	}
}

// read implements Reader for LimitedReader.
pub fn (mut l LimitedReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	if l.n <= 0 {
		return Eof{}
	}
	if i64(buf.len) > l.n {
		buf = buf[..int(l.n)]
	}
	n := l.r.read(mut buf)!
	l.n -= i64(n)
	return n
}

// TeeReader mirrors every byte read into a writer as well as returning it.
// Write failures surface through read, like Go's io.TeeReader.
@[heap]
pub struct TeeReader {
	Reader
mut:
	r Reader
	w Writer
}

// tee_reader wraps r so that everything read from it is also written to w.
pub fn tee_reader(r Reader, w Writer) TeeReader {
	return TeeReader{
		r: r
		w: w
	}
}

// read implements Reader for TeeReader.
pub fn (mut t TeeReader) read(mut buf []u8) !int {
	n := t.r.read(mut buf)!
	if n > 0 {
		t.w.write(buf[..n]) or { return err }
	}
	return n
}

// MultiReader concatenates several readers in order; when one is exhausted,
// the next takes over (Go's io.MultiReader).
@[heap]
pub struct MultiReader {
	Reader
mut:
	readers []Reader
}

// multi_reader returns a reader over the concatenation of readers.
pub fn multi_reader(readers []Reader) MultiReader {
	return MultiReader{
		readers: readers.clone()
	}
}

// read implements Reader for MultiReader: an exhausted (or failing) reader is
// skipped, and once every reader is done its last error is returned.
pub fn (mut m MultiReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	for m.readers.len > 0 {
		n := m.readers[0].read(mut buf) or {
			first_err := err
			m.readers.delete(0)
			if m.readers.len == 0 {
				return first_err
			}
			continue
		}
		if n > 0 {
			return n
		}
		m.readers.delete(0)
	}
	return Eof{}
}

// SectionReader reads a fixed window of an underlying reader and then
// reports Eof, without the RandomReader tracking of Go's io.SectionReader
// (read-only direction).
@[heap]
pub struct SectionReader {
	Reader
mut:
	r     Reader
	count i64
	off   i64
}

// section_reader reads exactly `count` bytes from r, then reports Eof.
pub fn section_reader(r Reader, count i64) SectionReader {
	return SectionReader{
		r:     r
		count: count
	}
}

// read implements Reader for SectionReader.
pub fn (mut s SectionReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	if s.off >= s.count {
		return Eof{}
	}
	if i64(buf.len) > s.count - s.off {
		buf = buf[..int(s.count - s.off)]
	}
	n := s.r.read(mut buf)!
	s.off += i64(n)
	return n
}

// read_byte returns the next byte from a reader, or Eof at the end of the
// stream. It is the V-idiomatic equivalent of Go's ReadByte default.
pub fn read_byte(mut r Reader) !u8 {
	mut b := []u8{len: 1}
	n := r.read(mut b)!
	if n == 0 {
		return Eof{}
	}
	return b[0]
}

// read_full fills buf completely from r, returning Eof when the stream ends
// before buf is full (Go's io.ReadFull).
pub fn read_full(mut r Reader, mut buf []u8) ! {
	mut n := 0
	for n < buf.len {
		read := r.read(mut buf[n..])!
		if read == 0 {
			return Eof{}
		}
		n += read
	}
}

// read_byte returns the next byte from a buffered reader, or Eof at the end
// of the stream (like bufio.Reader.ReadByte).
pub fn (mut b BufferedReader) read_byte() !u8 {
	if b.offset >= b.len {
		if !b.fill_buffer() {
			return Eof{}
		}
	}
	ch := b.buf[b.offset]
	b.offset++
	return ch
}

// unread_byte returns the last-read byte to the buffer, so the next read
// returns it again. It errors when the buffer holds nothing to unread
// (like bufio.Reader.UnreadByte).
pub fn (mut b BufferedReader) unread_byte() ! {
	if b.offset <= 0 {
		return error('io: unread_byte: no byte to unread')
	}
	b.offset--
}
