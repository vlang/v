module io

// LimitedReader reads at most n further bytes from an underlying reader;
// after that it reports Eof. It is the V-idiomatic equivalent of Go's
// io.LimitedReader.
@[heap]
pub struct LimitedReader {
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
	limit := if i64(buf.len) > l.n { int(l.n) } else { buf.len }
	// Borrow the caller's storage without cloning or changing its array header.
	mut view := unsafe { buf[..limit] }
	n := l.r.read(mut view)!
	validate_reader_count(n, limit)!
	l.n -= i64(n)
	return n
}

// TeeReader mirrors every byte read into a writer as well as returning it.
// Write failures surface through read, like Go's io.TeeReader.
@[heap]
pub struct TeeReader {
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
	validate_reader_count(n, buf.len)!
	if n > 0 {
		written := t.w.write(buf[..n])!
		if written != n {
			return error('io: tee_reader: short or invalid write')
		}
	}
	return n
}

// MultiReader concatenates several readers in order; when one is exhausted,
// the next takes over (Go's io.MultiReader).
@[heap]
pub struct MultiReader {
mut:
	readers []Reader
}

// multi_reader returns a reader over the concatenation of readers.
pub fn multi_reader(readers []Reader) MultiReader {
	return MultiReader{
		readers: readers.clone()
	}
}

// read implements Reader for MultiReader. Only Eof advances to the next reader;
// other errors and zero-byte reads leave the current reader in place.
pub fn (mut m MultiReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	for m.readers.len > 0 {
		n := m.readers[0].read(mut buf) or {
			if err is Eof {
				m.readers.delete(0)
				continue
			}
			return err
		}
		validate_reader_count(n, buf.len)!
		return n
	}
	return Eof{}
}

// SectionReader reads a fixed window of an underlying reader and then
// reports Eof, without the RandomReader tracking of Go's io.SectionReader
// (read-only direction).
@[heap]
pub struct SectionReader {
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
	remaining := s.count - s.off
	limit := if i64(buf.len) > remaining { int(remaining) } else { buf.len }
	// Borrow the caller's storage without cloning or changing its array header.
	mut view := unsafe { buf[..limit] }
	n := s.r.read(mut view)!
	validate_reader_count(n, limit)!
	s.off += i64(n)
	return n
}

// read_byte returns the next byte from a reader, or Eof at the end of the
// stream. It is the V-idiomatic equivalent of Go's ReadByte default.
pub fn read_byte(mut r Reader) !u8 {
	mut b := []u8{len: 1}
	n := r.read(mut b)!
	validate_reader_count(n, 1)!
	if n == 0 {
		return Eof{}
	}
	return b[0]
}

// read_full fills buf completely from r, returning Eof when the stream ends
// before buf is full. A successful zero-byte read returns a no-progress error.
pub fn read_full(mut r Reader, mut buf []u8) ! {
	mut n := 0
	for n < buf.len {
		remaining := buf.len - n
		// Borrow the unread suffix so writes fill the original caller buffer.
		mut view := unsafe { buf[n..] }
		read := r.read(mut view)!
		validate_reader_count(read, remaining)!
		if read == 0 {
			return error('io: read_full: no progress')
		}
		n += read
	}
}

// read_byte returns the next byte from a buffered reader, or Eof at the end
// of the stream (like bufio.Reader.ReadByte).
pub fn (mut b BufferedReader) read_byte() !u8 {
	b.can_unread_byte = false
	if b.offset >= b.len {
		if !b.fill_buffer() {
			return Eof{}
		}
	}
	ch := b.buf[b.offset]
	b.offset++
	b.total_read++
	b.can_unread_byte = true
	return ch
}

// unread_byte returns the last-read byte to the buffer, so the next read
// returns it again. Only one unread after a successful read_byte is allowed;
// other read operations, peeking, and failed reads invalidate that permission.
pub fn (mut b BufferedReader) unread_byte() ! {
	if !b.can_unread_byte || b.offset <= 0 {
		return error('io: unread_byte: no byte to unread')
	}
	b.offset--
	b.total_read--
	b.can_unread_byte = false
}

fn validate_reader_count(n int, size int) ! {
	if n < 0 || n > size {
		return error('io: invalid read count')
	}
}
