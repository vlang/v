module buffer


// Reader implements an io.Reader over a byte slice.
struct Reader {
	buf []byte
	pos int
}

// new_reader returns a new Reader for a given byte slice.
fn new_reader(buf []byte) *Reader {
	return &Reader{
		buf: buf,
	}
}

// read reads bytes into the given byte slice and returns the number of bytes read and an error if occurred.
fn (r &Reader) read(b []byte) ?int {
	if len(b) == 0 {
		return 0
	}
	if r.pos >= len(r.buf) {
		// return 0, io.EOF
		return error('EOF')
	}
	n = copy(b, r.buf[r.pos:])
	r.pos += n
	
	return n
}

// bytes returns the underlying byte slice.
fn (r &Reader) bytes() []byte {
	return r.buf
}

// reset resets the position of the read pointer to the beginning of the underlying byte slice.
fn (r &Reader) reset() {
	r.pos = 0
}

// len returns the length of the buffer.
fn (r &Reader) len() int {
	return len(r.buf)
}
