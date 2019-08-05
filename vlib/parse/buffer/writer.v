package buffer

// Writer implements an io.Writer over a byte slice.
struct Writer {
	buf []byte
}

// new_writer returns a new Writer for a given byte slice.
fn new_writer(buf []byte) *Writer {
	return &Writer{
		buf: buf,
	}
}

// write writes bytes from the given byte slice and returns the number of bytes written and an error if occurred. When err != nil, n == 0.
fn (w &Writer) write(b []byte) ?int {
	n := len(b)
	end := len(w.buf)
	if end+n > cap(w.buf) {
		// buf := make([]byte, end, 2*cap(w.buf)+n)
		buf := make([]byte, end, 2*cap(w.buf)+n)
		copy(buf, w.buf)
		w.buf = buf
	}
	w.buf = w.buf[:end+n]
	return copy(w.buf[end:], b)
}

// len returns the length of the underlying byte slice.
fn (w &Writer) len() int {
	return len(w.buf)
}

// bytes returns the underlying byte slice.
fn (w &Writer) bytes() []byte {
	return w.buf
}

// reset empties and reuses the current buffer. Subsequent writes will overwrite the buffer, so any reference to the underlying slice is invalidated after this call.
fn (w &Writer) reset() {
	// w.buf = w.buf[:0]
	w.buf = w.buf.left(0)
}
