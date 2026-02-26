module io

pub struct BufferedWriter {
mut:
	n  int
	wr Writer
pub mut:
	buf []u8
}

@[params]
pub struct BufferedWriterConfig {
pub:
	writer Writer
	cap    int = 128 * 1024
}

// new_buffered_writer creates a new BufferedWriter with the specified BufferedWriterConfig.
// Returns an error when cap is 0 or negative.
pub fn new_buffered_writer(o BufferedWriterConfig) !&BufferedWriter {
	if o.cap < 1 {
		return error('`o.cap` must be a positive integer')
	}

	return &BufferedWriter{
		buf: []u8{len: o.cap}
		wr:  o.writer
	}
}

// reset resets the buffer to its initial state.
pub fn (mut b BufferedWriter) reset() {
	cap := b.buf.len
	b.buf = []u8{len: cap}
	b.n = 0
}

// buffered returns the number of bytes currently stored in the buffer.
pub fn (b BufferedWriter) buffered() int {
	return b.n
}

// flush writes the buffered data to the underlying writer and clears the buffer, ensures all data is
// written.
// Returns an error if the writer fails to write all buffered data.
pub fn (mut b BufferedWriter) flush() ! {
	if b.buffered() == 0 {
		return
	}
	mut written := 0
	for written < b.n {
		n := b.wr.write(b.buf[written..b.n]) or {
			b.shift_unwritten_to_front(written)
			return err
		}
		if n <= 0 || n > b.n - written {
			b.shift_unwritten_to_front(written)
			return error('writer returned an invalid number of bytes while flushing')
		}
		written += n
	}
	b.n = 0
}

// available returns the amount of available space left in the buffer.
pub fn (b BufferedWriter) available() int {
	return b.buf.len - b.n
}

fn (mut b BufferedWriter) shift_unwritten_to_front(written int) {
	if written <= 0 {
		return
	}
	remaining := b.n - written
	if remaining <= 0 {
		b.n = 0
		return
	}
	copy(mut b.buf[..remaining], b.buf[written..b.n])
	b.n = remaining
}

// write writes `src` in the buffer, flushing it to the underlying writer as needed, and returns the
// number of bytes written.
pub fn (mut b BufferedWriter) write(src []u8) !int {
	mut written := 0
	for written < src.len {
		remaining := src.len - written
		if b.buffered() == 0 && remaining > b.available() {
			n := b.wr.write(src[written..])!
			if n <= 0 || n > remaining {
				return error('writer returned an invalid number of bytes while writing')
			}
			written += n
			continue
		}
		n := copy(mut b.buf[b.n..], src[written..])
		b.n += n
		written += n
		if b.available() == 0 {
			b.flush()!
		}
	}
	return written
}
