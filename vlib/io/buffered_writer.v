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

	n := b.wr.write(b.buf[0..b.n])!
	if n < b.n {
		return error('Writer accepted less bytes than expected without returning any explicit error.')
	}

	b.n = 0
	return
}

// available returns the amount of available space left in the buffer.
pub fn (b BufferedWriter) available() int {
	return b.buf.len - b.n
}

// write writes `src` in the buffer, flushing it to the underlying writer as needed, and returns the
// number of bytes written.
pub fn (mut b BufferedWriter) write(src []u8) !int {
	mut p := src.clone()
	mut nn := 0
	for p.len > b.available() {
		mut n := 0
		if b.buffered() == 0 {
			n = b.wr.write(p)!
		} else {
			n = copy(mut b.buf[b.n..], p)
			b.n += n
			b.flush()!
		}
		nn += n
		p = p[n..].clone()
	}

	n := copy(mut b.buf[b.n..], p)
	b.n += n
	nn += n
	return nn
}
