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

pub fn new_buffered_writer(o BufferedWriterConfig) !&BufferedWriter {
	if o.cap < 1 {
		return error('`o.cap` must be a positive integer')
	}

	return &BufferedWriter{
		buf: []u8{len: o.cap}
		wr:  o.writer
	}
}

pub fn (mut b BufferedWriter) reset() {
	cap := b.buf.len
	b.buf = []u8{len: cap}
	b.n = 0
}

pub fn (b BufferedWriter) buffered() int {
	return b.n
}

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

pub fn (b BufferedWriter) available() int {
	return b.buf.len - b.n
}

pub fn (mut b BufferedWriter) write(src []u8) !int {
	mut p := src.clone()
	len := p.len
	mut nn := 0
	for len > b.available() {
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
