module iotest

import io

// OneByteReader returns a Reader that reads at most one byte at a time
// from the wrapped reader. It is useful for testing code that must cope
// with short reads.
pub struct OneByteReader {
mut:
	inner io.Reader
}

pub fn one_byte_reader(r io.Reader) OneByteReader {
	return OneByteReader{ inner: r }
}

pub fn (mut r OneByteReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	mut one := []u8{len: 1}
	n := r.inner.read(mut one)!
	if n == 0 {
		return 0
	}
	buf[0] = one[0]
	return 1
}

// HalfReader returns a Reader that reads at most half the requested
// bytes (at least one) per call, exercising short-read handling.
pub struct HalfReader {
mut:
	inner io.Reader
}

pub fn half_reader(r io.Reader) HalfReader {
	return HalfReader{ inner: r }
}

pub fn (mut r HalfReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	n := if buf.len == 1 { 1 } else { buf.len / 2 }
	mut sub := buf[..n]
	return r.inner.read(mut sub)
}

// DataErrReader wraps a reader together with data and an error: reads
// return the data first, then the error on subsequent calls.
pub struct DataErrReader {
mut:
	inner io.Reader
	data  []u8
	err   IError
	fused bool
}

pub fn data_err_reader(r io.Reader, data []u8, err IError) DataErrReader {
	return DataErrReader{ inner: r, data: data.clone(), err: err }
}

pub fn (mut r DataErrReader) read(mut buf []u8) !int {
	if !r.fused && r.data.len > 0 {
		n := if buf.len < r.data.len { buf.len } else { r.data.len }
		for i in 0 .. n {
			buf[i] = r.data[i]
		}
		r.data = r.data[n..].clone()
		return n
	}
	r.fused = true
	n := r.inner.read(mut buf) or { return r.err }
	if n == 0 {
		return r.err
	}
	return n
}

// ErrReader returns a Reader that always fails with `err`.
pub struct ErrReader {
	err IError
}

pub fn err_reader(err IError) ErrReader {
	return ErrReader{ err: err }
}

pub fn (mut r ErrReader) read(mut buf []u8) !int {
	return r.err
}

// OneByteWriter returns a Writer that writes at most one byte per call.
pub struct OneByteWriter {
mut:
	inner io.Writer
}

pub fn one_byte_writer(w io.Writer) OneByteWriter {
	return OneByteWriter{ inner: w }
}

pub fn (mut w OneByteWriter) write(buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	return w.inner.write(buf[..1])
}

// HalfWriter returns a Writer that writes at most half the bytes
// (at least one) per call.
pub struct HalfWriter {
mut:
	inner io.Writer
}

pub fn half_writer(w io.Writer) HalfWriter {
	return HalfWriter{ inner: w }
}

pub fn (mut w HalfWriter) write(buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	n := if buf.len == 1 { 1 } else { buf.len / 2 }
	return w.inner.write(buf[..n])
}

// ErrWriter returns a Writer that always fails with `err`.
pub struct ErrWriter {
	err IError
}

pub fn err_writer(err IError) ErrWriter {
	return ErrWriter{ err: err }
}

pub fn (mut w ErrWriter) write(buf []u8) !int {
	return w.err
}

// TruncateWriter writes to the inner writer but reports `n` as the
// number of bytes written, simulating a short write.
pub struct TruncateWriter {
mut:
	inner io.Writer
	n     int
}

pub fn truncate_writer(w io.Writer, n int) TruncateWriter {
	return TruncateWriter{ inner: w, n: n }
}

pub fn (mut w TruncateWriter) write(buf []u8) !int {
	m := if buf.len < w.n { buf.len } else { w.n }
	if m == 0 {
		return 0
	}
	w.inner.write(buf[..m])!
	return m
}
