module iotest

import io

struct SliceReader {
	data []u8
mut:
	pos int
}

fn (mut r SliceReader) read(mut buf []u8) !int {
	if r.pos >= r.data.len {
		return io.Eof{}
	}
	n := if buf.len < r.data.len - r.pos { buf.len } else { r.data.len - r.pos }
	for i in 0 .. n {
		buf[i] = r.data[r.pos + i]
	}
	r.pos += n
	return n
}

struct SharedBuf {
mut:
	data []u8
}

struct SliceWriter {
mut:
	shared &SharedBuf
}

fn (mut w SliceWriter) write(buf []u8) !int {
	w.shared.data << buf
	return buf.len
}

fn test_one_byte_reader() {
	mut r := one_byte_reader(SliceReader{ data: 'hello'.bytes() })
	mut out := []u8{}
	for {
		mut buf := []u8{len: 4}
		n := r.read(mut buf) or { break }
		if n == 0 {
			break
		}
		assert n == 1
		out << buf[..n]
	}
	assert out.bytestr() == 'hello'
}

fn test_half_reader() {
	mut r := half_reader(SliceReader{ data: 'hello!'.bytes() })
	mut out := []u8{}
	for {
		mut buf := []u8{len: 6}
		n := r.read(mut buf) or { break }
		if n == 0 {
			break
		}
		assert n <= 3
		out << buf[..n]
	}
	assert out.bytestr() == 'hello!'
}

fn test_data_err_reader() {
	mut r := data_err_reader(SliceReader{ data: []u8{} }, 'xy'.bytes(), error('boom'))
	mut buf := []u8{len: 8}
	n := r.read(mut buf)!
	assert n == 2
	assert buf[..n].bytestr() == 'xy'
	if _ := r.read(mut buf) {
		assert false, 'expected boom'
	} else {
		assert err.msg() == 'boom'
	}
}

fn test_err_reader() {
	mut r := err_reader(error('nope'))
	mut buf := []u8{len: 4}
	if _ := r.read(mut buf) {
		assert false, 'expected error'
	} else {
		assert err.msg() == 'nope'
	}
}

fn test_one_byte_writer() {
	shared := &SharedBuf{}
	mut w := one_byte_writer(SliceWriter{ shared: shared })
	mut writer := io.Writer(w)
	n := writer.write('hey'.bytes())!
	assert n == 1
	assert shared.data.bytestr() == 'h'
}

fn test_half_writer() {
	shared := &SharedBuf{}
	mut w := half_writer(SliceWriter{ shared: shared })
	mut writer := io.Writer(w)
	n := writer.write('hey!'.bytes())!
	assert n == 2
	assert shared.data.bytestr() == 'he'
}

fn test_err_writer() {
	mut w := err_writer(error('wfail'))
	mut writer := io.Writer(w)
	if _ := writer.write('hey'.bytes()) {
		assert false, 'expected error'
	} else {
		assert err.msg() == 'wfail'
	}
}

fn test_truncate_writer() {
	shared := &SharedBuf{}
	mut w := truncate_writer(SliceWriter{ shared: shared }, 2)
	mut writer := io.Writer(w)
	n := writer.write('hey!'.bytes())!
	assert n == 2
	assert shared.data.bytestr() == 'he'
}
