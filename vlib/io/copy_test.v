module io

struct MemReader {
mut:
	data     []u8
	pos      int
	max_read int
	reads    int
}

fn (mut r MemReader) read(mut buf []u8) !int {
	r.reads++
	if r.pos >= r.data.len {
		return Eof{}
	}
	mut n := if buf.len < r.data.len - r.pos { buf.len } else { r.data.len - r.pos }
	if r.max_read > 0 && n > r.max_read {
		n = r.max_read
	}
	for i in 0 .. n {
		buf[i] = r.data[r.pos + i]
	}
	r.pos += n
	return n
}

struct MemWriter {
mut:
	data      []u8
	max_write int
	writes    int
}

fn (mut w MemWriter) write(buf []u8) !int {
	w.writes++
	n := if w.max_write > 0 && buf.len > w.max_write { w.max_write } else { buf.len }
	w.data << buf[..n]
	return n
}

fn test_copy() {
	mut src := MemReader{ data: 'hello world'.bytes() }
	mut dst := MemWriter{}
	n := copy(mut dst, mut src)!
	assert n == 11
	assert dst.data.bytestr() == 'hello world'
}

fn test_copy_n() {
	mut src := MemReader{ data: 'hello world'.bytes() }
	mut dst := MemWriter{}
	n := copy_n(mut dst, mut src, 5)!
	assert n == 5
	assert dst.data.bytestr() == 'hello'
}

fn test_copy_n_unexpected_eof() {
	mut src := MemReader{ data: 'hi'.bytes() }
	mut dst := MemWriter{}
	if _ := copy_n(mut dst, mut src, 5) {
		assert false
	} else {
		assert err.msg().contains('unexpected EOF')
	}
}

fn test_copy_buffer() {
	mut src := MemReader{ data: 'abc'.bytes() }
	mut dst := MemWriter{}
	n := copy_buffer(mut dst, mut src, mut []u8{len: 2})!
	assert n == 3
	assert dst.data.bytestr() == 'abc'
}

// Exercise the default, bounded and caller-buffered entry points with the same
// reader/writer behavior. The bounded variant copies four bytes.
fn run_copy_variant(variant int, mut dst Writer, mut src Reader) !i64 {
	if variant == 0 {
		return copy(mut dst, mut src)
	}
	if variant == 1 {
		return copy_n(mut dst, mut src, 4)
	}
	mut buf := []u8{len: 2}
	return copy_buffer(mut dst, mut src, mut buf)
}

fn test_copy_short_reads_and_writes() {
	for variant in 0 .. 3 {
		mut src := MemReader{ data: 'abcdef'.bytes(), max_read: 3 }
		mut dst := MemWriter{ max_write: 2 }
		n := run_copy_variant(variant, mut dst, mut src)!
		expected := if variant == 1 { 'abcd' } else { 'abcdef' }
		assert n == expected.len
		assert dst.data.bytestr() == expected
		assert src.pos == expected.len
		assert dst.writes > 1
	}
}

fn test_copy_n_multiple_buffers_and_source_remainder() {
	data := []u8{len: 32 * 1024 + 5, init: u8(index % 251)}
	mut src := MemReader{ data: data }
	mut dst := MemWriter{ max_write: 11 }
	n := data.len - 2
	assert copy_n(mut dst, mut src, n)! == n
	assert dst.data == data[..n]
	assert src.pos == n
	mut rest := []u8{len: 2}
	assert src.read(mut rest)! == 2
	assert rest == data[n..]
}

fn test_copy_n_nonpositive_is_noop() {
	for n in [i64(0), -1] {
		mut src := MemReader{ data: 'abc'.bytes() }
		mut dst := MemWriter{}
		assert copy_n(mut dst, mut src, n)! == 0
		assert src.reads == 0
		assert dst.writes == 0
	}
}

fn test_copy_empty_source() {
	mut src := MemReader{}
	mut dst := MemWriter{}
	assert copy(mut dst, mut src)! == 0
	assert dst.writes == 0
	mut buf := []u8{len: 3}
	assert copy_buffer(mut dst, mut src, mut buf)! == 0
	assert dst.writes == 0
}

fn test_copy_buffer_default_and_reuse() {
	mut src := MemReader{ data: 'abc'.bytes() }
	mut dst := MemWriter{}
	mut buf := []u8{}
	assert copy_buffer(mut dst, mut src, mut buf)! == 3
	assert buf.len == 32 * 1024
	assert dst.data.bytestr() == 'abc'
	mut short_src := MemReader{ data: 'abcdef'.bytes() }
	mut short_dst := MemWriter{}
	mut short_buf := []u8{len: 2}
	assert copy_buffer(mut short_dst, mut short_src, mut short_buf)! == 6
	assert short_buf.len == 2
	assert short_dst.data.bytestr() == 'abcdef'
}

struct CopyCountReader {
	count int
mut:
	calls int
}

fn (mut r CopyCountReader) read(mut _buf []u8) !int {
	r.calls++
	if r.calls > 1 {
		return Eof{}
	}
	return r.count
}

fn test_copy_rejects_invalid_read_counts() {
	for variant in 0 .. 3 {
		for count in [-1, 32 * 1024 + 1] {
			mut src := CopyCountReader{ count: count }
			mut dst := MemWriter{}
			if _ := run_copy_variant(variant, mut dst, mut src) {
				assert false, 'invalid read count must fail'
			} else {
				assert err.msg() == 'io: invalid read count'
			}
			assert src.calls == 1
			assert dst.writes == 0
		}
	}
}

fn test_copy_n_rejects_reads_beyond_remaining_limit() {
	mut src := CopyCountReader{ count: 3 }
	mut dst := MemWriter{}
	if _ := copy_n(mut dst, mut src, 2) {
		assert false, 'copy_n must reject a count beyond its bounded read buffer'
	} else {
		assert err.msg() == 'io: invalid read count'
	}
	assert dst.writes == 0
}

fn test_copy_zero_read_is_not_eof() {
	for variant in 0 .. 3 {
		mut src := CopyCountReader{}
		mut dst := MemWriter{}
		if _ := run_copy_variant(variant, mut dst, mut src) {
			assert false, 'a zero-byte read must not report a completed stream'
		} else {
			assert err.msg().contains('no progress')
			assert err !is Eof
		}
		assert src.calls == 1
		assert dst.writes == 0
	}
}

struct CopyCountWriter {
	count int
}

fn (mut w CopyCountWriter) write(_buf []u8) !int {
	return w.count
}

fn test_copy_rejects_invalid_write_counts() {
	for variant in 0 .. 3 {
		for count in [-1, 0, 5] {
			mut src := MemReader{ data: 'abcd'.bytes() }
			mut dst := CopyCountWriter{ count: count }
			if _ := run_copy_variant(variant, mut dst, mut src) {
				assert false, 'invalid write count must fail'
			} else {
				assert err.msg().contains('write')
			}
		}
	}
}

struct CopyErrorReader {
	immediate bool
mut:
	calls int
}

fn (mut r CopyErrorReader) read(mut buf []u8) !int {
	r.calls++
	if !r.immediate && r.calls == 1 {
		buf[0] = `a`
		buf[1] = `b`
		return 2
	}
	return error_with_code('copy reader failure', 91)
}

fn test_copy_preserves_reader_errors() {
	for variant in 0 .. 3 {
		for immediate in [true, false] {
			mut src := CopyErrorReader{ immediate: immediate }
			mut dst := MemWriter{}
			if _ := run_copy_variant(variant, mut dst, mut src) {
				assert false, 'reader error must propagate'
			} else {
				assert err.msg() == 'copy reader failure'
				assert err.code() == 91
			}
			assert dst.data.bytestr() == if immediate { '' } else { 'ab' }
		}
	}
}

struct CopyErrorWriter {
mut:
	data []u8
}

fn (mut w CopyErrorWriter) write(buf []u8) !int {
	if w.data.len == 0 {
		w.data << buf[0]
		return 1
	}
	return error_with_code('copy writer failure', 92)
}

fn test_copy_preserves_writer_errors_after_partial_write() {
	for variant in 0 .. 3 {
		mut src := MemReader{ data: 'abcdef'.bytes() }
		mut dst := CopyErrorWriter{}
		if _ := run_copy_variant(variant, mut dst, mut src) {
			assert false, 'writer error must propagate'
		} else {
			assert err.msg() == 'copy writer failure'
			assert err.code() == 92
		}
		assert dst.data.bytestr() == 'a'
	}
}

struct CopyEofWriter {}

fn (mut w CopyEofWriter) write(_buf []u8) !int {
	return Eof{}
}

fn test_copy_writer_eof_is_an_error() {
	for variant in 0 .. 3 {
		mut src := MemReader{ data: 'abcdef'.bytes() }
		mut dst := CopyEofWriter{}
		if _ := run_copy_variant(variant, mut dst, mut src) {
			assert false, 'only reader Eof can complete a copy'
		} else {
			assert err is Eof
		}
	}
}
