module io

// ShortReader returns at most 2 bytes per read call, exercising the
// short-read looping of read_exact.
struct ShortReader {
mut:
	data []u8
}

fn (mut r ShortReader) read(mut buf []u8) !int {
	if r.data.len == 0 {
		return Eof{}
	}
	want := if buf.len < 2 { buf.len } else { 2 }
	n := if r.data.len < want { r.data.len } else { want }
	for i in 0 .. n {
		buf[i] = r.data[i]
	}
	r.data = r.data[n..]
	return n
}

fn test_read_exact_fills_across_short_reads() {
	mut r := ShortReader{
		data: 'hello world'.bytes()
	}
	mut buf := []u8{len: 11}
	read_exact(mut r, mut buf)!
	assert buf.bytestr() == 'hello world'
}

fn test_read_exact_errors_on_short_stream() {
	mut r := ShortReader{
		data: 'hi'.bytes()
	}
	mut buf := []u8{len: 5}
	read_exact(mut r, mut buf) or {
		assert err is Eof, 'short stream must surface Eof'
		return
	}
	assert false, 'read_exact must error on a short stream'
}

fn test_read_exact_empty_buffer_succeeds() {
	mut r := ShortReader{
		data: 'hi'.bytes()
	}
	mut buf := []u8{len: 0}
	read_exact(mut r, mut buf)!
	assert r.data.len == 2, 'empty read must not consume input'
}

struct ExactCountReader {
mut:
	counts []int
}

fn (mut r ExactCountReader) read(mut buf []u8) !int {
	n := r.counts[0]
	r.counts.delete(0)
	return n
}

fn test_read_exact_rejects_oversized_later_read() {
	mut r := ExactCountReader{
		counts: [1, 3]
	}
	mut buf := []u8{len: 3}
	read_exact(mut r, mut buf) or {
		assert err.msg() == 'io.read_exact: reader returned a read count larger than the buffer (3)'
		return
	}
	assert false, 'oversized read must not be accepted as a filled buffer'
}

fn test_read_exact_rejects_negative_read() {
	mut r := ExactCountReader{
		counts: [-1]
	}
	mut buf := []u8{len: 3}
	read_exact(mut r, mut buf) or {
		assert err.msg() == 'io.read_exact: reader returned a negative read count (-1)'
		return
	}
	assert false, 'negative read must be rejected'
}

fn test_read_exact_zero_read_is_eof() {
	mut r := ExactCountReader{
		counts: [0]
	}
	mut buf := []u8{len: 3}
	read_exact(mut r, mut buf) or {
		assert err is Eof
		return
	}
	assert false, 'zero read must return Eof'
}
