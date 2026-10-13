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
