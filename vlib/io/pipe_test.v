module io

fn test_pipe_roundtrip() {
	r, w := pipe()
	spawn fn (w &PipeWriter) {
		w.write('hello'.bytes()) or {}
		w.close()
	}(w)
	data := read_all(ReadAllConfig{ reader: r, read_to_end_of_stream: true }) or { panic(err) }
	assert data.bytestr() == 'hello'
	r.close()
}

fn test_pipe_write_after_reader_close() {
	r, w := pipe()
	r.close()
	if _ := w.write('x'.bytes()) {
		assert false
	} else {
		assert true
	}
}

fn test_pipe_close_with_error() {
	r, w := pipe()
	spawn fn (w &PipeWriter) {
		w.close_with_error(error('boom'))
	}(w)
	mut buf := []u8{len: 10}
	if _ := r.read(mut buf) {
		assert false
	} else {
		assert err.msg() == 'boom'
	}
}
