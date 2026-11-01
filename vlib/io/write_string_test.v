module io

struct MemWriter2 {
mut:
	data  []u8
	calls int
}

fn (mut w MemWriter2) write(buf []u8) !int {
	w.calls++
	w.data << buf
	return buf.len
}

fn test_write_string() {
	mut w := MemWriter2{}
	n := write_string(mut w, 'hello')!
	assert n == 5
	assert w.data.bytestr() == 'hello'
	assert w.calls == 1
}

fn test_write_string_preserves_utf8_and_nul_bytes() {
	mut w := MemWriter2{}
	text := 'V\x00é🙂'
	assert write_string(mut w, text)! == text.len
	assert w.data == text.bytes()
	assert w.calls == 1
}

fn test_write_string_passes_empty_input_to_writer() {
	mut w := MemWriter2{}
	assert write_string(mut w, '')! == 0
	assert w.data.len == 0
	assert w.calls == 1
}

struct WriteStringShortWriter {
mut:
	data  []u8
	calls int
}

fn (mut w WriteStringShortWriter) write(buf []u8) !int {
	w.calls++
	w.data << buf[..2]
	return 2
}

fn test_write_string_returns_short_write_without_retry() {
	mut w := WriteStringShortWriter{}
	assert write_string(mut w, 'hello')! == 2
	assert w.data == 'he'.bytes()
	assert w.calls == 1
}

struct WriteStringErrorWriter {
mut:
	calls int
}

fn (mut w WriteStringErrorWriter) write(_ []u8) !int {
	w.calls++
	return error_with_code('write blocked', 73)
}

fn test_write_string_propagates_writer_error() {
	mut w := WriteStringErrorWriter{}
	write_string(mut w, 'hello') or {
		assert err.msg() == 'write blocked'
		assert err.code() == 73
		assert w.calls == 1
		return
	}
	assert false, 'the writer error must propagate'
}
