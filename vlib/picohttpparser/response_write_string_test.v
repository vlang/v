module picohttpparser

// Direct coverage for `Response.write_string`, the cursor primitive every other
// writer in `response.c.v` is built on. `response_builder_test.v` asserts the
// strings the helpers produce, which exercises `write_string` without asserting
// what it does to `buf` and `buf_start`; that is what this file pins down.

fn writer(buf &[128]u8) Response {
	return Response{
		buf_start: unsafe { &buf[0] }
		buf:       unsafe { &buf[0] }
	}
}

fn offset(mut r Response) int {
	return int(i64(r.buf) - i64(r.buf_start))
}

fn raw_bytes(mut r Response, n int) []u8 {
	mut out := []u8{cap: n}
	for i in 0 .. n {
		out << unsafe { *(r.buf_start + i) }
	}
	return out
}

fn test_write_string_appends_where_the_cursor_is() {
	mut heap_buf := [128]u8{}
	mut r := writer(&heap_buf)
	r.write_string('ab')
	r.write_string('cd')
	assert offset(mut r) == 4
	assert raw_bytes(mut r, 4) == [u8(`a`), `b`, `c`, `d`]
}

fn test_write_string_of_an_empty_string_copies_nothing() {
	mut heap_buf := [128]u8{}
	mut r := writer(&heap_buf)
	r.write_string('')
	assert offset(mut r) == 0
	// The buffer is untouched, so it still holds its zero fill.
	assert raw_bytes(mut r, 8) == [u8(0), 0, 0, 0, 0, 0, 0, 0]
}

fn test_write_string_does_not_move_buf_start() {
	mut heap_buf := [128]u8{}
	mut r := writer(&heap_buf)
	start := unsafe { &heap_buf[0] }
	r.write_string('GET / HTTP/1.1\r\n')
	assert offset(mut r) == 16
	assert unsafe { r.buf_start } == start
	// Every subsequent write still lands after the first one.
	r.write_string('Host: x\r\n')
	assert raw_bytes(mut r, 3) == [u8(`G`), `E`, `T`]
	assert offset(mut r) == 25
}

fn test_write_string_copies_bytes_not_just_printable_characters() {
	mut heap_buf := [128]u8{}
	mut r := writer(&heap_buf)
	// A backslash and a NUL are ordinary bytes as far as vmemcpy is concerned,
	// so the payload is built from bytes to get the NUL in at all.
	payload := [u8(`a`), `\\`, 0, `b`].bytestr()
	r.write_string(payload)
	assert offset(mut r) == 4
	bs := raw_bytes(mut r, 4)
	assert bs[0] == `a`
	assert bs[1] == `\\`
	assert bs[2] == 0
	assert bs[3] == `b`
	// NOTE: reading the buffer back through `tos` stops at the NUL, so the
	// string form is shorter than the offset. That is why the byte form is
	// asserted here.
	r.write_string('Z')
	assert offset(mut r) == 5
}

fn test_write_string_is_what_raw_writes() {
	mut heap_buf := [128]u8{}
	mut r := writer(&heap_buf)
	r.raw('HTTP/1.1 200 OK\r\n')
	assert offset(mut r) == 17
	// The cursor is now past the status line, and the bytes are the raw ones.
	// Index 8 is the space after the version, index 9 the first digit of 200.
	assert unsafe { *(r.buf_start + 8) } == ` `
	assert unsafe { *(r.buf_start + 9) } == `2`
	r.write_string('Tail')
	assert offset(mut r) == 21
	// The new write landed at the cursor, i.e. right after the raw response.
	assert unsafe { *(r.buf_start + 17) } == `T`
	assert unsafe { *(r.buf_start + 20) } == `l`
}
