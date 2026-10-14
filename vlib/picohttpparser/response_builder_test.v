module picohttpparser

// Coverage for the response writer in response.c.v, which builds a response
// into a caller-supplied buffer. `end()` is not exercised here because it calls
// C.send on `fd`; see response_sigpipe_test.c.v for that path.

fn response_at(ptr &u8) Response {
	return Response{
		buf_start: ptr
		buf:       ptr
	}
}

fn response_with_date(ptr &u8, date_ptr &u8) Response {
	return Response{
		buf_start: ptr
		buf:       ptr
		date:      date_ptr
	}
}

fn written(mut r Response) string {
	n := int(i64(r.buf) - i64(r.buf_start))
	return unsafe { tos(r.buf_start, n) }
}

fn written_len(mut r Response) int {
	return int(i64(r.buf) - i64(r.buf_start))
}

pub fn test_response_writes_a_full_response() {
	mut buf := [512]u8{}
	mut r := response_at(unsafe { &buf[0] })
	r.http_ok()
	r.header('Content-Type', 'text/plain')
	r.body('hello')
	r.http_404()
	r.http_405()
	r.http_500()
	r.raw('RAW')

	assert written(mut r) == 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\nhello' +
		'HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n' +
		'HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\n\r\n' +
		'HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n' + 'RAW', 'written: ${written(mut r)}'
	assert written_len(mut r) == 228, 'len ${written_len(mut r)}'
}

pub fn test_response_chains_and_writes_default_headers() {
	mut buf := [256]u8{}
	mut date := [40]u8{}
	for i in 0 .. 29 {
		date[i] = `7`
	}
	mut r := response_with_date(unsafe { &buf[0] }, unsafe { &date[0] })
	r.http_ok().header_server().content_type('application/json').html()
	r.header_date()

	assert written(mut r) == 'HTTP/1.1 200 OK\r\nServer: V\r\nContent-Type: application/json\r\nContent-Type: text/html\r\nDate: 77777777777777777777777777777\r\n', 'written: ${written(mut r)}'
	assert written_len(mut r) == 122, 'len ${written_len(mut r)}'
}

pub fn test_response_body_writes_the_content_length() {
	mut buf := [128]u8{}
	mut r := response_at(unsafe { &buf[0] })
	r.http_ok()
	r.body('1234567890')
	assert written(mut r) == 'HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n1234567890', 'written: ${written(mut r)}'

	// NOTE: body() appends to the same buffer, so a second call continues the
	// response and writes a fresh Content-Length for the new body.
	r.body('')
	assert written(mut r).ends_with('Content-Length: 0\r\n\r\n'), 'written: ${written(mut r)}'
}

pub fn test_response_content_type_helpers() {
	mut buf := [128]u8{}

	mut r1 := response_at(unsafe { &buf[0] })
	r1.html()
	assert written(mut r1) == 'Content-Type: text/html\r\n', 'written: ${written(mut r1)}'

	mut r2 := response_at(unsafe { &buf[0] })
	r2.plain()
	assert written(mut r2) == 'Content-Type: text/plain\r\n', 'written: ${written(mut r2)}'

	mut r3 := response_at(unsafe { &buf[0] })
	r3.json()
	assert written(mut r3) == 'Content-Type: application/json\r\n', 'written: ${written(mut r3)}'

	mut r4 := response_at(unsafe { &buf[0] })
	r4.content_type('image/png')
	assert written(mut r4) == 'Content-Type: image/png\r\n', 'written: ${written(mut r4)}'
}

pub fn test_response_header_with_empty_parts() {
	mut buf := [128]u8{}
	mut r := response_at(unsafe { &buf[0] })
	r.header('', '')
	r.header('X', '')
	r.header('', 'v')
	assert written(mut r) == ': \r\nX: \r\n: v\r\n', 'written: ${written(mut r)}'
}

pub fn test_response_status_helpers() {
	mut buf := [128]u8{}

	mut r := response_at(unsafe { &buf[0] })
	r.http_404()
	assert written(mut r) == 'HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n', 'written: ${written(mut r)}'
	assert written_len(mut r) == 45, 'len ${written_len(mut r)}'

	mut r2 := response_at(unsafe { &buf[0] })
	r2.http_405()
	assert written(mut r2) == 'HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\n\r\n', 'written: ${written(mut r2)}'

	mut r3 := response_at(unsafe { &buf[0] })
	r3.http_500()
	assert written(mut r3) == 'HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n', 'written: ${written(mut r3)}'
}
