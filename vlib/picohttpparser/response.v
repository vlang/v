module picohttpparser

pub struct Response {
	fd int
pub:
	date byteptr
	buf_start byteptr
pub mut:
	buf byteptr
}

[inline]
pub fn (mut r Response) http_ok() &Response {
	r.buf += cpy_str(r.buf, "HTTP/1.1 200 OK\r\n")
	return r
}

[inline]
pub fn (mut r Response) header(k, v string) &Response {
	r.buf += cpy_str(r.buf, k)
	r.buf += cpy_str(r.buf, ": ")
	r.buf += cpy_str(r.buf, v)
	r.buf += cpy_str(r.buf, "\r\n")
	return r
}

[inline]
pub fn (mut r Response) header_date() &Response {
	r.buf += cpy_str(r.buf, "Date: ")
	r.buf += cpy(r.buf, r.date, 29)
	r.buf += cpy_str(r.buf, "\r\n")
	return r
}

[inline]
pub fn (mut r Response) header_server() &Response {
	r.buf += cpy_str(r.buf, "Server: V\r\n")
	return r
}

[inline]
pub fn (mut r Response) content_type(s string) &Response {
	r.buf += cpy_str(r.buf, "Content-Type: ")
	r.buf += cpy_str(r.buf, s)
	r.buf += cpy_str(r.buf, "\r\n")
	return r
}

[inline]
pub fn (mut r Response) plain() &Response {
	r.buf += cpy_str(r.buf, "Content-Type: text/plain\r\n")
	return r
}

[inline]
pub fn (mut r Response) json() &Response {
	r.buf += cpy_str(r.buf, "Content-Type: application/json\r\n")
	return r
}

[inline]
pub fn (mut r Response) body(body string) {
	r.buf += cpy_str(r.buf, "Content-Length: ")
	r.buf += C.u64toa(r.buf, body.len)
	r.buf += cpy_str(r.buf, "\r\n\r\n")
	r.buf += cpy_str(r.buf, body)
}

[inline]
pub fn (mut r Response) http_404() {
	r.buf += cpy_str(r.buf, 'HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n')
}

[inline]
pub fn (mut r Response) http_405() {
	r.buf += cpy_str(r.buf, 'HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\n\r\n')
}

[inline]
pub fn (mut r Response) http_500() {
	r.buf += cpy_str(r.buf, 'HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n')
}

[inline]
pub fn (mut r Response) raw(response string) {
	r.buf += cpy_str(r.buf, response)
}

[inline]
pub fn (mut r Response) end() int {
	n := int(r.buf - r.buf_start)
	if C.write(r.fd, r.buf_start, n) != n {
		return -1
	}
	return n
}
