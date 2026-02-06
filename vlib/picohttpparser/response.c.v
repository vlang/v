module picohttpparser

$if !windows {
	#include <sys/socket.h>
}

pub struct Response {
pub:
	fd        int
	date      &u8 = unsafe { nil }
	buf_start &u8 = unsafe { nil }
pub mut:
	buf &u8 = unsafe { nil }
}

@[inline]
pub fn (mut r Response) write_string(s string) {
	unsafe {
		vmemcpy(r.buf, s.str, s.len)
		r.buf += s.len
	}
}

@[inline]
pub fn (mut r Response) http_ok() &Response {
	r.write_string('HTTP/1.1 200 OK\r\n')
	return unsafe { r }
}

@[inline]
pub fn (mut r Response) header(k string, v string) &Response {
	r.write_string(k)
	r.write_string(': ')
	r.write_string(v)
	r.write_string('\r\n')
	return unsafe { r }
}

@[inline]
pub fn (mut r Response) header_date() &Response {
	r.write_string('Date: ')
	unsafe {
		vmemcpy(r.buf, r.date, 29)
		r.buf += 29
	}
	r.write_string('\r\n')
	return unsafe { r }
}

@[inline]
pub fn (mut r Response) header_server() &Response {
	r.write_string('Server: V\r\n')
	return unsafe { r }
}

@[inline]
pub fn (mut r Response) content_type(s string) &Response {
	r.write_string('Content-Type: ')
	r.write_string(s)
	r.write_string('\r\n')
	return unsafe { r }
}

@[inline]
pub fn (mut r Response) html() &Response {
	r.write_string('Content-Type: text/html\r\n')
	return unsafe { r }
}

@[inline]
pub fn (mut r Response) plain() &Response {
	r.write_string('Content-Type: text/plain\r\n')
	return unsafe { r }
}

@[inline]
pub fn (mut r Response) json() &Response {
	r.write_string('Content-Type: application/json\r\n')
	return unsafe { r }
}

@[inline]
pub fn (mut r Response) body(body string) {
	r.write_string('Content-Length: ')
	unsafe {
		r.buf += u64toa(r.buf, u64(body.len)) or { panic(err) }
	}
	r.write_string('\r\n\r\n')
	r.write_string(body)
}

@[inline]
pub fn (mut r Response) http_404() {
	r.write_string('HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n')
}

@[inline]
pub fn (mut r Response) http_405() {
	r.write_string('HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\n\r\n')
}

@[inline]
pub fn (mut r Response) http_500() {
	r.write_string('HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n')
}

@[inline]
pub fn (mut r Response) raw(response string) {
	r.write_string(response)
}

fn C.send(sockfd int, buf voidptr, len usize, flags int) int

@[inline]
pub fn (mut r Response) end() int {
	n := int(i64(r.buf) - i64(r.buf_start))
	// use send instead of write for windows compatibility
	if C.send(r.fd, r.buf_start, n, 0) != n {
		return -1
	}
	return n
}
