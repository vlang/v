module picohttpparser

pub struct Request {
mut:
	prev_len int
pub mut:
	method      string
	path        string
	headers     [100]C.phr_header
	num_headers u64
	body        string
}

[inline]
pub fn (mut r Request) parse_request(s string, max_headers int) int {
	method_len := usize(0)
	path_len := usize(0)
	minor_version := 0
	num_headers := usize(max_headers)

	pret := C.phr_parse_request(&char(s.str), s.len, voidptr(&r.method.str), &method_len,
		voidptr(&r.path.str), &path_len, &minor_version, &r.headers[0], &num_headers,
		r.prev_len)
	if pret > 0 {
		unsafe {
			r.method = tos(r.method.str, int(method_len))
			r.path = tos(r.path.str, int(path_len))
		}
		r.num_headers = u64(num_headers)
	}
	r.body = unsafe { (&s.str[pret]).vstring_literal_with_len(s.len - pret) }
	r.prev_len = s.len
	return pret
}

[inline]
pub fn (mut r Request) parse_request_path(s string) int {
	method_len := usize(0)
	path_len := usize(0)

	pret := C.phr_parse_request_path(&char(s.str), s.len, voidptr(&r.method.str), &method_len,
		voidptr(&r.path.str), &path_len)
	if pret > 0 {
		unsafe {
			r.method = tos(r.method.str, int(method_len))
			r.path = tos(r.path.str, int(path_len))
		}
	}
	return pret
}

[inline]
pub fn (mut r Request) parse_request_path_pipeline(s string) int {
	method_len := usize(0)
	path_len := usize(0)

	pret := C.phr_parse_request_path_pipeline(&char(s.str), s.len, voidptr(&r.method.str),
		&method_len, voidptr(&r.path.str), &path_len)
	if pret > 0 {
		unsafe {
			r.method = tos(r.method.str, int(method_len))
			r.path = tos(r.path.str, int(path_len))
		}
	}
	return pret
}
