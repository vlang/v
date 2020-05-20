module picohttpparser

pub struct Request {
pub mut:
	method string
	path string
	headers &C.phr_header_t
	num_headers u64
}




[inline]
pub fn (mut r Request) parse_request(s string, headers &C.phr_header_t, max_headers int) int {
	method_len := u64(0)
	path_len := u64(0)
	minor_version := 0
	num_headers := u64(max_headers)

	pret := C.phr_parse_request(
		s.str, s.len,
		&r.method, &method_len,
		&r.path, &path_len,
		&minor_version,
		headers, &num_headers,
		0
	)
	if pret > 0 {
		r.method = tos(r.method.str, int(method_len))
		r.path = tos(r.path.str, int(path_len))
		r.headers = headers
		r.num_headers = num_headers
	}
	return pret
}

[inline]
pub fn (mut r Request) parse_request_path(s string) int {
	method_len := u64(0)
	path_len := u64(0)

	pret := C.phr_parse_request_path(
		s.str, s.len,
		&r.method, &method_len,
		&r.path, &path_len
	)
	if pret > 0 {
		r.method = tos(r.method.str, int(method_len))
		r.path = tos(r.path.str, int(path_len))
	}
	return pret
}

[inline]
pub fn (mut r Request) parse_request_path_pipeline(s string) int {
	method_len := u64(0)
	path_len := u64(0)

	pret := C.phr_parse_request_path_pipeline(
		s.str, s.len,
		&r.method, &method_len,
		&r.path, &path_len
	)
	if pret > 0 {
		r.method = tos(r.method.str, int(method_len))
		r.path = tos(r.path.str, int(path_len))
	}
	return pret
}
