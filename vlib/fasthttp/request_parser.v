module fasthttp

const empty_space = u8(` `)
const cr_char = u8(`\r`)
const lf_char = u8(`\n`)

// libc memchr is AVX2-accelerated via glibc IFUNC
@[inline]
fn find_byte(buf &u8, len int, c u8) int {
	unsafe {
		p := C.memchr(buf, c, len)
		if p == voidptr(nil) {
			return -1
		}
		return int(&u8(p) - buf)
	}
}

// parse_http1_request_line parses the request line of an HTTP/1.1 request.
// spec: https://datatracker.ietf.org/doc/rfc9112/
// request-line is the start-line for for requests
// According to RFC 9112, the request line is structured as:
// `request-line   = method SP request-target SP HTTP-version`
// where:
// METHOD is the HTTP method (e.g., GET, POST)
// SP is a single space character
// REQUEST-TARGET is the path or resource being requested
// HTTP-VERSION is the version of HTTP being used (e.g., HTTP/1.1)
// CRLF is a carriage return followed by a line feed
// returns the position after the CRLF on success
@[direct_array_access]
pub fn parse_http1_request_line(mut req HttpRequest) !int {
	unsafe {
		buf := &req.buffer[0]
		len := req.buffer.len

		if len < 12 {
			return error('Too short')
		}

		// METHOD
		pos1 := find_byte(buf, len, empty_space)
		if pos1 <= 0 {
			return error('Invalid method')
		}
		req.method = Slice{0, pos1}

		// PATH - skip any extra spaces
		mut pos2 := pos1 + 1
		for pos2 < len && buf[pos2] == empty_space {
			pos2++
		}
		if pos2 >= len {
			return error('Missing path')
		}

		path_start := pos2
		space_pos := find_byte(buf + pos2, len - pos2, empty_space)
		cr_pos := find_byte(buf + pos2, len - pos2, cr_char)

		if space_pos < 0 && cr_pos < 0 {
			return error('Invalid request line')
		}

		// pick earliest delimiter
		mut path_len := 0
		mut delim_pos := 0
		if space_pos >= 0 && (cr_pos < 0 || space_pos < cr_pos) {
			path_len = space_pos
			delim_pos = pos2 + space_pos
		} else {
			path_len = cr_pos
			delim_pos = pos2 + cr_pos
		}

		req.path = Slice{path_start, path_len}

		// VERSION
		if buf[delim_pos] == cr_char {
			// No HTTP version specified
			req.version = Slice{delim_pos, 0}
		} else {
			version_start := delim_pos + 1
			cr := find_byte(buf + version_start, len - version_start, cr_char)
			if cr < 0 {
				return error('Invalid HTTP request line: Missing CR')
			}
			req.version = Slice{version_start, cr}
			delim_pos = version_start + cr
		}

		// Validate CRLF
		if delim_pos + 1 >= len || buf[delim_pos + 1] != lf_char {
			return error('Invalid CRLF')
		}

		return delim_pos + 2 // Return position after CRLF
	}
}

// decode_http_request parses a raw HTTP request from the given byte buffer
pub fn decode_http_request(buffer []u8) !HttpRequest {
	mut req := HttpRequest{
		buffer: buffer
	}

	// header_start is the byte index immediately after the request line's \r\n
	header_start := parse_http1_request_line(mut req)!

	// Find the end of the header block (\r\n\r\n)
	mut body_start := -1
	for i := header_start; i <= buffer.len - 4; i++ {
		if buffer[i] == cr_char && buffer[i + 1] == lf_char && buffer[i + 2] == cr_char
			&& buffer[i + 3] == lf_char {
			body_start = i + 4

			// The header fields slice covers everything from header_start
			// up to (but not including) the final double CRLF
			req.header_fields = Slice{
				start: header_start
				len:   i - header_start
			}
			break
		}
	}

	if body_start != -1 {
		req.body = Slice{
			start: body_start
			len:   buffer.len - body_start
		}
	} else {
		// If no body delimiter found, assume headers go to end or body is missing
		req.header_fields = Slice{header_start, buffer.len - header_start - 2}
		req.body = Slice{0, 0}
	}

	return req
}

// Helper function to convert Slice to string for debugging
fn (slice Slice) to_string(buffer []u8) string {
	if slice.len <= 0 {
		return ''
	}
	return buffer[slice.start..slice.start + slice.len].bytestr()
}
