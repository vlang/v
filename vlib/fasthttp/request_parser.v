module fasthttp

const empty_space = u8(` `)
const cr_char = u8(0x0d)
const lf_char = u8(0x0a)

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

@[direct_array_access]
fn find_header_end_in_buf(buf &u8, buf_len int) int {
	for i := 0; i < buf_len - 1; i++ {
		unsafe {
			if buf[i] == `\n` {
				if i + 1 < buf_len && buf[i + 1] == `\n` {
					return i + 2
				}
				if i + 2 < buf_len && buf[i + 1] == `\r` && buf[i + 2] == `\n` {
					return i + 3
				}
			}
			if i + 3 < buf_len && buf[i] == `\r` && buf[i + 1] == `\n` && buf[i + 2] == `\r`
				&& buf[i + 3] == `\n` {
				return i + 4
			}
		}
	}
	return -1
}

// has_complete_body checks if a raw HTTP request buffer contains the full body
// as indicated by the Content-Length or Transfer-Encoding headers. Returns true if:
//   - there is no Content-Length header and no chunked encoding (body not expected)
//   - Content-Length is 0
//   - enough body bytes have been received
//   - chunked encoding is complete (the zero-size chunk and trailers were parsed)
// Returns false only when more body data is expected.
@[direct_array_access]
fn has_complete_body(buf &u8, buf_len int) bool {
	header_end := find_header_end_in_buf(buf, buf_len)
	if header_end < 0 {
		return false // headers not complete yet
	}
	// Check for Transfer-Encoding: chunked header (case-insensitive)
	if has_chunked_transfer_encoding_in_buf(buf, header_end) {
		return has_complete_chunked_body(buf, buf_len, header_end)
	}
	content_length := parse_content_length_from_buf(buf, header_end)
	if content_length <= 0 {
		return true // no content-length or zero: body complete
	}
	body_received := buf_len - header_end
	return body_received >= content_length
}

@[direct_array_access]
fn has_complete_chunked_body(buf &u8, buf_len int, body_start int) bool {
	mut pos := body_start
	for {
		lf_pos := find_line_lf_in_buf(buf, buf_len, pos)
		if lf_pos < 0 {
			return false
		}
		mut line_end := lf_pos
		unsafe {
			if line_end > pos && buf[line_end - 1] == `\r` {
				line_end--
			}
		}
		mut size_end := line_end
		for i := pos; i < line_end; i++ {
			unsafe {
				if buf[i] == `;` {
					size_end = i
					break
				}
			}
		}
		mut size_start := pos
		for size_start < size_end {
			unsafe {
				if buf[size_start] != ` ` && buf[size_start] != `\t` {
					break
				}
			}
			size_start++
		}
		for size_end > size_start {
			unsafe {
				if buf[size_end - 1] != ` ` && buf[size_end - 1] != `\t` {
					break
				}
			}
			size_end--
		}
		if size_start == size_end {
			return true
		}
		mut chunk_size := 0
		for i := size_start; i < size_end; i++ {
			digit := chunked_hex_digit_value(unsafe { buf[i] })
			if digit < 0 {
				return true
			}
			if chunk_size > (max_int - digit) / 16 {
				return true
			}
			chunk_size = chunk_size * 16 + digit
		}
		pos = lf_pos + 1
		if chunk_size == 0 {
			return has_complete_chunked_trailers(buf, buf_len, pos)
		}
		if chunk_size > buf_len - pos {
			return false
		}
		data_end := pos + chunk_size
		if data_end + 2 > buf_len {
			return false
		}
		unsafe {
			if buf[data_end] != `\r` || buf[data_end + 1] != `\n` {
				return true
			}
		}
		pos = data_end + 2
	}
	return false
}

@[direct_array_access]
fn has_complete_chunked_trailers(buf &u8, buf_len int, start int) bool {
	mut pos := start
	for {
		lf_pos := find_line_lf_in_buf(buf, buf_len, pos)
		if lf_pos < 0 {
			return false
		}
		mut line_end := lf_pos
		unsafe {
			if line_end > pos && buf[line_end - 1] == `\r` {
				line_end--
			}
		}
		if line_end == pos {
			return true
		}
		pos = lf_pos + 1
	}
	return false
}

@[direct_array_access]
fn find_line_lf_in_buf(buf &u8, buf_len int, start int) int {
	for i := start; i < buf_len; i++ {
		unsafe {
			if buf[i] == `\n` {
				return i
			}
		}
	}
	return -1
}

fn chunked_hex_digit_value(ch u8) int {
	if ch >= `0` && ch <= `9` {
		return int(ch - `0`)
	}
	if ch >= `a` && ch <= `f` {
		return int(ch - `a` + 10)
	}
	if ch >= `A` && ch <= `F` {
		return int(ch - `A` + 10)
	}
	return -1
}

// has_chunked_transfer_encoding_in_buf scans the header bytes for a
// "Transfer-Encoding:" header whose value contains "chunked" (case-insensitive).
@[direct_array_access]
fn has_chunked_transfer_encoding_in_buf(buf &u8, header_end int) bool {
	te_lower := 'transfer-encoding:'
	for i := 0; i < header_end - te_lower.len; i++ {
		unsafe {
			if buf[i] != `\n` {
				continue
			}
			pos := i + 1
			if pos + te_lower.len > header_end {
				continue
			}
			mut matched := true
			for j := 0; j < te_lower.len; j++ {
				mut ch := buf[pos + j]
				if ch >= `A` && ch <= `Z` {
					ch = ch + 32
				}
				if ch != te_lower[j] {
					matched = false
					break
				}
			}
			if matched {
				chunked_str := 'chunked'
				for val_start := pos + te_lower.len; val_start < header_end - chunked_str.len; val_start++ {
					if buf[val_start] == `\r` || buf[val_start] == `\n` {
						break
					}
					mut cmatch := true
					for k := 0; k < chunked_str.len; k++ {
						mut ch2 := buf[val_start + k]
						if ch2 >= `A` && ch2 <= `Z` {
							ch2 = ch2 + 32
						}
						if ch2 != chunked_str[k] {
							cmatch = false
							break
						}
					}
					if cmatch {
						return true
					}
				}
			}
		}
	}
	return false
}

// parse_content_length_from_buf scans the header bytes for a Content-Length header
// and returns its integer value, or -1 if not found.
@[direct_array_access]
fn parse_content_length_from_buf(buf &u8, header_end int) int {
	cl_lower := 'content-length:'
	for i := 0; i < header_end - cl_lower.len; i++ {
		unsafe {
			if buf[i] != `\n` {
				continue
			}
			pos := i + 1
			if pos + cl_lower.len > header_end {
				continue
			}
			mut matched := true
			for j := 0; j < cl_lower.len; j++ {
				mut ch := buf[pos + j]
				if ch >= `A` && ch <= `Z` {
					ch = ch + 32
				}
				if ch != cl_lower[j] {
					matched = false
					break
				}
			}
			if matched {
				mut start := pos + cl_lower.len
				for start < header_end && buf[start] == ` ` {
					start++
				}
				mut val := 0
				for start < header_end && buf[start] >= `0` && buf[start] <= `9` {
					val = val * 10 + int(buf[start] - `0`)
					start++
				}
				return val
			}
		}
	}
	return -1
}
