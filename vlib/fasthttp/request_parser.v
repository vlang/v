module fasthttp

const empty_space = u8(` `)
const cr_char = u8(0x0d)
const lf_char = u8(0x0a)
const colon_char = u8(`:`)

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

// ---- request framing -------------------------------------------------------
//
// The read loop needs to know not just *whether* a full request has arrived
// (`has_complete_body`), but exactly *where* it ends, so a single recv holding
// several pipelined requests can be split into individual messages and answered
// in one batched write. That decision is a PURE function of the bytes received
// so far, kept here so it can be unit-tested by feeding growing prefixes
// (split-point fuzzing) without any sockets. Ported from vanilla's
// `request_parser` framing layer.

// Framing sentinels returned by frame_request_length_lim_idx (the no-Result
// twin). Distinct from -1 (incomplete) and any real length (>= 0); the Result
// wrapper maps each to its HTTP status code.
const frame_err_malformed = -400
const frame_err_body = -413 // body exceeds the configured max_body
const frame_err_header = -431 // header block exceeds the configured max_header

// frame_request_length inspects the bytes received so far and returns:
//   -1          -> incomplete; read more bytes
//   total >= 0  -> a complete message occupying exactly `total` bytes is present
// It errors only on genuinely malformed framing (map to 400). Body length comes
// from Content-Length, or from chunked decoding (Transfer-Encoding), or is zero.
pub fn frame_request_length(buf []u8) !int {
	return frame_request_length_lim(buf, 0, 0)
}

// frame_request_length_lim is frame_request_length with optional size limits
// (0 = unlimited, zero-cost). When a limit is exceeded it returns an error whose
// `.code()` is the HTTP status to send: 431 (header fields too large) or 413
// (payload too large). Other malformed framing carries code 400. Thin Result
// wrapper over the no-Result hot-path twin frame_request_length_lim_idx: cold
// callers (tests, decode) keep this API, while the per-request drain loop calls
// the twin directly to skip the !int boxing.
pub fn frame_request_length_lim(buf []u8, max_header int, max_body int) !int {
	r := frame_request_length_lim_idx(buf, max_header, max_body)
	if r == frame_err_body {
		return error_with_code('body exceeds ${max_body} bytes', 413)
	}
	if r == frame_err_header {
		return error_with_code('header fields exceed ${max_header} bytes', 431)
	}
	if r == frame_err_malformed {
		return error_with_code('malformed request framing', 400)
	}
	return r // >= 0 complete, or -1 incomplete
}

// frame_request_length_lim_idx is the no-Result hot-path twin of
// frame_request_length_lim: it returns a plain int and never constructs a Result,
// so the per-request success path skips the !int boxing. Returns a length >= 0
// (complete — exactly that many bytes), -1 (incomplete — wait for more bytes), or
// a frame_err_* sentinel that the Result wrapper maps to 400 / 413 / 431.
@[direct_array_access]
pub fn frame_request_length_lim_idx(buf []u8, max_header int, max_body int) int {
	if buf.len < 4 {
		return -1
	}
	// End of the request line (first LF). Headers start right after it.
	rl := find_byte(&buf[0], buf.len, lf_char)
	if rl < 0 {
		return -1
	}
	mut pos := rl + 1

	// ONE pass over the header lines: locate the blank-line terminator AND detect
	// Content-Length / Transfer-Encoding as we go (a single walk with a cheap
	// per-line reject beats two separate header scans).
	mut content_length := -1
	mut chunked := false
	for {
		// Cap the head size so a hostile peer can't grow it without bound.
		if max_header > 0 && pos > max_header {
			return frame_err_header
		}
		if pos >= buf.len {
			return -1
		}
		// Blank line => end of header section.
		if buf[pos] == cr_char {
			if pos + 1 >= buf.len {
				return -1
			}
			if buf[pos + 1] == lf_char {
				body_start := pos + 2
				if chunked {
					// Cold path: map the chunked framer's Result to a sentinel.
					return frame_chunked_total(buf, body_start, max_body) or {
						if err.code() == 413 {
							frame_err_body
						} else {
							frame_err_malformed
						}
					}
				}
				if content_length >= 0 {
					total := body_start + content_length
					return if buf.len >= total { total } else { -1 }
				}
				return body_start
			}
		}
		line_lf := find_byte(&buf[pos], buf.len - pos, lf_char)
		if line_lf < 0 {
			return -1
		}
		line_start := pos
		line_len := line_lf - 1 // bytes before the CR
		pos = line_start + line_lf + 1

		// Cheap checks: both reject at byte 0 for the vast majority of headers.
		if v := line_header_value(buf, line_start, line_len, 'Content-Length') {
			content_length = parse_content_length(buf, v) or { return frame_err_malformed }
			// Reject an over-large body from the declared length, BEFORE buffering it.
			if max_body > 0 && content_length > max_body {
				return frame_err_body
			}
		} else if v := line_header_value(buf, line_start, line_len, 'Transfer-Encoding') {
			if ci_contains(buf, v, 'chunked') {
				chunked = true
			}
		}
	}
	return -1
}

// frame_expected_total returns the full HTTP/1.1 message length (headers + body)
// as soon as it is determinable from the bytes buffered so far: the header
// section must be complete AND the body length known via Content-Length. Returns
// -1 when not yet determinable — headers incomplete, a chunked body (length
// unknown until the terminator), or no Content-Length at all.
//
// This is a pure sizing HINT for the read loop: it lets a large upload grow its
// recv buffer to the exact message size in ONE allocation instead of doubling
// toward it. The authoritative framing and limit checks stay in
// frame_request_length_lim, which the read loop still runs once the bytes arrive.
@[direct_array_access]
pub fn frame_expected_total(buf []u8) int {
	if buf.len < 4 {
		return -1
	}
	rl := find_byte(&buf[0], buf.len, lf_char)
	if rl < 0 {
		return -1
	}
	mut pos := rl + 1
	mut content_length := -1
	for {
		if pos >= buf.len {
			return -1
		}
		if buf[pos] == cr_char {
			if pos + 1 >= buf.len {
				return -1
			}
			if buf[pos + 1] == lf_char {
				body_start := pos + 2
				if content_length >= 0 {
					return body_start + content_length
				}
				return -1 // chunked or bodyless — nothing to pre-size against
			}
		}
		line_lf := find_byte(&buf[pos], buf.len - pos, lf_char)
		if line_lf < 0 {
			return -1
		}
		line_start := pos
		line_len := line_lf - 1 // bytes before the CR
		pos = line_start + line_lf + 1
		if v := line_header_value(buf, line_start, line_len, 'Content-Length') {
			content_length = parse_content_length(buf, v) or { return -1 }
		}
	}
	return -1
}

// frame_head_len returns the byte offset where the body begins — the length of
// the request head (request line + header section + the terminating CRLFCRLF) —
// or -1 if the head is not yet complete in `buf`.
@[direct_array_access]
pub fn frame_head_len(buf []u8) int {
	if buf.len < 4 {
		return -1
	}
	rl := find_byte(&buf[0], buf.len, lf_char)
	if rl < 0 {
		return -1
	}
	mut pos := rl + 1
	for {
		if pos >= buf.len {
			return -1
		}
		if buf[pos] == cr_char {
			if pos + 1 >= buf.len {
				return -1
			}
			if buf[pos + 1] == lf_char {
				return pos + 2 // past the blank line's CRLF => body start
			}
		}
		line_lf := find_byte(&buf[pos], buf.len - pos, lf_char)
		if line_lf < 0 {
			return -1
		}
		pos = pos + line_lf + 1
	}
	return -1
}

// line_header_value returns the value Slice if a header line (line_len bytes
// before CRLF, starting at line_start) has the case-insensitive name `name`
// immediately followed by ':'. Used by the single-pass framer.
@[direct_array_access; inline]
fn line_header_value(buf []u8, line_start int, line_len int, name string) ?Slice {
	if name.len + 1 > line_len {
		return none
	}
	if !ascii_ci_eq(&buf[line_start], name.str, name.len)
		|| buf[line_start + name.len] != colon_char {
		return none
	}
	line_end := line_start + line_len
	mut v := line_start + name.len + 1
	for v < line_end && buf[v] == empty_space {
		v++
	}
	return Slice{
		start: v
		len:   line_end - v
	}
}

// parse_content_length parses the decimal digits of a Content-Length value Slice.
fn parse_content_length(buf []u8, s Slice) !int {
	if s.len == 0 {
		return error('empty Content-Length')
	}
	mut n := 0
	for i in s.start .. s.start + s.len {
		c := buf[i]
		if c < `0` || c > `9` {
			return error('non-digit in Content-Length')
		}
		n = n * 10 + int(c - `0`)
	}
	return n
}

// ascii_ci_eq compares `len` bytes case-insensitively (ASCII only — HTTP header
// names are ASCII per RFC 9110 §5.1). No allocation, no lowercase copy: fold each
// byte inline. Kept tight because it runs on the header hot path.
@[direct_array_access; inline]
fn ascii_ci_eq(a &u8, b &u8, len int) bool {
	unsafe {
		for i in 0 .. len {
			x := a[i] ^ b[i]
			if x != 0 {
				// The ONLY acceptable difference is the ASCII case bit (0x20) on a
				// letter — everything else is a mismatch.
				if x != 0x20 {
					return false
				}
				c := a[i] | 0x20
				if c < `a` || c > `z` {
					return false
				}
			}
		}
	}
	return true
}

// ci_contains reports whether the value slice contains `needle` (ASCII, CI).
fn ci_contains(buf []u8, val Slice, needle string) bool {
	if needle.len > val.len {
		return false
	}
	last := val.start + val.len - needle.len
	for i := val.start; i <= last; i++ {
		if ascii_ci_eq(&buf[i], needle.str, needle.len) {
			return true
		}
	}
	return false
}

// frame_chunked_total walks chunk-size lines from body_start and returns the
// total message length once the terminating zero-length chunk + CRLF is present,
// -1 if more bytes are needed, or an error on malformed chunk framing.
@[direct_array_access]
fn frame_chunked_total(buf []u8, body_start int, max_body int) !int {
	// Bound the buffered chunked payload (the total length isn't known up front).
	if max_body > 0 && buf.len - body_start > max_body {
		return error_with_code('body exceeds ${max_body} bytes', 413)
	}
	mut pos := body_start
	for {
		if pos >= buf.len {
			return -1
		}
		line_lf := find_byte(&buf[pos], buf.len - pos, lf_char)
		if line_lf < 0 {
			return -1
		}
		size_end := pos + line_lf // index of LF
		mut size := 0
		mut j := pos
		for j < size_end && buf[j] != cr_char {
			c := buf[j]
			if c == `;` {
				break // chunk extensions: ignore the rest of the size line
			}
			d := chunked_hex_digit_value(c)
			if d < 0 {
				return error_with_code('invalid chunk size', 400)
			}
			size = size * 16 + d
			j++
		}
		data_start := size_end + 1
		if size == 0 {
			// Terminating chunk; require the closing CRLF (trailers not modeled).
			if data_start + 1 >= buf.len {
				return -1
			}
			if buf[data_start] == cr_char && buf[data_start + 1] == lf_char {
				return data_start + 2
			}
			return -1
		}
		next := data_start + size + 2 // data + trailing CRLF
		if next > buf.len {
			return -1
		}
		pos = next
	}
	return -1
}
