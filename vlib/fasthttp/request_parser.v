module fasthttp

const empty_space = u8(` `)
const tab_char = u8(0x09)
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
	buf := req.buffer
	if buf.len < 12 {
		return error('Too short')
	}
	line_lf := find_byte(&buf[0], buf.len, lf_char)
	if line_lf < 0 {
		return error('Invalid HTTP request line: Missing CR')
	}
	line_end := if line_lf > 0 && buf[line_lf - 1] == cr_char {
		line_lf - 1
	} else {
		line_lf
	}
	method_end := find_byte(&buf[0], line_end, empty_space)
	if method_end <= 0 {
		return error('Invalid method')
	}
	req.method = Slice{0, method_end}

	mut path_start := method_end + 1
	for path_start < line_end && buf[path_start] == empty_space {
		path_start++
	}
	if path_start >= line_end {
		return error('Missing path')
	}
	path_end_rel := find_byte(&buf[path_start], line_end - path_start, empty_space)
	if path_end_rel < 0 {
		req.path = Slice{path_start, line_end - path_start}
		req.version = Slice{line_end, 0}
		return line_lf + 1
	}
	path_end := path_start + path_end_rel
	if path_end == path_start {
		return error('Missing path')
	}
	req.path = Slice{path_start, path_end - path_start}
	mut version_start := path_end + 1
	for version_start < line_end && buf[version_start] == empty_space {
		version_start++
	}
	if version_start >= line_end {
		return error('Missing HTTP version')
	}
	req.version = Slice{version_start, line_end - version_start}
	return line_lf + 1
}

// decode_http_request parses a raw HTTP request from the given byte buffer
pub fn decode_http_request(buffer []u8) !HttpRequest {
	mut req := HttpRequest{
		buffer: buffer
	}

	// header_start is the byte index immediately after the request line's \r\n
	header_start := parse_http1_request_line(mut req)!

	head := scan_request_head(buffer, 0)
	if head.head_len < -1 {
		return error_with_code('malformed request framing', 400)
	}
	if head.head_len >= 0 {
		req.header_fields = Slice{
			start: header_start
			len: head.header_fields_end - header_start
		}
		req.body = Slice{
			start: head.head_len
			len: buffer.len - head.head_len
		}
	} else {
		// Keep decode_http_request useful for callers that only pass a request line
		// or an incomplete header block. Server backends dispatch only complete frames.
		mut header_fields_end := buffer.len
		if header_fields_end >= header_start + 2 && buffer[header_fields_end - 2] == cr_char
			&& buffer[header_fields_end - 1] == lf_char {
			header_fields_end -= 2
		} else if header_fields_end > header_start && buffer[header_fields_end - 1] == lf_char {
			header_fields_end--
		}
		req.header_fields = Slice{header_start, header_fields_end - header_start}
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

// has_complete_body checks if a raw HTTP request buffer contains the full body
// according to the authoritative request framer. Malformed requests are not
// complete; server backends inspect the framing sentinel to reject them.
fn has_complete_body(buf &u8, buf_len int) bool {
	if buf_len <= 0 {
		return false
	}
	view := unsafe { buf.vbytes(buf_len) }
	return frame_request_length_lim_idx(view, 0, 0) >= 0
}

fn find_header_end_in_buf(buf &u8, buf_len int) int {
	if buf_len <= 0 {
		return -1
	}
	view := unsafe { buf.vbytes(buf_len) }
	return frame_head_len(view)
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

struct TransferEncodingState {
mut:
	seen          bool
	final_chunked bool
	chunked_seen  bool
}

struct RequestHead {
	head_len          int = -1
	header_fields_end int
	content_length    int = -1
	chunked           bool
}

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
	head := scan_request_head(buf, max_header)
	if head.head_len < 0 {
		return head.head_len
	}
	if head.chunked {
		return frame_chunked_total_idx(buf, head.head_len, max_header, max_body)
	}
	if head.content_length >= 0 {
		if max_body > 0 && head.content_length > max_body {
			return frame_err_body
		}
		if head.content_length > max_int - head.head_len {
			return frame_err_malformed
		}
		total := head.head_len + head.content_length
		return if buf.len >= total { total } else { -1 }
	}
	return head.head_len
}

// scan_request_head parses request framing fields once and returns canonical
// offsets for the framer, allocation hint, and decoder.
@[direct_array_access]
fn scan_request_head(buf []u8, max_header int) RequestHead {
	if buf.len == 0 {
		return RequestHead{}
	}
	rl := find_byte(&buf[0], buf.len, lf_char)
	if rl < 0 {
		return RequestHead{
			head_len: if max_header > 0 && buf.len >= max_header {
				frame_err_header
			} else {
				-1
			}
		}
	}
	if rl == 0 {
		return RequestHead{
			head_len: frame_err_malformed
		}
	}
	mut pos := rl + 1
	mut header_fields_end := pos
	mut content_length := -1
	mut transfer := TransferEncodingState{}
	for {
		if pos >= buf.len {
			return RequestHead{
				head_len: if max_header > 0 && buf.len >= max_header {
					frame_err_header
				} else {
					-1
				}
			}
		}
		blank := blank_line_end(buf, pos)
		if blank == frame_need_more {
			return RequestHead{
				head_len: if max_header > 0 && buf.len >= max_header {
					frame_err_header
				} else {
					-1
				}
			}
		}
		if blank >= 0 {
			if max_header > 0 && blank > max_header {
				return RequestHead{
					head_len: frame_err_header
				}
			}
			if transfer.seen && (!transfer.final_chunked || content_length >= 0) {
				return RequestHead{
					head_len: frame_err_malformed
				}
			}
			return RequestHead{
				head_len: blank
				header_fields_end: header_fields_end
				content_length: content_length
				chunked: transfer.final_chunked
			}
		}
		line_lf := find_byte(&buf[pos], buf.len - pos, lf_char)
		if line_lf < 0 {
			return RequestHead{
				head_len: if max_header > 0 && buf.len >= max_header {
					frame_err_header
				} else {
					-1
				}
			}
		}
		line_start := pos
		line_len := header_line_content_len(buf, line_start, line_lf)
		pos = line_start + line_lf + 1
		if max_header > 0 && pos >= max_header {
			return RequestHead{
				head_len: frame_err_header
			}
		}
		if !valid_header_line(buf, line_start, line_len) {
			return RequestHead{
				head_len: frame_err_malformed
			}
		}
		header_fields_end = line_start + line_len
		if v := line_header_value(buf, line_start, line_len, 'Content-Length') {
			parsed := parse_content_length(buf, v) or {
				return RequestHead{
					head_len: frame_err_malformed
				}
			}
			if content_length >= 0 && content_length != parsed {
				return RequestHead{
					head_len: frame_err_malformed
				}
			}
			content_length = parsed
		} else if v := line_header_value(buf, line_start, line_len, 'Transfer-Encoding') {
			if !parse_transfer_encoding(buf, v, mut transfer) {
				return RequestHead{
					head_len: frame_err_malformed
				}
			}
		}
	}
	return RequestHead{}
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
	head := scan_request_head(buf, 0)
	if head.head_len < 0 || head.chunked || head.content_length < 0
		|| head.content_length > max_int - head.head_len {
		return -1
	}
	return head.head_len + head.content_length
}

// frame_head_len returns the byte offset where the body begins — the length of
// the request head (request line + header section + the terminating CRLFCRLF) —
// or -1 if the head is not yet complete in `buf`.
@[direct_array_access]
pub fn frame_head_len(buf []u8) int {
	head := scan_request_head(buf, 0)
	return if head.head_len >= 0 { head.head_len } else { -1 }
}

// blank_line_end classifies the bytes at `pos` as a header-section terminator.
// Returns the offset just past the blank line (CRLF or a bare LF), -1 when `pos`
// is not a blank line, or frame_need_more when more bytes are needed to decide.
// Bare LF is accepted for the same lenient behavior as has_complete_body, so the
// framer and the body-completeness check never disagree (a smuggling gap).
@[direct_array_access; inline]
fn blank_line_end(buf []u8, pos int) int {
	if buf[pos] == lf_char {
		return pos + 1
	}
	if buf[pos] == cr_char {
		if pos + 1 >= buf.len {
			return frame_need_more
		}
		if buf[pos + 1] == lf_char {
			return pos + 2
		}
	}
	return -1
}

const frame_need_more = -2

// header_line_content_len returns the length of a header line's content — the
// bytes before its terminator — given the LF offset `line_lf` relative to
// `line_start`. It drops a preceding CR only when one is actually present, so a
// bare-LF line is measured correctly (not one byte short).
@[direct_array_access; inline]
fn header_line_content_len(buf []u8, line_start int, line_lf int) int {
	if line_lf > 0 && buf[line_start + line_lf - 1] == cr_char {
		return line_lf - 1
	}
	return line_lf
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
	mut line_end := line_start + line_len
	mut v := line_start + name.len + 1
	// Trim optional whitespace (space or tab) around the value (RFC 9110 OWS).
	for v < line_end && (buf[v] == empty_space || buf[v] == tab_char) {
		v++
	}
	for line_end > v && (buf[line_end - 1] == empty_space || buf[line_end - 1] == tab_char) {
		line_end--
	}
	return Slice{
		start: v
		len: line_end - v
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
		digit := int(c - `0`)
		if n > (max_int - digit) / 10 {
			return error('Content-Length overflow')
		}
		n = n * 10 + digit
	}
	return n
}

@[inline]
fn is_http_token_char(ch u8) bool {
	return (ch >= `0` && ch <= `9`) || (ch >= `A` && ch <= `Z`)
		|| (ch >= `a` && ch <= `z`) || ch == 33 || ch == 35 || ch == 36 || ch == 37
		|| ch == 38 || ch == 39 || ch == 42 || ch == 43 || ch == 45 || ch == 46
		|| ch == 94 || ch == 95 || ch == 96 || ch == 124 || ch == 126
}

// valid_header_line rejects whitespace before the field name/colon, obsolete
// folding, and control characters that different HTTP parsers can interpret
// differently.
@[direct_array_access]
fn valid_header_line(buf []u8, start int, len int) bool {
	if len == 0 {
		return false
	}
	mut colon := -1
	for i in start .. start + len {
		ch := buf[i]
		if colon < 0 {
			if ch == colon_char {
				if i == start {
					return false
				}
				colon = i
			} else if !is_http_token_char(ch) {
				return false
			}
		} else if ch == 0 || ch == cr_char || ch == lf_char || (ch < 32 && ch != tab_char) {
			return false
		}
	}
	return colon >= 0
}

// parse_transfer_encoding parses exact comma-separated transfer-coding tokens.
// Unknown codings are allowed before chunked, but chunked must occur once and be
// final. The server rejects a transfer-encoded request without final chunked
// framing because a request cannot be delimited by connection close.
@[direct_array_access]
fn parse_transfer_encoding(buf []u8, value Slice, mut state TransferEncodingState) bool {
	mut pos := value.start
	end := value.start + value.len
	for {
		for pos < end && (buf[pos] == empty_space || buf[pos] == tab_char) {
			pos++
		}
		if pos >= end || state.chunked_seen {
			return false
		}
		token_start := pos
		for pos < end && is_http_token_char(buf[pos]) {
			pos++
		}
		if pos == token_start {
			return false
		}
		// The cast is load bearing: the parameter is `&u8`, and handing a C `char[]`
		// straight to it is a pointer sign mismatch that `-cstrict` rejects.
		is_chunked := pos - token_start == 7 && ascii_ci_eq(&buf[token_start], &u8(c'chunked'), 7)
		state.seen = true
		state.final_chunked = is_chunked
		state.chunked_seen = is_chunked
		for pos < end && (buf[pos] == empty_space || buf[pos] == tab_char) {
			pos++
		}
		if pos < end && buf[pos] == `;` {
			// RFC 9112 forbids parameters on the chunked coding. Other codings are
			// not decoded here, but their parameters do not affect message framing.
			if is_chunked {
				return false
			}
			pos++
			mut parameter_bytes := 0
			for pos < end && buf[pos] != `,` {
				ch := buf[pos]
				if ch == 0 || ch == cr_char || ch == lf_char || (ch < 32 && ch != tab_char) {
					return false
				}
				parameter_bytes++
				pos++
			}
			if parameter_bytes == 0 {
				return false
			}
		} else {
			for pos < end && (buf[pos] == empty_space || buf[pos] == tab_char) {
				pos++
			}
		}
		if pos == end {
			return true
		}
		if buf[pos] != `,` || is_chunked {
			return false
		}
		pos++
	}
	return false
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

// frame_chunked_total walks chunk-size lines from body_start and returns the
// total message length once the terminating zero-length chunk + CRLF is present,
// -1 if more bytes are needed, or a framing sentinel on failure.
@[direct_array_access]
fn frame_chunked_total_idx(buf []u8, body_start int, max_header int, max_body int) int {
	mut pos := body_start
	mut body_bytes := 0
	for {
		if pos >= buf.len {
			return -1
		}
		line_lf := find_byte(&buf[pos], buf.len - pos, lf_char)
		if line_lf < 0 {
			return if max_header > 0 && buf.len - pos >= max_header {
				frame_err_header
			} else {
				-1
			}
		}
		if line_lf == 0 || buf[pos + line_lf - 1] != cr_char {
			return frame_err_malformed
		}
		if max_header > 0 && line_lf + 1 > max_header {
			return frame_err_header
		}
		size_end := pos + line_lf - 1 // index of CR
		mut size := 0
		mut j := pos
		mut digits := 0
		for j < size_end {
			c := buf[j]
			if c == `;` {
				break // chunk extensions: ignore the rest of the size line
			}
			d := chunked_hex_digit_value(c)
			if d < 0 {
				return frame_err_malformed
			}
			if size > (max_int - d) / 16 {
				return frame_err_malformed
			}
			size = size * 16 + d
			digits++
			j++
		}
		if digits == 0 {
			return frame_err_malformed
		}
		if j < size_end {
			for k in j + 1 .. size_end {
				ch := buf[k]
				if ch == 0 || (ch < 32 && ch != tab_char) {
					return frame_err_malformed
				}
			}
		}
		if size > max_int - body_bytes {
			return frame_err_malformed
		}
		body_bytes += size
		if max_body > 0 && body_bytes > max_body {
			return frame_err_body
		}
		data_start := pos + line_lf + 1
		if size == 0 {
			// Terminating chunk: validate trailer fields and their CRLF boundaries.
			mut tpos := data_start
			for {
				if tpos >= buf.len {
					return -1
				}
				tlf := find_byte(&buf[tpos], buf.len - tpos, lf_char)
				if tlf < 0 {
					return if max_header > 0 && buf.len - data_start >= max_header {
						frame_err_header
					} else {
						-1
					}
				}
				if tlf == 0 || buf[tpos + tlf - 1] != cr_char {
					return frame_err_malformed
				}
				if max_header > 0 && tpos + tlf + 1 - data_start > max_header {
					return frame_err_header
				}
				trailer_len := tlf - 1
				if trailer_len == 0 {
					return tpos + tlf + 1
				}
				if !valid_header_line(buf, tpos, trailer_len) {
					return frame_err_malformed
				}
				tpos = tpos + tlf + 1
			}
		}
		// Check representability before any offset addition, then compare against
		// remaining bytes by subtraction so a declared size cannot wrap `pos`.
		if data_start > max_int - 2 || size > max_int - data_start - 2 {
			return frame_err_malformed
		}
		remaining := buf.len - data_start
		if size > remaining || remaining - size < 2 {
			return -1
		}
		data_end := data_start + size
		if buf[data_end] != cr_char || buf[data_end + 1] != lf_char {
			return frame_err_malformed
		}
		pos = data_end + 2
	}
	return -1
}
