module picohttpparser

// NOTE: picohttpparser is designed for speed. Please do some benchmarks when
// you change something in this file

// token_char_map contains all allowed characters in HTTP headers
const token_char_map = '\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0' +
	'\0\1\0\1\1\1\1\1\0\0\1\1\0\1\1\0\1\1\1\1\1\1\1\1\1\1\0\0\0\0\0\0' +
	'\0\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\0\0\0\1\1' +
	'\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\0\1\0\1\0' +
	'\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0' +
	'\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0' +
	'\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0' +
	'\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0'

fn (mut r Request) phr_parse_request_path(buf_start &u8, buf_end &u8, mut pret Pret) {
	mut buf := unsafe { buf_start + 0 }

	// ADVANCE_TOKEN
	method := advance_token(buf, buf_end, mut pret)
	if pret.ret < 0 {
		return
	}
	unsafe {
		buf += pret.ret
	}
	$if trace_parse ? {
		eprintln('method: ${method}')
	}
	// skip spaces
	for {
		unsafe { buf++ }
		if *buf != ` ` {
			break
		}
	}

	path := advance_token(buf, buf_end, mut pret)
	if pret.ret < 0 {
		return
	}
	$if trace_parse ? {
		eprintln('path: ${path}')
	}
	unsafe {
		buf += pret.ret
	}
	// skip spaces
	for {
		unsafe { buf++ }
		if *buf != ` ` {
			break
		}
	}
	// validate
	if method == '' || path == '' {
		pret.ret = -1
		pret.err = 'error parsing request: invalid method or path'
		return
	}
	r.method = method
	r.path = path

	pret.ret = unsafe { buf - buf_start }
}

fn (mut r Request) phr_parse_request_path_pipeline(buf_start &u8, buf_end &u8, mut pret Pret) {
	mut buf := unsafe { buf_start }
	method := advance_token2(buf, buf_end, mut pret)
	if pret.ret < 0 {
		return
	}
	unsafe {
		buf += pret.ret
	}
	path := advance_token2(buf, buf_end, mut pret)
	if pret.ret < 0 {
		return
	}
	unsafe {
		buf += pret.ret
	}
	// validate
	if method == '' || path == '' {
		pret.ret = -1
		pret.err = 'error parsing request: invalid method or path'
		return
	}
	r.method = method
	r.path = path

	for buf < buf_end {
		unsafe { buf++ }
		// check if following 4 characters are '\r\n\r\n' indicating a new request line
		if unsafe { *(&u32(buf)) == 0x0a0d0a0d } {
			unsafe {
				buf += 4
			}
			pret.ret = unsafe { buf - buf_start }
			return
		}
	}

	pret.ret = -1
	pret.err = 'error parsing request: no request found'
}

fn (mut r Request) phr_parse_request(buf_start &u8, buf_end &u8, mut pret Pret) &u8 {
	// make copy of `buf_start` that can be mutated
	mut buf := unsafe { buf_start }

	// skip first empty line (some clients add CRLF after POST content)
	// CHECK_EOF
	if buf == buf_end {
		pret.ret = -2
		return unsafe { nil }
	}
	if *buf == `\r` {
		unsafe { buf++ }
		// EXPECT_CHAR
		if buf == buf_end {
			pret.ret = -2
			return unsafe { nil }
		}
		if *buf != `\n` {
			pret.ret = -1
			pret.err = 'error parsing request: expected "\n" after "\r"'
			return unsafe { nil }
		}
	}

	// parse request line
	r.phr_parse_request_path(buf, buf_end, mut pret)
	if pret.ret < 0 {
		return unsafe { nil }
	}
	unsafe {
		buf += pret.ret
	}
	minor_version := parse_http_version(buf, buf_end, mut pret)
	if pret.ret < 0 {
		return unsafe { nil }
	}
	$if trace_parse ? {
		eprintln('minor_version: ${minor_version}')
	}
	unsafe {
		buf += pret.ret
	}
	// CHECK_EOF
	if buf == buf_end {
		pret.ret = -2
		return unsafe { nil }
	}
	if *buf == `\r` {
		unsafe { buf++ }
		// EXPECT_CHAR
		if buf == buf_end {
			pret.ret = -2
			return unsafe { nil }
		}
		if *buf != `\n` {
			pret.ret = -1
			pret.err = 'error parsing request: expected "\n" after "\r"'
			return unsafe { nil }
		}
		unsafe { buf++ }
	} else if *buf == `\n` {
		unsafe { buf++ }
	} else {
		pret.ret = -1
		pret.err = 'error parsing request: expecting "\r\n" after HTTP version'
		return unsafe { nil }
	}

	return r.parse_headers(buf, buf_end, mut pret)
}

@[direct_array_access]
fn (mut r Request) parse_headers(buf_start &u8, buf_end &u8, mut pret Pret) &u8 {
	mut buf := unsafe { buf_start }

	mut i := 0

	for i = r.num_headers; i < max_headers; i++ {
		// CHECK_EOF
		if buf == buf_end {
			pret.ret = -2
			return unsafe { nil }
		}
		if *buf == `\r` {
			unsafe { buf++ }
			// EXPECT_CHAR
			if buf == buf_end {
				pret.ret = -2
				return unsafe { nil }
			}
			if *buf != `\n` {
				pret.ret = -1
				pret.err = 'error parsing request: expected "\n" after "\r"'
				return unsafe { nil }
			}
			unsafe { buf++ }

			break
		} else if *buf == `\n` {
			unsafe { buf++ }
			break
		}

		if !(*buf == ` ` || *buf == `\t`) {
			name_start := buf
			// parsing name, but do not discard SP before colon, see
			// http://www.mozilla.org/security/announce/2006/mfsa2006-33.html
			for *buf != `:` {
				// check if the current character is allowed in an HTTP header
				if token_char_map[*buf] == 0 {
					$if trace_parse ? {
						eprintln('invalid character! ${*buf}')
					}
					pret.ret = -1
					pret.err = 'error parsing request: invalid character in header "${*buf}"'
					return unsafe { nil }
				}
				unsafe { buf++ }

				// CHECK_EOF
				if buf == buf_end {
					pret.ret = -2
					return unsafe { nil }
				}
			}

			name_len := unsafe { buf - name_start }
			if name_len == 0 {
				pret.ret = -1
				pret.err = 'error parsing request: invalid header name'
				return unsafe { nil }
			}
			r.headers[i].name = unsafe { tos(name_start, name_len) }

			unsafe { buf++ }
			for { // CHECK_EOF
				if buf == buf_end {
					pret.ret = -2
					return unsafe { nil }
				}
				if !(*buf == ` ` || *buf == `\t`) {
					break
				}
				unsafe { buf++ }
			}
		} else {
			r.headers[i].name = ''
		}

		mut value_len := get_token_length_to_eol(buf, buf_end, mut pret)
		if pret.ret < 0 {
			return unsafe { nil }
		}

		// TODO: strip characters
		value_end := unsafe { buf + value_len }
		for value_end != buf {
			c := unsafe { *(value_end - 1) }
			if !(c == ` ` || c == `\t`) {
				break
			}
			unsafe { value_end-- }
		}

		r.headers[i].value = unsafe { tos(buf, value_end - buf) }
		r.num_headers++

		unsafe {
			buf += pret.ret
		}
	}

	if i == max_headers {
		// too many headers
		eprintln('Too many headers!')
		pret.ret = -1
		pret.err = 'error parsing request: too many headers!'
		return unsafe { nil }
	}

	pret.ret = unsafe { buf - buf_start }
	return buf
}

// is_complete checks if an http request is done
fn is_complete(buf_start &u8, buf_end &u8, last_len int, mut pret Pret) &u8 {
	mut ret_cnt := 0
	// get the last 3 characters of the request buffer
	buf := if last_len < 3 { buf_start } else { unsafe { buf_start + last_len - 3 } }

	for {
		// CHECK_EOF
		if buf == buf_end {
			pret.ret = -2
			return unsafe { nil }
		}
		// We expect a line of an http request to end with '\r\n'
		if *buf == `\r` {
			unsafe { buf++ }
			// CHECK_EOF
			if buf == buf_end {
				pret.ret = -2
				return unsafe { nil }
			}
			// EXPECT_CHAR_NO_CHECK
			if *buf != `\n` {
				// no '\n' after '\r' indicates a parse error
				pret.ret = -1
				pret.err = 'error parsing request: expected "\n" after "\r"'
				return unsafe { nil }
			}
			unsafe { buf++ }

			ret_cnt++
		} else if *buf == `\n` {
			unsafe { buf++ }
			ret_cnt++
		} else {
			// other character
			unsafe { buf++ }
			ret_cnt = 0
		}
		if ret_cnt == 2 {
			return buf
		}
	}

	pret.ret = -2
	return unsafe { nil }
}

fn parse_http_version(buf_start &u8, buf_end &u8, mut pret Pret) int {
	// we want at least [HTTP/1.<two chars>] to try to parse
	if unsafe { buf_end - buf_start } < 9 {
		pret.ret = -2
		return 0
	}
	if unsafe { tos(buf_start, 7) != 'HTTP/1.' } {
		pret.ret = -1
		pret.err = 'error parsing request: picohttpparser only supports HTTP/1.x'
		return 0
	}

	// PARSE_INT
	c := unsafe { *(buf_start + 7) }
	if c < `0` || c > `9` {
		pret.ret = -1
		pret.err = 'error parsing request: invalid HTTP version'
		return 0
	}
	pret.ret = 8
	return int(c - `0`)
}

fn get_token_length_to_eol(buf_start &u8, buf_end &u8, mut pret Pret) int {
	mut buf := unsafe { buf_start }
	mut token_len := 0

	// find non-printable char within the next 8 bytes
	// HOT code: (TODO: should be manually inlined)
	for _likely_(unsafe { buf_end - buf >= 8 }) {
		for _ in 0 .. 8 {
			if _unlikely_(!is_printable_ascii(*buf)) {
				// non printable
				unsafe {
					goto non_printable
				}
			}
			unsafe { buf++ }
			continue

			non_printable:
			// allow space and horizontal tab
			if _likely_(*buf < ` ` && *buf != 9) || _unlikely_(*buf == 127) {
				// found clear the line (CTL)
				unsafe {
					goto found_ctl
				}
			}
			unsafe { buf++ }
		}
	}
	// remaining characters
	for {
		// CHECK_EOF
		if buf == buf_end {
			pret.ret = -2
			return 0
		}
		if _likely_(*buf < ` ` && *buf != 9) || _unlikely_(*buf == 127) {
			// found clear the line (CTL)
			unsafe {
				goto found_ctl
			}
		}
		unsafe { buf++ }
	}

	found_ctl:
	if _likely_(*buf == `\r`) {
		unsafe { buf++ }
		// EXPECT_CHAR
		if buf == buf_end {
			pret.ret = -2
			return 0
		}
		if *buf != `\n` {
			// no '\n' after '\r' indicates a parse error
			pret.ret = -1
			pret.err = 'error parsing request: expected "\n" after "\r"'
			return 0
		}
		unsafe { buf++ }
		token_len = unsafe { buf - 2 - buf_start }
	} else if *buf == `\n` {
		token_len = unsafe { buf - buf_start }
		unsafe { buf++ }
	} else {
		pret.ret = -1
		pret.err = 'error parsing request: expecting "\r\n" after header'
		return 0
	}

	if token_len == 0 {
		pret.ret = 0
		return 0
	}

	pret.ret = unsafe { buf - buf_start }
	return token_len
}

// following functions are #define in the C version, but inline here for better readability

@[inline]
fn advance_token(tok_start &u8, tok_end &u8, mut pret Pret) string {
	mut buf := unsafe { tok_start }
	for *buf != ` ` {
		if _unlikely_(!is_printable_ascii(*buf)) {
			if *buf < ` ` || *buf == 127 {
				pret.ret = -1
				pret.err = 'error parsing request: invalid character "${*buf}"'
				return ''
			}
		}
		unsafe { buf++ }
		// CHECK_EOF
		if buf == tok_end {
			pret.ret = -2
			return ''
		}
	}

	pret.ret = unsafe { buf - tok_start }
	return unsafe { tos(tok_start, pret.ret) }
}

// advance_token2 is a less safe version of advance_token
@[inline]
fn advance_token2(tok_start &u8, tok_end &u8, mut pret Pret) string {
	mut len := 0
	mut i := 0
	for {
		if unsafe { *(tok_start + i) == ` ` } {
			len = i
			for unsafe { *(tok_start + i) == ` ` } {
				i++
			}
			break
		}
		i++
	}
	pret.ret = i
	return unsafe { tos(tok_start, len) }
}

@[inline]
fn is_printable_ascii(c u8) bool {
	return u32(c - 32) < 95
}
