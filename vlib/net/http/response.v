// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.http.chunked
import strings

// Response represents the result of the request
pub struct Response {
pub mut:
	text        string
	header      Header
	cookies     map[string]string
	status_code int
	version     Version
}

fn (mut resp Response) free() {
	unsafe { resp.header.data.free() }
}

pub struct ResponseRenderConfig {
	chunked    bool
	chunk_size int = 20
}

// Formats resp to bytes suitable for HTTP response transmission
pub fn (resp Response) bytes(conf ...ResponseRenderConfig) []byte {
	// TODO: build []byte directly; this uses two allocations
	return resp.bytestr(...conf).bytes()
}

// Formats resp to a string suitable for HTTP response transmission
pub fn (resp Response) bytestr(conf ...ResponseRenderConfig) string {
	do_chunk := conf.any(it.chunked) && conf[0].chunk_size > 0
	// TODO: cookies
	headers := if do_chunk && !resp.header.values(.transfer_encoding).contains('chunked') {
		mut h := resp.header
		h.add(.transfer_encoding, 'chunked')
		h.render(version: resp.version)
	} else {
		resp.header.render(version: resp.version)
	}
	status_msg := status_from_int(resp.status_code)
	body := if do_chunk { chunk_encode(resp.text, conf[0].chunk_size) } else { resp.text }

	return '$resp.version $resp.status_code $status_msg\r\n$headers\r\n$body'
}

// TODO: move to http.chunked?
// chunk_encode encodes an HTTP body for chunked transfer
[manualfree]
fn chunk_encode(s string, max_len int) string {
	mut sb := strings.new_builder(s.len + 64)
	defer {
		unsafe { sb.free() }
	}
	mut i := 0
	for ; i < s.len - max_len; i += max_len {
		sb.write_string(max_len.hex())
		sb.write_string('\r\n')
		sb.write_string(s[i..i + max_len])
		sb.write_string('\r\n')
	}
	rest := s.len - i
	sb.write_string(rest.hex())
	sb.write_string('\r\n')
	sb.write_string(s[i..])
	sb.write_string('\r\n')
	if rest > 0 {
		sb.write_string('0\r\n\r\n')
	}
	return sb.str()
}

// Parse a raw HTTP response into a Response object
pub fn parse_response(resp string) Response {
	mut header := new_header()
	// TODO: Cookie data type
	mut cookies := map[string]string{}
	first_header := resp.all_before('\n')
	mut status_code := 0
	if first_header.contains('HTTP/') {
		val := first_header.find_between(' ', ' ')
		status_code = val.int()
	}
	mut text := ''
	// Build resp header map and separate the body
	mut nl_pos := 3
	mut i := 1
	for {
		old_pos := nl_pos
		nl_pos = resp.index_after('\n', nl_pos + 1)
		if nl_pos == -1 {
			break
		}
		h := resp[old_pos + 1..nl_pos]
		// End of headers
		if h.len <= 1 {
			text = resp[nl_pos + 1..]
			break
		}
		i++
		pos := h.index(':') or { continue }
		mut key := h[..pos]
		val := h[pos + 2..].trim_space()
		header.add_custom(key, val) or { eprintln('$err; skipping header') }
	}
	// set cookies
	for cookie in header.values(.set_cookie) {
		parts := cookie.split_nth('=', 2)
		cookies[parts[0]] = parts[1]
	}
	if header.get(.transfer_encoding) or { '' } == 'chunked' {
		text = chunked.decode(text)
	}
	return Response{
		status_code: status_code
		header: header
		cookies: cookies
		text: text
	}
}
