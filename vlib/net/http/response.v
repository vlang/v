// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.http.chunked

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

// Formats resp to bytes suitable for HTTP response transmission
pub fn (resp Response) bytes() []byte {
	// TODO: build []byte directly; this uses two allocations
	// TODO: cookies
	return ('$resp.version $resp.status_code ${status_from_int(resp.status_code).str()}\r\n' + '${resp.header.render(
		version: resp.version
	)}\r\n' + '$resp.text').bytes()
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
