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
	return resp.bytestr().bytes()
}

// Formats resp to a string suitable for HTTP response transmission
pub fn (resp Response) bytestr() string {
	// TODO: cookies
	return ('$resp.version $resp.status_code ${status_from_int(resp.status_code).str()}\r\n' + '${resp.header.render(
		version: resp.version
	)}\r\n' + '$resp.text')
}

// Parse a raw HTTP response into a Response object
pub fn parse_response(resp string) ?Response {
	// TODO: Cookie data type
	mut cookies := map[string]string{}
	first_header := resp.all_before('\n')
	mut status_code := 0
	if first_header.contains('HTTP/') {
		val := first_header.find_between(' ', ' ')
		status_code = val.int()
	}
	// Build resp header map and separate the body
	start_idx, end_idx := find_headers_range(resp) ?
	header := parse_headers(resp.substr(start_idx, end_idx)) ?
	mut text := resp.substr(end_idx, resp.len)
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

// find_headers_range returns the start (inclusive) and end (exclusive)
// index of the headers in the string, including the trailing newlines. This
// helper function expects the first line in `data` to be the HTTP status line
// (HTTP/1.1 200 OK).
fn find_headers_range(data string) ?(int, int) {
	start_idx := data.index('\n') or { return error('no start index found') } + 1
	mut count := 0
	for i := start_idx; i < data.len; i++ {
		if data[i] == `\n` {
			count++
		} else if data[i] != `\r` {
			count = 0
		}
		if count == 2 {
			return start_idx, i + 1
		}
	}
	return error('no end index found')
}
