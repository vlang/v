// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.http.chunked
import strconv

// Response represents the result of the request
pub struct Response {
pub mut:
	body         string
	text         string [deprecated: 'use Response.body instead'; deprecated_after: '2022-10-03']
	header       Header
	status_code  int
	status_msg   string
	http_version string
}

fn (mut resp Response) free() {
	unsafe { resp.header.free() }
}

// Formats resp to bytes suitable for HTTP response transmission
pub fn (resp Response) bytes() []u8 {
	// TODO: build []u8 directly; this uses two allocations
	return resp.bytestr().bytes()
}

// Formats resp to a string suitable for HTTP response transmission
pub fn (resp Response) bytestr() string {
	return 'HTTP/${resp.http_version} ${resp.status_code} ${resp.status_msg}\r\n' + '${resp.header.render(
		version: resp.version()
	)}\r\n' + resp.body
}

// Parse a raw HTTP response into a Response object
pub fn parse_response(resp string) !Response {
	version, status_code, status_msg := parse_status_line(resp.all_before('\n'))!
	// Build resp header map and separate the body
	start_idx, end_idx := find_headers_range(resp)!
	header := parse_headers(resp.substr(start_idx, end_idx))!
	mut body := resp.substr(end_idx, resp.len)
	if header.get(.transfer_encoding) or { '' } == 'chunked' {
		body = chunked.decode(body)
	}
	return Response{
		http_version: version
		status_code: status_code
		status_msg: status_msg
		header: header
		body: body
		text: body // TODO: remove as depreciated
	}
}

// parse_status_line parses the first HTTP response line into the HTTP
// version, status code, and reason phrase
fn parse_status_line(line string) !(string, int, string) {
	if line.len < 5 || line[..5].to_lower() != 'http/' {
		return error('response does not start with HTTP/')
	}
	data := line.split_nth(' ', 3)
	if data.len != 3 {
		return error('expected at least 3 tokens')
	}
	version := data[0].substr(5, data[0].len)
	// validate version is 1*DIGIT "." 1*DIGIT
	digits := version.split_nth('.', 3)
	if digits.len != 2 {
		return error('HTTP version malformed')
	}
	for digit in digits {
		strconv.atoi(digit) or { return error('HTTP version must contain only integers') }
	}
	return version, strconv.atoi(data[1])!, data[2]
}

// cookies parses the Set-Cookie headers into Cookie objects
pub fn (r Response) cookies() []Cookie {
	mut cookies := []Cookie{}
	for cookie in r.header.values(.set_cookie) {
		cookies << parse_cookie(cookie) or { continue }
	}
	return cookies
}

// status parses the status_code into a Status struct
pub fn (r Response) status() Status {
	return status_from_int(r.status_code)
}

// set_status sets the status_code and status_msg of the response
pub fn (mut r Response) set_status(s Status) {
	r.status_code = s.int()
	r.status_msg = s.str()
}

// version parses the version
pub fn (r Response) version() Version {
	return version_from_str('HTTP/${r.http_version}')
}

// set_version sets the http_version string of the response
pub fn (mut r Response) set_version(v Version) {
	if v == .unknown {
		r.http_version = ''
		return
	}
	maj, min := v.protos()
	r.http_version = '${maj}.${min}'
}

pub struct ResponseConfig {
	version Version = .v1_1
	status  Status  = .ok
	header  Header
	body    string
	text    string  [deprecated: 'use ResponseConfig.body instead'; deprecated_after: '2022-10-03']
}

// new_response creates a Response object from the configuration. This
// function will add a Content-Length header if body is not empty.
pub fn new_response(conf ResponseConfig) Response {
	mut resp := Response{
		body: conf.body + conf.text
		header: conf.header
	}
	if resp.body.len > 0 && !resp.header.contains(.content_length) {
		resp.header.add(.content_length, resp.body.len.str())
	}
	resp.set_status(conf.status)
	resp.set_version(conf.version)
	return resp
}

// find_headers_range returns the start (inclusive) and end (exclusive)
// index of the headers in the string, including the trailing newlines. This
// helper function expects the first line in `data` to be the HTTP status line
// (HTTP/1.1 200 OK).
fn find_headers_range(data string) !(int, int) {
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
