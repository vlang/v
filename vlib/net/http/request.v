// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import io
import net
import net.urllib
import rand
import strings
import time

pub type RequestRedirectFn = fn (request &Request, nredirects int, new_url string) !

pub type RequestProgressFn = fn (request &Request, chunk []u8, read_so_far u64) !

pub type RequestFinishFn = fn (request &Request, final_size u64) !

// Request holds information about an HTTP request (either received by
// a server or to be sent by a client)
pub struct Request {
pub mut:
	version    Version = .v1_1
	method     Method  = .get
	header     Header
	host       string
	cookies    map[string]string @[deprecated: 'use req.cookie(name) and req.add_cookie(name) instead']
	data       string
	url        string
	user_agent string = 'v.http'
	verbose    bool
	user_ptr   voidptr
	proxy      &HttpProxy = unsafe { nil }
	// NOT implemented for ssl connections
	// time = -1 for no timeout
	read_timeout  i64 = 30 * time.second
	write_timeout i64 = 30 * time.second
	//
	validate               bool // when true, certificate failures will stop further processing
	verify                 string
	cert                   string
	cert_key               string
	in_memory_verification bool // if true, verify, cert, and cert_key are read from memory, not from a file
	allow_redirect         bool = true // whether to allow redirect
	max_retries            int  = 5 // maximum number of retries required when an underlying socket error occurs
	// callbacks to allow custom reporting code to run, while the request is running
	on_redirect RequestRedirectFn = unsafe { nil }
	on_progress RequestProgressFn = unsafe { nil }
	on_finish   RequestFinishFn   = unsafe { nil }
}

fn (mut req Request) free() {
	unsafe { req.header.free() }
}

// add_header adds the key and value of an HTTP request header
// To add a custom header, use add_custom_header
pub fn (mut req Request) add_header(key CommonHeader, val string) {
	req.header.add(key, val)
}

// add_custom_header adds the key and value of an HTTP request header
// This method may fail if the key contains characters that are not permitted
pub fn (mut req Request) add_custom_header(key string, val string) ! {
	return req.header.add_custom(key, val)
}

// add_cookie adds a cookie to the request.
pub fn (mut req Request) add_cookie(c Cookie) {
	req.cookies[c.name] = c.value
}

// cookie returns the named cookie provided in the request or `none` if not found.
// If multiple cookies match the given name, only one cookie will be returned.
pub fn (req &Request) cookie(name string) ?Cookie {
	// TODO(alex) this should work once Cookie is used
	// return req.cookies[name] or { none }

	if value := req.cookies[name] {
		return Cookie{
			name: name
			value: value
		}
	}
	return none
}

// do will send the HTTP request and returns `http.Response` as soon as the response is received
pub fn (req &Request) do() !Response {
	mut url := urllib.parse(req.url) or { return error('http.Request.do: invalid url ${req.url}') }
	mut rurl := url
	mut resp := Response{}
	mut nredirects := 0
	for {
		if nredirects == max_redirects {
			return error('http.request.do: maximum number of redirects reached (${max_redirects})')
		}
		qresp := req.method_and_url_to_response(req.method, rurl)!
		resp = qresp
		if !req.allow_redirect {
			break
		}
		if resp.status() !in [.moved_permanently, .found, .see_other, .temporary_redirect,
			.permanent_redirect] {
			break
		}
		// follow any redirects
		mut redirect_url := resp.header.get(.location) or { '' }
		if redirect_url.len > 0 && redirect_url[0] == `/` {
			url.set_path(redirect_url) or {
				return error('http.request.do: invalid path in redirect: "${redirect_url}"')
			}
			redirect_url = url.str()
		}
		if req.on_redirect != unsafe { nil } {
			req.on_redirect(req, nredirects, redirect_url)!
		}
		qrurl := urllib.parse(redirect_url) or {
			return error('http.request.do: invalid URL in redirect "${redirect_url}"')
		}
		rurl = qrurl
		nredirects++
	}
	return resp
}

fn (req &Request) method_and_url_to_response(method Method, url urllib.URL) !Response {
	host_name := url.hostname()
	scheme := url.scheme
	p := url.escaped_path().trim_left('/')
	path := if url.query().len > 0 { '/${p}?${url.query().encode()}' } else { '/${p}' }
	mut nport := url.port().int()
	if nport == 0 {
		if scheme == 'http' {
			nport = 80
		}
		if scheme == 'https' {
			nport = 443
		}
	}
	// println('fetch $method, $scheme, $host_name, $nport, $path ')
	if scheme == 'https' && req.proxy == unsafe { nil } {
		// println('ssl_do( $nport, $method, $host_name, $path )')
		for i in 0 .. req.max_retries {
			res := req.ssl_do(nport, method, host_name, path) or {
				if i == req.max_retries - 1 || is_no_need_retry_error(err.code()) {
					return err
				}
				continue
			}
			return res
		}
	} else if scheme == 'http' && req.proxy == unsafe { nil } {
		// println('http_do( $nport, $method, $host_name, $path )')
		for i in 0 .. req.max_retries {
			res := req.http_do('${host_name}:${nport}', method, path) or {
				if i == req.max_retries - 1 || is_no_need_retry_error(err.code()) {
					return err
				}
				continue
			}
			return res
		}
	} else if req.proxy != unsafe { nil } {
		for i in 0 .. req.max_retries {
			res := req.proxy.http_do(url, method, path, req) or {
				if i == req.max_retries - 1 || is_no_need_retry_error(err.code()) {
					return err
				}
				continue
			}
			return res
		}
	}
	return error('http.request.method_and_url_to_response: unsupported scheme: "${scheme}"')
}

fn (req &Request) build_request_headers(method Method, host_name string, path string) string {
	ua := req.user_agent
	mut uheaders := []string{}
	if !req.header.contains(.host) {
		uheaders << 'Host: ${host_name}\r\n'
	}
	if !req.header.contains(.user_agent) {
		uheaders << 'User-Agent: ${ua}\r\n'
	}
	if req.data.len > 0 && !req.header.contains(.content_length) {
		uheaders << 'Content-Length: ${req.data.len}\r\n'
	}
	for key in req.header.keys() {
		if key == CommonHeader.cookie.str() {
			continue
		}
		val := req.header.custom_values(key).join('; ')
		uheaders << '${key}: ${val}\r\n'
	}
	uheaders << req.build_request_cookies_header()
	version := if req.version == .unknown { Version.v1_1 } else { req.version }
	return '${method} ${path} ${version}\r\n' + uheaders.join('') + 'Connection: close\r\n\r\n' +
		req.data
}

fn (req &Request) build_request_cookies_header() string {
	if req.cookies.keys().len < 1 {
		return ''
	}
	mut cookie := []string{}
	for key, val in req.cookies {
		cookie << '${key}=${val}'
	}
	cookie << req.header.values(.cookie)
	return 'Cookie: ' + cookie.join('; ') + '\r\n'
}

fn (req &Request) http_do(host string, method Method, path string) !Response {
	host_name, _ := net.split_address(host)!
	s := req.build_request_headers(method, host_name, path)
	mut client := net.dial_tcp(host)!
	client.set_read_timeout(req.read_timeout)
	client.set_write_timeout(req.write_timeout)
	// TODO this really needs to be exposed somehow
	client.write(s.bytes())!
	$if trace_http_request ? {
		eprintln('> ${s}')
	}
	mut bytes := req.read_all_from_client_connection(client)!
	client.close()!
	response_text := bytes.bytestr()
	$if trace_http_response ? {
		eprintln('< ${response_text}')
	}
	if req.on_finish != unsafe { nil } {
		req.on_finish(req, u64(response_text.len))!
	}
	return parse_response(response_text)
}

fn (req &Request) read_all_from_client_connection(r &net.TcpConn) ![]u8 {
	mut read := i64(0)
	mut b := []u8{len: 32768}
	for {
		old_read := read
		new_read := r.read(mut b[read..]) or { break }
		if new_read <= 0 {
			break
		}
		read += new_read
		if req.on_progress != unsafe { nil } {
			req.on_progress(req, b[old_read..read], u64(read))!
		}
		for b.len <= read {
			unsafe { b.grow_len(4096) }
		}
	}
	return b[..read]
}

// referer returns 'Referer' header value of the given request
pub fn (req &Request) referer() string {
	return req.header.get(.referer) or { '' }
}

// parse_request parses a raw HTTP request into a Request object.
// See also: `parse_request_head`, which parses only the headers.
pub fn parse_request(mut reader io.BufferedReader) !Request {
	mut request := parse_request_head(mut reader)!

	// body
	mut body := []u8{}
	if length := request.header.get(.content_length) {
		n := length.int()
		if n > 0 {
			body = []u8{len: n}
			mut count := 0
			for count < body.len {
				count += reader.read(mut body[count..]) or { break }
			}
		}
	}

	request.data = body.bytestr()
	return request
}

// parse_request_head parses *only* the header of a raw HTTP request into a Request object
pub fn parse_request_head(mut reader io.BufferedReader) !Request {
	// request line
	mut line := reader.read_line()!
	method, target, version := parse_request_line(line)!

	// headers
	mut header := new_header()
	line = reader.read_line()!
	for line != '' {
		// key, value := parse_header(line)!
		mut pos := parse_header_fast(line)!
		key := line.substr_unsafe(0, pos)
		for pos < line.len - 1 && line[pos + 1].is_space() {
			if line[pos + 1].is_space() {
				// Skip space or tab in value name
				pos++
			}
		}
		value := line.substr_unsafe(pos + 1, line.len)
		_, _ = key, value
		// println('key,value=${key},${value}')
		header.add_custom(key, value)!
		line = reader.read_line()!
	}
	// header.coerce(canonicalize: true)

	mut request_cookies := map[string]string{}
	for _, cookie in read_cookies(header, '') {
		request_cookies[cookie.name] = cookie.value
	}

	return Request{
		method: method
		url: target.str()
		header: header
		host: header.get(.host) or { '' }
		version: version
		cookies: request_cookies
	}
}

fn parse_request_line(s string) !(Method, urllib.URL, Version) {
	// println('S=${s}')
	// words := s.split(' ')
	// println(words)
	space1, space2 := fast_request_words(s)
	// if words.len != 3 {
	if space1 == 0 || space2 == 0 {
		return error('malformed request line')
	}
	method_str := s.substr_unsafe(0, space1)
	target_str := s.substr_unsafe(space1 + 1, space2)
	version_str := s.substr_unsafe(space2 + 1, s.len)
	// println('${method_str}!${target_str}!${version_str}')
	// method := method_from_str(words[0])
	// target := urllib.parse(words[1])!
	// version := version_from_str(words[2])
	method := method_from_str(method_str)
	target := urllib.parse(target_str)!
	version := version_from_str(version_str)
	if version == .unknown {
		return error('unsupported version')
	}
	return method, target, version
}

// Parse URL encoded key=value&key=value forms
//
// FIXME: Some servers can require the
// parameter in a specific order.
//
// a possible solution is to use the a list of QueryValue
pub fn parse_form(body string) map[string]string {
	mut form := map[string]string{}

	if body.match_glob('{*}') {
		form['json'] = body
	} else {
		words := body.split('&')

		for word in words {
			kv := word.split_nth('=', 2)
			if kv.len != 2 {
				continue
			}
			key := urllib.query_unescape(kv[0]) or { continue }
			val := urllib.query_unescape(kv[1]) or { continue }
			form[key] = val
		}
	}
	return form
	// }
	// todo: parse form-data and application/json
	// ...
}

pub struct FileData {
pub:
	filename     string
	content_type string
	data         string
}

pub struct UnexpectedExtraAttributeError {
	Error
	attributes []string
}

pub fn (err UnexpectedExtraAttributeError) msg() string {
	return 'Encountered unexpected extra attributes: ${err.attributes}'
}

pub struct MultiplePathAttributesError {
	Error
}

pub fn (err MultiplePathAttributesError) msg() string {
	return 'Expected at most one path attribute'
}

// multipart_form_body converts form and file data into a multipart/form
// HTTP request body. It is the inverse of parse_multipart_form. Returns
// (body, boundary).
// Note: Form keys should not contain quotes
fn multipart_form_body(form map[string]string, files map[string][]FileData) (string, string) {
	rboundary := rand.ulid()
	mut sb := strings.new_builder(1024)
	for name, value in form {
		sb.write_string('\r\n--')
		sb.write_string(rboundary)
		sb.write_string('\r\nContent-Disposition: form-data; name="')
		sb.write_string(name)
		sb.write_string('"\r\n\r\n')
		sb.write_string(value)
	}
	for name, fs in files {
		for f in fs {
			sb.write_string('\r\n--')
			sb.write_string(rboundary)
			sb.write_string('\r\nContent-Disposition: form-data; name="')
			sb.write_string(name)
			sb.write_string('"; filename="')
			sb.write_string(f.filename)
			sb.write_string('"\r\nContent-Type: ')
			sb.write_string(f.content_type)
			sb.write_string('\r\n\r\n')
			sb.write_string(f.data)
		}
	}
	sb.write_string('\r\n--')
	sb.write_string(rboundary)
	sb.write_string('--')
	return sb.str(), rboundary
}

struct LineSegmentIndexes {
mut:
	start int
	end   int
}

// parse_multipart_form parses an http request body, given a boundary string
// For more details about multipart forms, see:
//   https://datatracker.ietf.org/doc/html/rfc2183
//   https://datatracker.ietf.org/doc/html/rfc2388
//   https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Disposition
pub fn parse_multipart_form(body string, boundary string) (map[string]string, map[string][]FileData) {
	// dump(body)
	// dump(boundary)
	mut form := map[string]string{}
	mut files := map[string][]FileData{}
	// TODO: do not use split, but only indexes, to reduce copying of potentially large data
	sections := body.split(boundary)
	fields := sections[1..sections.len - 1]
	for field in fields {
		mut line_segments := []LineSegmentIndexes{cap: 100}
		mut line_idx, mut line_start := 0, 0
		for cidx, c in field {
			if line_idx >= 6 {
				// no need to scan further
				break
			}
			if c == `\n` {
				line_segments << LineSegmentIndexes{line_start, cidx}
				line_start = cidx + 1
				line_idx++
			}
		}
		line_segments << LineSegmentIndexes{line_start, field.len}
		line1 := field[line_segments[1].start..line_segments[1].end]
		line2 := field[line_segments[2].start..line_segments[2].end]
		disposition := parse_disposition(line1.trim_space())
		// Grab everything between the double quotes
		name := disposition['name'] or { continue }
		// Parse files
		// TODO: handle `filename*`, see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Disposition
		if filename := disposition['filename'] {
			// reject early broken content
			if line_segments.len < 5 {
				continue
			}
			// reject early non Content-Type headers
			if !line2.to_lower().starts_with('content-type:') {
				continue
			}
			content_type := line2.split_nth(':', 2)[1].trim_space()
			// line1: Content-Disposition: form-data; name="upfile"; filename="photo123.jpg"
			// line2: Content-Type: image/jpeg
			// line3:
			// line4: DATA
			// ...
			// lineX: --
			data := field[line_segments[4].start..field.len - 4] // each multipart field ends with \r\n--
			// dump(data.limit(20).bytes())
			// dump(data.len)
			files[name] << FileData{
				filename: filename
				content_type: content_type
				data: data
			}
			continue
		}
		if line_segments.len < 4 {
			continue
		}
		form[name] = field[line_segments[3].start..field.len - 4]
	}
	// dump(form)
	return form, files
}

// parse_disposition parses the Content-Disposition header of a multipart form.
// Returns a map of the key="value" pairs
// Example: assert parse_disposition('Content-Disposition: form-data; name="a"; filename="b"') == {'name': 'a', 'filename': 'b'}
fn parse_disposition(line string) map[string]string {
	mut data := map[string]string{}
	for word in line.split(';') {
		kv := word.split_nth('=', 2)
		if kv.len != 2 {
			continue
		}
		key, value := kv[0].to_lower().trim_left(' \t'), kv[1]
		if value.starts_with('"') && value.ends_with('"') {
			data[key] = value[1..value.len - 1]
		} else {
			data[key] = value
		}
	}
	return data
}

fn is_no_need_retry_error(err_code int) bool {
	return err_code in [
		net.err_port_out_of_range.code(),
		net.err_no_udp_remote.code(),
		net.err_connect_timed_out.code(),
		net.err_timed_out_code,
	]
}
