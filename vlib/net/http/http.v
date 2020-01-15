// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.urllib
import net.http.chunked

const (
	max_redirects = 4
)

pub const (
	ContentTypeBinary = 'application/octet-stream'
	ContentTypeForm = 'application/x-www-form-urlencoded'
	ContentTypeJSON = 'application/json'
	ContentTypeHTML = 'text/html; charset=utf-8'
	ContentTypeText = 'text/plain; charset=utf-8'
)

pub struct Request {
pub:
	method     string
	headers    map[string]string
	cookies    map[string]string
	data       string
	url        urllib.URL
	user_agent string
	verbose    bool
	// h          string
	// cmd        string
mut:
	user_ptr   voidptr
	ws_func    voidptr
}

pub struct RequestConfig {
pub mut:
	method     string
	data       string=''
	params     map[string]string=map[string]string
	headers    map[string]string=map[string]string
	cookies    map[string]string=map[string]string
	user_agent string='v'
	verbose    bool=false
}

pub struct Response {
pub:
	text        string
	headers     map[string]string
	status_code int
}

pub fn get(url string, config RequestConfig) ?Response {
	return fetch_with_method('GET', url, config)
}

pub fn post(url string, config RequestConfig) ?Response {
	return fetch_with_method('POST', url, config)
}

pub fn put(url string, config RequestConfig) ?Response {
	return fetch_with_method('PUT', url, config)
}

pub fn patch(url string, config RequestConfig) ?Response {
	return fetch_with_method('PATCH', url, config)
}

pub fn delete(url string, config RequestConfig) ?Response {
	return fetch_with_method('DELETE', url, config)
}

pub fn head(url string, config RequestConfig) ?Response {
	return fetch_with_method('HEAD', url, config)
}

pub fn fetch(_url string, config RequestConfig) ?Response {
	if _url == '' {
		return error('http.fetch: empty url')
	}
	url := build_url_from_fetch(_url, config) or {
		return error('http.fetch: invalid url ${_url}')
	}
	data := config.data
	method := config.method.to_upper()
	req := Request{
		method: method
		url: url
		data: data
		headers: config.headers
		cookies: config.cookies
		user_agent: 'v'
		ws_func: 0
		user_ptr: 0
		verbose: config.verbose
	}
	res := req.do() or {
		return error(err)
	}
	return res
}

pub fn get_text(url string) string {
	resp := fetch(url, {
		method: 'GET'
	}) or {
		return ''
	}
	return resp.text
}

fn fetch_with_method(method string, url string, _config RequestConfig) ?Response {
	mut config := _config
	config.method = method
	return fetch(url, config)
}

fn build_url_from_fetch(_url string, config RequestConfig) ?urllib.URL {
	mut url := urllib.parse(_url) or {
		return error(err)
	}
	params := config.params
	if params.keys().len == 0 {
		return url
	}
	mut pieces := []string
	for key in params.keys() {
		pieces << '${key}=${params[key]}'
	}
	mut query := pieces.join('&')
	if url.raw_query.len > 1 {
		query = url.raw_query + '&' + query
	}
	url.raw_query = query
	return url
}

fn (req mut Request) free() {
	req.headers.free()
}

fn (resp mut Response) free() {
	resp.headers.free()
}

// add_header adds the key and value of an HTTP request header
pub fn (req mut Request) add_header(key, val string) {
	req.headers[key] = val
}

pub fn parse_headers(lines []string) map[string]string {
	mut headers := map[string]string
	for i, line in lines {
		if i == 0 {
			continue
		}
		words := line.split(': ')
		if words.len != 2 {
			continue
		}
		headers[words[0]] = words[1]
	}
	return headers
}

// do will send the HTTP request and returns `http.Response` as soon as the response is recevied
pub fn (req &Request) do() ?Response {
	mut rurl := req.url
	mut resp := Response{}
	mut no_redirects := 0
	for {
		if no_redirects == max_redirects {
			return error('http.request.do: maximum number of redirects reached ($max_redirects)')
		}
		qresp := req.method_and_url_to_response(req.method, rurl) or {
			return error(err)
		}
		resp = qresp
		if !(resp.status_code in [301, 302, 303, 307, 308]) {
			break
		}
		// follow any redirects
		redirect_url := resp.headers['Location']
		qrurl := urllib.parse(redirect_url) or {
			return error('http.request.do: invalid URL in redirect "$redirect_url"')
		}
		rurl = qrurl
		no_redirects++
	}
	return resp
}

fn (req &Request) method_and_url_to_response(method string, url net_dot_urllib.URL) ?Response {
	host_name := url.hostname()
	scheme := url.scheme
	p := url.path.trim_left('/')
	path := if url.query().size > 0 { '/$p?${url.query().encode()}' } else { '/$p' }
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
	if scheme == 'https' {
		// println('ssl_do( $nport, $method, $host_name, $path )')
		res := req.ssl_do(nport, method, host_name, path) or {
			return error(err)
		}
		return res
	}
	else if scheme == 'http' {
		// println('http_do( $nport, $method, $host_name, $path )')
		res := req.http_do(nport, method, host_name, path) or {
			return error(err)
		}
		return res
	}
	return error('http.request.method_and_url_to_response: unsupported scheme: "$scheme"')
}

fn parse_response(resp string) Response {
	mut headers := map[string]string
	first_header := resp.all_before('\n')
	mut status_code := 0
	if first_header.contains('HTTP/') {
		val := first_header.find_between(' ', ' ')
		status_code = val.int()
	}
	mut text := ''
	// Build resp headers map and separate the body
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
		pos := h.index(':') or {
			continue
		}
		// if h.contains('Content-Type') {
		// continue
		// }
		key := h[..pos]
		val := h[pos + 2..]
		headers[key] = val.trim_space()
	}
	if headers['Transfer-Encoding'] == 'chunked' {
		text = chunked.decode(text)
	}
	return Response{
		status_code: status_code
		headers: headers
		text: text
	}
}

fn (req &Request) build_request_headers(method, host_name, path string) string {
	ua := req.user_agent
	mut uheaders := []string
	for key, val in req.headers {
		uheaders << '${key}: ${val}\r\n'
	}
	if req.data.len > 0 {
		uheaders << 'Content-Length: ${req.data.len}\r\n'
	}
	return '$method $path HTTP/1.1\r\n' + 'Host: $host_name\r\n' + 'User-Agent: $ua\r\n' + uheaders.join('') + 'Connection: close\r\n\r\n' + req.data
}

pub fn unescape_url(s string) string {
	panic('http.unescape_url() was replaced with urllib.query_unescape()')
}

pub fn escape_url(s string) string {
	panic('http.escape_url() was replaced with urllib.query_escape()')
}

pub fn unescape(s string) string {
	panic('http.unescape() was replaced with http.unescape_url()')
}

pub fn escape(s string) string {
	panic('http.escape() was replaced with http.escape_url()')
}

type wsfn fn(s string, ptr voidptr)
