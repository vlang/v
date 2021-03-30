// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.urllib
import net.http.chunked
import net
import io

const (
	max_redirects        = 4
	content_type_default = 'text/plain'
	bufsize              = 1536
)

// Request holds information about an HTTP request
pub struct Request {
pub mut:
	version    Version = .v1_1
	method     Method
	headers    map[string]string // original requset headers
	lheaders   map[string]string // same as headers, but with normalized lowercased keys (for received requests)
	cookies    map[string]string
	data       string
	url        string
	user_agent string = 'v.http'
	verbose    bool
	user_ptr   voidptr
	ws_func    voidptr
}

// FetchConfig holds configurations of fetch
pub struct FetchConfig {
pub mut:
	method     Method
	data       string
	params     map[string]string
	headers    map[string]string
	cookies    map[string]string
	user_agent string = 'v.http'
	verbose    bool
}

// Response represents the result of the request
pub struct Response {
pub:
	text        string
	headers     map[string]string // original response headers, 'Set-Cookie' or 'set-Cookie', etc.
	lheaders    map[string]string // same as headers, but with normalized lowercased keys, like 'set-cookie'
	cookies     map[string]string
	status_code int
}

pub fn new_request(method Method, url_ string, data string) ?Request {
	url := if method == .get { url_ + '?' + data } else { url_ }
	// println('new req() method=$method url="$url" dta="$data"')
	return Request{
		method: method
		url: url
		data: data
		/*
		headers: {
			'Accept-Encoding': 'compress'
		}
		*/
	}
}

// get sends a GET HTTP request to the URL
pub fn get(url string) ?Response {
	return fetch_with_method(.get, url, FetchConfig{})
}

// post sends a POST HTTP request to the URL with a string data
pub fn post(url string, data string) ?Response {
	return fetch_with_method(.post, url,
		data: data
		headers: map{
			'Content-Type': http.content_type_default
		}
	)
}

// post_json sends a POST HTTP request to the URL with a JSON data
pub fn post_json(url string, data string) ?Response {
	return fetch_with_method(.post, url,
		data: data
		headers: map{
			'Content-Type': 'application/json'
		}
	)
}

// post_form sends a POST HTTP request to the URL with X-WWW-FORM-URLENCODED data
pub fn post_form(url string, data map[string]string) ?Response {
	return fetch_with_method(.post, url,
		headers: map{
			'Content-Type': 'application/x-www-form-urlencoded'
		}
		data: url_encode_form_data(data)
	)
}

// put sends a PUT HTTP request to the URL with a string data
pub fn put(url string, data string) ?Response {
	return fetch_with_method(.put, url,
		data: data
		headers: map{
			'Content-Type': http.content_type_default
		}
	)
}

// patch sends a PATCH HTTP request to the URL with a string data
pub fn patch(url string, data string) ?Response {
	return fetch_with_method(.patch, url,
		data: data
		headers: map{
			'Content-Type': http.content_type_default
		}
	)
}

// head sends a HEAD HTTP request to the URL
pub fn head(url string) ?Response {
	return fetch_with_method(.head, url, FetchConfig{})
}

// delete sends a DELETE HTTP request to the URL
pub fn delete(url string) ?Response {
	return fetch_with_method(.delete, url, FetchConfig{})
}

// fetch sends an HTTP request to the URL with the given method and configurations
pub fn fetch(_url string, config FetchConfig) ?Response {
	if _url == '' {
		return error('http.fetch: empty url')
	}
	url := build_url_from_fetch(_url, config) or { return error('http.fetch: invalid url $_url') }
	data := config.data
	req := Request{
		method: config.method
		url: url
		data: data
		headers: config.headers
		cookies: config.cookies
		user_agent: config.user_agent
		ws_func: 0
		user_ptr: 0
		verbose: config.verbose
	}
	res := req.do() ?
	return res
}

// get_text sends a GET HTTP request to the URL and returns the text content of the response
pub fn get_text(url string) string {
	resp := fetch(url, method: .get) or { return '' }
	return resp.text
}

// url_encode_form_data converts mapped data to an URL encoded string
pub fn url_encode_form_data(data map[string]string) string {
	mut pieces := []string{}
	for key_, value_ in data {
		key := urllib.query_escape(key_)
		value := urllib.query_escape(value_)
		pieces << '$key=$value'
	}
	return pieces.join('&')
}

fn fetch_with_method(method Method, url string, _config FetchConfig) ?Response {
	mut config := _config
	config.method = method
	return fetch(url, config)
}

fn build_url_from_fetch(_url string, config FetchConfig) ?string {
	mut url := urllib.parse(_url) ?
	if config.params.len == 0 {
		return url.str()
	}
	mut pieces := []string{cap: config.params.len}
	for key, val in config.params {
		pieces << '$key=$val'
	}
	mut query := pieces.join('&')
	if url.raw_query.len > 1 {
		query = url.raw_query + '&' + query
	}
	url.raw_query = query
	return url.str()
}

fn (mut req Request) free() {
	unsafe { req.headers.free() }
}

fn (mut resp Response) free() {
	unsafe { resp.headers.free() }
}

// add_header adds the key and value of an HTTP request header
pub fn (mut req Request) add_header(key string, val string) {
	req.headers[key] = val
}

// parse_headers parses HTTP header strings to mapped data
pub fn parse_headers(lines []string) map[string]string {
	mut headers := map[string]string{}
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
	mut url := urllib.parse(req.url) or { return error('http.Request.do: invalid url $req.url') }
	mut rurl := url
	mut resp := Response{}
	mut no_redirects := 0
	for {
		if no_redirects == http.max_redirects {
			return error('http.request.do: maximum number of redirects reached ($http.max_redirects)')
		}
		qresp := req.method_and_url_to_response(req.method, rurl) ?
		resp = qresp
		if resp.status_code !in [301, 302, 303, 307, 308] {
			break
		}
		// follow any redirects
		mut redirect_url := resp.lheaders['location']
		if redirect_url.len > 0 && redirect_url[0] == `/` {
			url.set_path(redirect_url) or {
				return error('http.request.do: invalid path in redirect: "$redirect_url"')
			}
			redirect_url = url.str()
		}
		qrurl := urllib.parse(redirect_url) or {
			return error('http.request.do: invalid URL in redirect "$redirect_url"')
		}
		rurl = qrurl
		no_redirects++
	}
	return resp
}

fn (req &Request) method_and_url_to_response(method Method, url urllib.URL) ?Response {
	host_name := url.hostname()
	scheme := url.scheme
	p := url.path.trim_left('/')
	path := if url.query().len > 0 { '/$p?$url.query().encode()' } else { '/$p' }
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
		res := req.ssl_do(nport, method, host_name, path) ?
		return res
	} else if scheme == 'http' {
		// println('http_do( $nport, $method, $host_name, $path )')
		res := req.http_do('$host_name:$nport', method, path) ?
		return res
	}
	return error('http.request.method_and_url_to_response: unsupported scheme: "$scheme"')
}

fn parse_response(resp string) Response {
	// TODO: Header data type
	mut headers := map[string]string{}
	mut lheaders := map[string]string{}
	// TODO: Cookie data type
	mut cookies := map[string]string{}
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
		pos := h.index(':') or { continue }
		// if h.contains('Content-Type') {
		// continue
		// }
		mut key := h[..pos]
		lkey := key.to_lower()
		val := h[pos + 2..]
		if lkey == 'set-cookie' {
			parts := val.trim_space().split('=')
			cookies[parts[0]] = parts[1]
		}
		tval := val.trim_space()
		headers[key] = tval
		lheaders[lkey] = tval
	}
	if lheaders['transfer-encoding'] == 'chunked' || lheaders['content-length'] == '' {
		text = chunked.decode(text)
	}
	return Response{
		status_code: status_code
		headers: headers
		lheaders: lheaders
		cookies: cookies
		text: text
	}
}

fn (req &Request) build_request_headers(method Method, host_name string, path string) string {
	ua := req.user_agent
	mut uheaders := []string{}
	if 'Host' !in req.headers {
		uheaders << 'Host: $host_name\r\n'
	}
	if 'User-Agent' !in req.headers {
		uheaders << 'User-Agent: $ua\r\n'
	}
	if req.data.len > 0 && 'Content-Length' !in req.headers {
		uheaders << 'Content-Length: $req.data.len\r\n'
	}
	for key, val in req.headers {
		if key == 'Cookie' {
			continue
		}
		uheaders << '$key: $val\r\n'
	}
	uheaders << req.build_request_cookies_header()
	version := if req.version == .unknown { Version.v1_1 } else { req.version }
	return '$method $path $version\r\n' + uheaders.join('') + 'Connection: close\r\n\r\n' + req.data
}

fn (req &Request) build_request_cookies_header() string {
	if req.cookies.keys().len < 1 {
		return ''
	}
	mut cookie := []string{}
	for key, val in req.cookies {
		cookie << '$key=$val'
	}
	if 'Cookie' in req.headers && req.headers['Cookie'] != '' {
		cookie << req.headers['Cookie']
	}
	return 'Cookie: ' + cookie.join('; ') + '\r\n'
}

// unescape_url is deprecated, use urllib.query_unescape() instead
pub fn unescape_url(s string) string {
	panic('http.unescape_url() was replaced with urllib.query_unescape()')
}

// escape_url is deprecated, use urllib.query_escape() instead
pub fn escape_url(s string) string {
	panic('http.escape_url() was replaced with urllib.query_escape()')
}

// unescape is deprecated, use urllib.query_escape() instead
pub fn unescape(s string) string {
	panic('http.unescape() was replaced with http.unescape_url()')
}

// escape is deprecated, use urllib.query_unescape() instead
pub fn escape(s string) string {
	panic('http.escape() was replaced with http.escape_url()')
}

fn (req &Request) http_do(host string, method Method, path string) ?Response {
	host_name, _ := net.split_address(host) ?
	s := req.build_request_headers(method, host_name, path)
	mut client := net.dial_tcp(host) ?
	// TODO this really needs to be exposed somehow
	client.write(s.bytes()) ?
	$if trace_http_request ? {
		eprintln('> $s')
	}
	mut bytes := io.read_all(reader: client) ?
	client.close() ?
	response_text := bytes.bytestr()
	$if trace_http_response ? {
		eprintln('< $response_text')
	}
	return parse_response(response_text)
}

// referer returns 'Referer' header value of the given request
pub fn (req &Request) referer() string {
	return req.headers['Referer']
}
