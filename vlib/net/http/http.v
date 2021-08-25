// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.urllib

const (
	max_redirects        = 4
	content_type_default = 'text/plain'
	bufsize              = 1536
)

// FetchConfig holds configurations of fetch
pub struct FetchConfig {
pub mut:
	url        string
	method     Method
	header     Header
	data       string
	params     map[string]string
	cookies    map[string]string
	user_agent string = 'v.http'
	verbose    bool
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
	return fetch(method: .get, url: url)
}

// post sends a POST HTTP request to the URL with a string data
pub fn post(url string, data string) ?Response {
	return fetch(
		method: .post
		url: url
		data: data
		header: new_header(key: .content_type, value: http.content_type_default)
	)
}

// post_json sends a POST HTTP request to the URL with a JSON data
pub fn post_json(url string, data string) ?Response {
	return fetch(
		method: .post
		url: url
		data: data
		header: new_header(key: .content_type, value: 'application/json')
	)
}

// post_form sends a POST HTTP request to the URL with X-WWW-FORM-URLENCODED data
pub fn post_form(url string, data map[string]string) ?Response {
	return fetch(
		method: .post
		url: url
		header: new_header(key: .content_type, value: 'application/x-www-form-urlencoded')
		data: url_encode_form_data(data)
	)
}

// put sends a PUT HTTP request to the URL with a string data
pub fn put(url string, data string) ?Response {
	return fetch(
		method: .put
		url: url
		data: data
		header: new_header(key: .content_type, value: http.content_type_default)
	)
}

// patch sends a PATCH HTTP request to the URL with a string data
pub fn patch(url string, data string) ?Response {
	return fetch(
		method: .patch
		url: url
		data: data
		header: new_header(key: .content_type, value: http.content_type_default)
	)
}

// head sends a HEAD HTTP request to the URL
pub fn head(url string) ?Response {
	return fetch(method: .head, url: url)
}

// delete sends a DELETE HTTP request to the URL
pub fn delete(url string) ?Response {
	return fetch(method: .delete, url: url)
}

// fetch sends an HTTP request to the URL with the given method and configurations
pub fn fetch(config FetchConfig) ?Response {
	if config.url == '' {
		return error('http.fetch: empty url')
	}
	url := build_url_from_fetch(config) or { return error('http.fetch: invalid url $config.url') }
	req := Request{
		method: config.method
		url: url
		data: config.data
		header: config.header
		cookies: config.cookies
		user_agent: config.user_agent
		user_ptr: 0
		verbose: config.verbose
	}
	res := req.do() ?
	return res
}

// get_text sends a GET HTTP request to the URL and returns the text content of the response
pub fn get_text(url string) string {
	resp := fetch(url: url, method: .get) or { return '' }
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

[deprecated: 'use fetch()']
fn fetch_with_method(method Method, _config FetchConfig) ?Response {
	mut config := _config
	config.method = method
	return fetch(config)
}

fn build_url_from_fetch(config FetchConfig) ?string {
	mut url := urllib.parse(config.url) ?
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
