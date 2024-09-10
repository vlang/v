// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.urllib

const max_redirects = 16 // safari max - other browsers allow up to 20

const content_type_default = 'text/plain'

const bufsize = 64 * 1024

// FetchConfig holds configuration data for the fetch function.
pub struct FetchConfig {
pub mut:
	url        string
	method     Method = .get
	header     Header
	data       string
	params     map[string]string
	cookies    map[string]string
	user_agent string  = 'v.http'
	user_ptr   voidptr = unsafe { nil }
	verbose    bool
	proxy      &HttpProxy = unsafe { nil }

	validate               bool   // set this to true, if you want to stop requests, when their certificates are found to be invalid
	verify                 string // the path to a rootca.pem file, containing trusted CA certificate(s)
	cert                   string // the path to a cert.pem file, containing client certificate(s) for the request
	cert_key               string // the path to a key.pem file, containing private keys for the client certificate(s)
	in_memory_verification bool   // if true, verify, cert, and cert_key are read from memory, not from a file
	allow_redirect         bool = true // whether to allow redirect
	max_retries            int  = 5    // maximum number of retries required when an underlying socket error occurs
	// callbacks to allow custom reporting code to run, while the request is running, and to implement streaming
	on_redirect      RequestRedirectFn     = unsafe { nil }
	on_progress      RequestProgressFn     = unsafe { nil }
	on_progress_body RequestProgressBodyFn = unsafe { nil }
	on_finish        RequestFinishFn       = unsafe { nil }

	stop_copying_limit   i64 = -1 // after this many bytes are received, stop copying to the response. Note that on_progress and on_progress_body callbacks, will continue to fire normally, until the full response is read, which allows you to implement streaming downloads, without keeping the whole big response in memory
	stop_receiving_limit i64 = -1 // after this many bytes are received, break out of the loop that reads the response, effectively stopping the request early. No more on_progress callbacks will be fired. The on_finish callback will fire.
}

// new_request creates a new Request given the request `method`, `url_`, and
// `data`.
pub fn new_request(method Method, url_ string, data string) Request {
	url := if method == .get && !url_.contains('?') { url_ + '?' + data } else { url_ }
	// println('new req() method=$method url="$url" dta="$data"')
	return Request{
		method: method
		url:    url
		data:   data
		/*
		headers: {
			'Accept-Encoding': 'compress'
		}
		*/
	}
}

// get sends a GET HTTP request to the given `url`.
pub fn get(url string) !Response {
	return fetch(method: .get, url: url)
}

// post sends the string `data` as an HTTP POST request to the given `url`.
pub fn post(url string, data string) !Response {
	return fetch(
		method: .post
		url:    url
		data:   data
		header: new_header(key: .content_type, value: content_type_default)
	)
}

// post_json sends the JSON `data` as an HTTP POST request to the given `url`.
pub fn post_json(url string, data string) !Response {
	return fetch(
		method: .post
		url:    url
		data:   data
		header: new_header(key: .content_type, value: 'application/json')
	)
}

// post_form sends the map `data` as X-WWW-FORM-URLENCODED data to an HTTP POST request
// to the given `url`.
pub fn post_form(url string, data map[string]string) !Response {
	return fetch(
		method: .post
		url:    url
		header: new_header(key: .content_type, value: 'application/x-www-form-urlencoded')
		data:   url_encode_form_data(data)
	)
}

pub fn post_form_with_cookies(url string, data map[string]string, cookies map[string]string) !Response {
	return fetch(
		method:  .post
		url:     url
		header:  new_header(key: .content_type, value: 'application/x-www-form-urlencoded')
		data:    url_encode_form_data(data)
		cookies: cookies
	)
}

@[params]
pub struct PostMultipartFormConfig {
pub mut:
	form   map[string]string
	files  map[string][]FileData
	header Header
}

// post_multipart_form sends multipart form data `conf` as an HTTP POST
// request to the given `url`.
pub fn post_multipart_form(url string, conf PostMultipartFormConfig) !Response {
	body, boundary := multipart_form_body(conf.form, conf.files)
	println(conf.header)
	mut header := conf.header
	header.set(.content_type, 'multipart/form-data; boundary="${boundary}"')
	return fetch(
		method: .post
		url:    url
		header: header
		data:   body
	)
}

// put sends string `data` as an HTTP PUT request to the given `url`.
pub fn put(url string, data string) !Response {
	return fetch(
		method: .put
		url:    url
		data:   data
		header: new_header(key: .content_type, value: content_type_default)
	)
}

// patch sends string `data` as an HTTP PATCH request to the given `url`.
pub fn patch(url string, data string) !Response {
	return fetch(
		method: .patch
		url:    url
		data:   data
		header: new_header(key: .content_type, value: content_type_default)
	)
}

// head sends an HTTP HEAD request to the given `url`.
pub fn head(url string) !Response {
	return fetch(method: .head, url: url)
}

// delete sends an HTTP DELETE request to the given `url`.
pub fn delete(url string) !Response {
	return fetch(method: .delete, url: url)
}

// prepare prepares a new request for fetching, but does not call its .do() method.
// It is useful, if you want to reuse request objects, for several requests in a row,
// modifying the request each time, then calling .do() to get the new response.
pub fn prepare(config FetchConfig) !Request {
	if config.url == '' {
		return error('http.fetch: empty url')
	}
	url := build_url_from_fetch(config) or { return error('http.fetch: invalid url ${config.url}') }
	req := Request{
		method:                 config.method
		url:                    url
		data:                   config.data
		header:                 config.header
		cookies:                config.cookies
		user_agent:             config.user_agent
		user_ptr:               config.user_ptr
		verbose:                config.verbose
		validate:               config.validate
		verify:                 config.verify
		cert:                   config.cert
		proxy:                  config.proxy
		cert_key:               config.cert_key
		in_memory_verification: config.in_memory_verification
		allow_redirect:         config.allow_redirect
		max_retries:            config.max_retries
		on_progress:            config.on_progress
		on_progress_body:       config.on_progress_body
		on_redirect:            config.on_redirect
		on_finish:              config.on_finish
		stop_copying_limit:     config.stop_copying_limit
		stop_receiving_limit:   config.stop_receiving_limit
	}
	return req
}

// TODO: @[noinline] attribute is used for temporary fix the 'get_text()' intermittent segfault / nil value when compiling with GCC 13.2.x and -prod option ( Issue #20506 )
// fetch sends an HTTP request to the `url` with the given method and configuration.
@[noinline]
pub fn fetch(config FetchConfig) !Response {
	req := prepare(config)!
	return req.do()!
}

// get_text sends an HTTP GET request to the given `url` and returns the text content of the response.
pub fn get_text(url string) string {
	resp := fetch(url: url, method: .get) or { return '' }
	return resp.body
}

// url_encode_form_data converts mapped data to a URL encoded string.
pub fn url_encode_form_data(data map[string]string) string {
	mut pieces := []string{}
	for key_, value_ in data {
		key := urllib.query_escape(key_)
		value := urllib.query_escape(value_)
		pieces << '${key}=${value}'
	}
	return pieces.join('&')
}

fn build_url_from_fetch(config FetchConfig) !string {
	mut url := urllib.parse(config.url)!
	if config.params.len == 0 {
		return url.str()
	}
	mut pieces := []string{cap: config.params.len}
	for key, val in config.params {
		pieces << '${key}=${val}'
	}
	mut query := pieces.join('&')
	if url.raw_query.len > 1 {
		query = url.raw_query + '&' + query
	}
	url.raw_query = query
	return url.str()
}
