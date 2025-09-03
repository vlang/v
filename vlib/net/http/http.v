// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.urllib

const max_redirects = 16 // safari max - other browsers allow up to 20

const content_type_default = 'text/plain'

const bufsize = 64 * 1024

// get sends a GET HTTP request to the given `url`.
pub fn get(url string, config ?Request) !Response {
	return fetch(url, &Request{ ...config, method: .get })
}

// post sends the string `data` as an HTTP POST request to the given `url`.
pub fn post(url string, config ?Request) !Response {
	return fetch(url, &Request{
		...config
		method: .post
		header: new_header(key: .content_type, value: content_type_default)
	})
}

// post_json sends the JSON `data` as an HTTP POST request to the given `url`.
pub fn post_json(url string, data string) !Response {
	return fetch(url, &Request{
		method: .post
		data:   data
		header: new_header(key: .content_type, value: 'application/json')
	})
}

// post_form sends the map `data` as X-WWW-FORM-URLENCODED data to an HTTP POST request
// to the given `url`.
pub fn post_form(url string, data map[string]string) !Response {
	return fetch(url, &Request{
		method: .post
		header: new_header(key: .content_type, value: 'application/x-www-form-urlencoded')
		data:   url_encode_form_data(data)
	})
}

pub fn post_form_with_cookies(url string, data map[string]string, cookies map[string]string) !Response {
	return fetch(url, &Request{
		method:  .post
		header:  new_header(key: .content_type, value: 'application/x-www-form-urlencoded')
		data:    url_encode_form_data(data)
		cookies: cookies
	})
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
	mut header := conf.header
	header.set(.content_type, 'multipart/form-data; boundary="${boundary}"')
	return fetch(url, &Request{
		method: .post
		header: header
		data:   body
	})
}

// put sends string `data` as an HTTP PUT request to the given `url`.
pub fn put(url string, config ?Request) !Response {
	return fetch(url, &Request{
		method: .put
		header: new_header(key: .content_type, value: content_type_default)
	})
}

// patch sends string `data` as an HTTP PATCH request to the given `url`.
pub fn patch(url string, config ?Request) !Response {
	return fetch(url, &Request{
		method: .head
		header: new_header(key: .content_type, value: content_type_default)
	})
}

// head sends an HTTP HEAD request to the given `url`.
pub fn head(url string, config ?Request) !Response {
	return fetch(url, &Request{ method: .head })
}

// delete sends an HTTP DELETE request to the given `url`.
pub fn delete(url string, config ?Request) !Response {
	return fetch(url, &Request{ method: .delete })
}

// prepare prepares a new request for fetching, but does not call its .do() method.
// It is useful, if you want to reuse request objects, for several requests in a row,
// modifying the request each time, then calling .do() to get the new response.
pub fn prepare(config Request) !Request {
	if config.url.raw_path == '' {
		return error('http.fetch: empty url')
	}
	// TODO: enter in more sanity checks for the values of Requests
	req := Request{
		...config
	}
	return req
}

fn (mut req Request) prepare() !Request {
	return prepare(req)
}

// TODO: @[noinline] attribute is used for temporary fix the 'get_text()' intermittent segfault / nil value when compiling with GCC 13.2.x and -prod option ( Issue #20506 )
// fetch sends an HTTP request to the `url` with the given method and configuration.
@[noinline]
pub fn fetch(url string, config ?&Request) !Response {
	url_parsed := urllib.parse(url)!
	mut req := Request{
		...config
		url: url_parsed
	}
	req.prepare()!
	return req.do()!
}

// get_text sends an HTTP GET request to the given `url` and returns the text content of the response.
pub fn get_text(url string, config ?Request) !string {
	resp := fetch(url, &Request{ ...config, method: .get }) or {
		println('fetch: error receiving response from fetch: ${err}')
		return err
	}
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
