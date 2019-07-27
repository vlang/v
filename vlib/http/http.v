// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

struct Request {
pub mut:
	// headers  []string
	headers  map_string
	method   string
	// cookies map[string]string
	h        string
	cmd      string
	typ      string // GET POST
	data     string
	url      string
	ws_func  voidptr
	user_ptr voidptr
	verbose  bool
}

struct Response {
pub:
	body        string
	headers     map_string
	status_code int
}

// embed 'http'

pub fn request() *Request{
	return &Request{
		data:''
		ws_func: 0
		user_ptr: 0
		headers: map[string]string{}
	}
}

pub fn (r Request) get (url string) Response{
	mut req := request()
	req.typ = 'GET'
	req.url = url
	resp := req.do()
	return resp
}

pub fn (r Request) post (url string, data string) Response{
	mut req := request()
	req.typ = 'POST'
	req.url = url
	req.data = data
	resp := req.do()
	return resp
}

pub fn get(url string) Response {
	req := request()
	return req.get(url)
}

pub fn post(url, data string) Response {
	req := request()
	return req.post(url,data)
}

fn (req mut Request) free() {
	req.headers.free()
}

fn (resp mut Response) free() {
	resp.headers.free()
}

pub fn (req mut Request) add_header(key, val string) {
	req.headers[key] = val
}

