// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

struct Request {
pub:
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
pub fn get(url string) Response {
	if url == '' {
		println('http: empty get url')
		return Response{}
	}
	mut req := new_request('GET', url, '')
	resp := req.do()
	return resp
}

pub fn post(url, data string) Response {
	req := new_request('POST', url, data)
	resp := req.do()
	return resp
}

pub fn new_request(typ, _url, _data string) *Request {
	mut url := _url
	mut data := _data
	// req.headers['User-Agent'] = 'V $VERSION'
	if typ == 'GET' && !url.contains('?') && data != '' {
		println('zeroing data, to url')
		url = '$url?$data'
		data = ''
	}
	// req.headers = new_map(0, sizeof(string))// []string{}
	return &Request {
		typ: typ
		url: url
		data: data
		ws_func: 0
		user_ptr: 0
		headers: map[string]string{} 
	}
}

/* 
fn (req &Request) do() Response {
	mut resp := Response{}
	return resp
}
*/
fn (req mut Request) free() {
	req.headers.free()
}

fn (resp mut Response) free() {
	resp.headers.free()
}

pub fn (req mut Request) add_header(key, val string) {
	req.headers[key] = val
}

