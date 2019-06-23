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
fn get(url string) string {
	if url == '' {
		println('http: empty get url')
		return ''
	}
	mut req := new_request('GET', url, '')
	resp := req.do()
	return resp.body
}

fn get2(url string) string {
	return ''
}

fn post(url, data string) string {
	req := new_request('POST', url, data)
	resp := req.do()
	return resp.body
}

fn new_request(typ, _url, _data string) *Request {
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
		url: _url
		data: _data
		ws_func: 0
		user_ptr: 0
		headers: new_map(0, sizeof(string))
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

fn (req mut Request) add_header(key, val string) {
	// println('start add header')
	// println('add header "$key" "$val"')
	// println(key)
	// println(val)
	// h := '$key: $val'
	// println('SET H')
	// req.headers << h
	req.headers[key] = val
	// mut h := req.h
	// h += ' -H "${key}: ${val}" '
	// req.h = h
}

