// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

import net.urllib
import http.chunked

struct Request {
pub:
	headers2  []string
	headers  map[string]string 
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
	text        string
	headers     map[string]string 
	status_code int
}

pub fn get(url string) ?Response {
	req := new_request('GET', url, '') or {
		return error(err)
	}
	return req.do()
}

pub fn post(url, data string) ?Response {
	req := new_request('POST', url, data) or {
		return error(err)
	}
	return req.do()
}

pub fn new_request(typ, _url, _data string) ?Request {
	if _url == '' {
		return error('bad url') 
	}
	mut url := _url
	mut data := _data
	// req.headers['User-Agent'] = 'V $VERSION'
	if typ == 'GET' && !url.contains('?') && data != '' {
		url = '$url?$data'
		data = ''
	}
	return Request {
		typ: typ
		url: url
		data: data
		ws_func: 0
		user_ptr: 0
		headers: map[string]string{} 
	}
}

pub fn get_text(url string) string {
	resp := get(url) or { return  '' } 
	return resp.text 
} 

fn (req mut Request) free() {
	req.headers.free()
}

fn (resp mut Response) free() {
	resp.headers.free()
}

pub fn (req mut Request) add_header(key, val string) {
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

pub fn (req &Request) do() Response {
	mut headers := map[string]string{}
	if req.typ == 'POST' {
		// req.headers << 'Content-Type: application/x-www-form-urlencoded'
	}
	for key, val in req.headers {
		//h := '$key: $val'
	}
	url := urllib.parse(req.url) or {
		// panic('http.request.do: invalid URL $req.url'
		return Response{} //error('ff')}
	}
	is_ssl := url.scheme == 'https'
	if !is_ssl {
		panic('non https requests are not supported right now') 
	}
	s := do_request(req.typ, url.host, url.path)
	// s := ssl_do(req.typ, url.host, url.path) 
	first_header := s.all_before('\n') 
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
		nl_pos = s.index_after('\n', nl_pos+1) 
		if nl_pos == -1 {
			break 
		} 
		h := s.substr(old_pos + 1, nl_pos) 
		// End of headers 
		if h.len <= 1 {
			text = s.right(nl_pos + 1) 
			break 
		} 
		i++ 
		pos := h.index(':')
		if pos == -1 {
			continue
		}
		//if h.contains('Content-Type') {
			//continue
		//}
		key := h.left(pos)
		val := h.right(pos + 2)
		headers[key] = val.trim_space()
	}
	if headers['Transfer-Encoding'] == 'chunked' {
		text = chunked.decode( text )
	}
	return Response {
		status_code: status_code 
		headers: headers
		text: text 
	} 
}

pub fn unescape_url(s string) string {
	panic('http.unescape_url() was replaced with urllib.query_unescape()') 
	return '' 
}

pub fn escape_url(s string) string {
	panic('http.escape_url() was replaced with urllib.query_escape()') 
	return '' 
}

pub fn unescape(s string) string {
	panic('http.unescape() was replaced with http.unescape_url()') 
	return '' 
}

pub fn escape(s string) string {
	panic('http.escape() was replaced with http.escape_url()') 
	return '' 
}

type wsfn fn (s string, ptr voidptr)

