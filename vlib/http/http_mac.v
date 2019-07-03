// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

#include <curl/curl.h>
#flag darwin -lcurl
#flag windows -lcurl
#flag linux -lcurl

fn C.curl_easy_init() *C.CURL 

fn foo() {} 

type wsfn fn (s string, ptr voidptr)

struct MemoryStruct {
	size     size_t
	ws_func  wsfn
	user_ptr voidptr // for wsfn
	strings  []string
}

import const (
	CURLOPT_WRITEFUNCTION
	CURLOPT_SSL_VERIFYPEER
	CURLOPT_HEADERFUNCTION
	CURLOPT_WRITEDATA
	CURLOPT_HEADERDATA
	CURLOPT_FOLLOWLOCATION
	CURLOPT_URL
	CURLOPT_VERBOSE
	CURLOPT_HTTP_VERSION
	CURL_HTTP_VERSION_1_1
	CURLOPT_HTTPHEADER
	CURLOPT_POSTFIELDS
	CURLOPT_CUSTOMREQUEST
	CURLOPT_TCP_KEEPALIVE
	CURLINFO_CONTENT_LENGTH_DOWNLOAD 
	CURLE_OK
)

fn C.curl_easy_strerror(curl voidptr) byteptr

fn C.curl_easy_perform(curl voidptr) C.CURLcode

fn write_fn(contents byteptr, size, nmemb int, _mem *MemoryStruct) int {
	mut mem := _mem
	// # printf("size =%d nmemb=%d contents=%s\n", size, nmemb, contents);
	realsize := size * nmemb// TODO size_t ?
	// if !isnil(mem.ws_func) {
	# if (mem->ws_func)
	{
		//C.printf('\n\nhttp_mac.m: GOT WS FUNC. size=%d\n', realsize)
		// Skip negative and 0 junk chars in the WS string
		mut start := 0
		for i := 0; i < realsize; i++ {
			// printf("char=%d %c\n", s[i], s[i]);
			if contents[i] == 0 && start == 0 {
				start = i
				break
			}
		}
		contents += start + 1
		// printf("GOOD CONTEnTS=%s\n", contents);
		s := string(contents)
		// mem.ws_func('kek', 0)
		# mem->ws_func(s, mem->user_ptr);
	}
	mut c := string(contents)
	c = c.trim_space()
	// Need to clone because libcurl reuses this memory
	mem.strings << c.clone()
	return realsize
}

struct C.curl_slist { }

fn (req &Request) do() Response {
	//println('req.do() mac/linux url="$req.url" data="$req.data"')
	// println('req.do() url="$req.url"')
	/* 
	mut resp := Response {
		headers: map[string]string{}
	}
*/
	mut headers := map[string]string{}
	// no data at this point
	chunk := MemoryStruct {
		ws_func: req.ws_func
		user_ptr: req.user_ptr
	}
	// header chunk
	hchunk := MemoryStruct {
		ws_func: foo 
		user_ptr: 0
	}
	// init curl
	curl := C.curl_easy_init()
	if isnil(curl) {
		println('curl init failed')
		return Response{}
	}
	// options
	// url2 := req.url.clone()
	C.curl_easy_setopt(curl, CURLOPT_URL, req.url.cstr())// ..clone())
	// C.curl_easy_setopt(curl, CURLOPT_URL, 'http://example.com')
	// return resp
	// curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0);
	$if windows {
		C.curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0)
	}
	C.curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_fn)
	C.curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, write_fn)
	C.curl_easy_setopt(curl, CURLOPT_WRITEDATA, &chunk)
	C.curl_easy_setopt(curl, CURLOPT_HEADERDATA, &hchunk)
	C.curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1)
	if req.typ == 'POST' {
		C.curl_easy_setopt(curl, CURLOPT_POSTFIELDS, req.data.cstr())
		C.curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, 'POST')
		// req.headers << 'Content-Type: application/x-www-form-urlencoded'
	}
	// Add request headers
	mut hlist := &C.curl_slist{!}
	// for i, h := range req.headers {
	for entry in req.headers.entries {
		key := entry.key
		val := req.headers[key]
		h := '$key: $val'
		hlist = C.curl_slist_append(hlist, h.cstr())
	}
	// curl_easy_setopt(curl, CURLOPT_HTTP_VERSION,	// (long)CURL_HTTP_VERSION_2TLS);�`C�ʀ9�
	C.curl_easy_setopt(curl, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_1_1)
	if req.verbose {
		C.curl_easy_setopt(curl, CURLOPT_VERBOSE, 1)
	}
	C.curl_easy_setopt(curl, CURLOPT_HTTPHEADER, hlist)
	C.curl_easy_setopt(curl, CURLOPT_TCP_KEEPALIVE, 1)
	C.curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1)
	//println('bef easy()')
	res := C.curl_easy_perform(curl)
	//println('after easy()')
	# if (res != CURLE_OK )
	{
		err := C.curl_easy_strerror(res)
		println('curl_easy_perform() failed: $err')
	}
	body := chunk.strings.join('')// string(chunk.memory)
	// chunk.strings.free()
	// resp.headers = hchunk.strings
	if hchunk.strings.len == 0 {
		return Response{}
	}
	first_header := hchunk.strings.first()
	mut status_code := 0
	if first_header.contains('HTTP/') {
		val := first_header.find_between(' ', ' ')
		status_code = val.int()
	}
	// Build resp headers map
	// println('building resp headers hchunk.strings.len')
	for h in hchunk.strings {
		// break
		// println(h)
		vals := h.split(':')
		pos := h.index(':')
		if pos == -1 {
			continue
		}
		if h.contains('Content-Type') {
			continue
		}
		key := h.left(pos)
		val := h.right(pos + 1)
		// println('"$key" *** "$val"')
		// val2 := val.trim_space()
		// println('val2="$val2"')
		headers[key] = val// val.trim_space()
	}
	// println('done')
	// j.println(resp.status_code)
	// println('body=')
	// j.println(resp.body)
	// j.println('headers=')
	// j.println(hchunk.strings)
	C.curl_easy_cleanup(curl)
	//println('end of req.do() url="$req.url"')
	return Response {
		body: body
	}
}

fn unescape(s string) string {
	return string(byteptr(C.curl_unescape(s.cstr(), s.len)))
}

fn escape(s string) string {
	return string(byteptr(C.curl_escape(s.cstr(), s.len)))
}

// ////////////////
fn (req &Request) do2() Response {
	mut resp := Response{}
	return resp
}

