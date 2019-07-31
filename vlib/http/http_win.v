// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

import time

#flag -lwininet -lShlwapi
#flag -lurlmon
#include <WinInet.h>
#include "urlmon.h"
#include <shlwapi.h>

import const (
	INTERNET_OPEN_TYPE_PRECONFIG
	INTERNET_DEFAULT_HTTP_PORT
	INTERNET_DEFAULT_HTTPS_PORT
	INTERNET_SERVICE_HTTP
	INTERNET_MAX_URL_LENGTH
	URL_ESCAPE_PERCENT
	URL_ESCAPE_SEGMENT_ONLY
)

const (
	BUF_MAX = 1024
	URL_ESCAPE_AS_UTF8 = 0x00040000 // missing in mingw, require Windows 7
	URL_ESCAPE_ASCII_URI_COMPONENT = 0x00080000 // missing in mingw, require Windows 8
)

pub fn (req &Request) do() Response {
	emptyresp := Response{}
	mut url := req.url
	//println('\n\nhttp.do() WIN URL="$url" TYP=$req.typ data="$req.data" headers.len=req.headers.len"')
	is_ssl := req.url.starts_with('https://')
	mut pos := url.index('://')
	if pos == -1 {return emptyresp}
	url = url.right(pos + 3)
	mut host := url
	mut path := '/'
	pos = url.index('/')
	if pos > -1 {
		host = url.left(pos)
		host = host.clone()
		path = url.right(pos)
	}
	mut headers := ''
	mut resp_headers := ''
	for key, val in req.headers {
		headers += '$key: $val\r\n'
	}
	if req.typ == 'POST' {
		headers += 'Content-Type: application/x-www-form-urlencoded'
	}
	data := req.data
	// Retrieve default http user agent
	user_agent := ''
	// DWORD szhttpUserAgent = sizeof(httpUseragent);
	// ObtainUserAgentString(0, httpUseragent, &szhttpUserAgent);
	// # HINTERNET internet = InternetOpenA(httpUseragent, INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
	internet := C.InternetOpen(user_agent.to_wide(), INTERNET_OPEN_TYPE_PRECONFIG, 0, 0, 0)
	// # if (!internet)
	if isnil(internet) {
		println('InternetOpen() failed')
		return emptyresp
	}
	port := int(if is_ssl{INTERNET_DEFAULT_HTTPS_PORT} else { INTERNET_DEFAULT_HTTP_PORT})
	// if is_ssl {
	// # port = INTERNET_DEFAULT_HTTPS_PORT;
	// }
	connect := C.InternetConnect(internet, host.to_wide(), port, 0, 0, INTERNET_SERVICE_HTTP, 0, 0)
	// # HINTERNET connect = InternetConnectA(internet, host.str, port, NULL, NULL,
	// # INTERNET_SERVICE_HTTP, 0, 0);
	# if (!connect)
	if isnil(connect) {
		e := C.GetLastError()
		println('[windows] InternetConnect() failed')
		C.printf('err=%d\n', e)
		return emptyresp
	}
	flags := 0
	#flags =
	# INTERNET_FLAG_HYPERLINK | INTERNET_FLAG_IGNORE_CERT_CN_INVALID |
	# INTERNET_FLAG_IGNORE_CERT_DATE_INVALID |
	# INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP |
	# INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS | INTERNET_FLAG_NO_AUTH |
	# INTERNET_FLAG_NO_CACHE_WRITE | INTERNET_FLAG_NO_UI |
	# INTERNET_FLAG_NO_COOKIES  |  // ...
	# INTERNET_FLAG_KEEP_CONNECTION |
	# INTERNET_FLAG_PRAGMA_NOCACHE | INTERNET_FLAG_RELOAD ;
	if is_ssl {
		#flags = flags | INTERNET_FLAG_SECURE;
	}
	request := C.HttpOpenRequest(connect, req.typ.to_wide(), path.to_wide(), 'HTTP/1.1'.to_wide(), 0, 0, flags, 0)
	// request := C.InternetOpenUrl(connect, req.typ.str, path.str, 'HTTP/1.1', 0, 0, flags, 0)
	// # HINTERNET request = HttpOpenRequest(connect, req->typ.str, path.str, "HTTP/1.1",
	// # NULL, NULL, flags, NULL);
	// # if (!request)
	if isnil(request) {
		println('HttpOpenRequest() failed')
		return emptyresp
	}
	// println('LEN BEFORE SEND=$headers.len ; $headers')
	ret := C.HttpSendRequest(request, headers.to_wide(), -1, data.str, data.len)
	// # printf("RET=%d\n", ret);
	// # int e = GetLastError();
	// # printf("e=%d\n", e);
	// Get response headers
	// Todo call twice to get len
	# LPSTR h_buf = malloc(1024);
	# DWORD dwSize = 1024;
	# HttpQueryInfo(request, HTTP_QUERY_RAW_HEADERS_CRLF,
	# h_buf,&dwSize,NULL);
	// Get response  body
	mut buf := [1025]byte
	mut nr_read := 0
	mut s := ''
	for	{
		ok := C.InternetReadFile(request, buf, BUF_MAX, &nr_read)
		if !ok {
			println('InternetReadFile() not ok ')
		}
		if ok && nr_read == 0 {
			if req.url.contains('websocket') {
				println('win sleeping 2')
				time.sleep(2)
				continue
			}
			break
		}
		buf[nr_read] = 0
		s += tos(buf, nr_read) // TODO perf
		nr_read = 0
	}
	C.InternetCloseHandle(request)
	C.InternetCloseHandle(connect)
	C.InternetCloseHandle(internet)
	# resp_headers = tos2(h_buf);
	hh := resp_headers.split('\n')
	mut resp := Response {
		text: s
		headers: map[string]string{}
		// headers: resp_headers
	}
	for h in hh {
		vals := h.split(':')
		hpos := h.index(':')
		if hpos == -1 {
			continue
		}
		key := h.left(hpos)
		val := h.right(hpos + 1)
		// println('$key => $val')
		resp.headers[key] = val.trim_space()
	}
	return resp
}

pub fn escape_url(s string) string {
	mut buf := &u16(malloc(INTERNET_MAX_URL_LENGTH * 2)) // INTERNET_MAX_URL_LENGTH * sizeof(wchar_t)
	mut nr_chars := INTERNET_MAX_URL_LENGTH
	res := C.UrlEscape(s.to_wide(), buf, &nr_chars, URL_ESCAPE_PERCENT | URL_ESCAPE_AS_UTF8 | URL_ESCAPE_ASCII_URI_COMPONENT)
	return string_from_wide2(buf, nr_chars)
}

pub fn unescape_url(s string) string {
	mut buf := &u16(malloc(INTERNET_MAX_URL_LENGTH * 2))
	mut nr_chars := INTERNET_MAX_URL_LENGTH
	res := C.UrlUnescape(s.to_wide(), &buf, &nr_chars, URL_ESCAPE_AS_UTF8 | URL_ESCAPE_ASCII_URI_COMPONENT)
	return string_from_wide2(buf, nr_chars)
}

pub fn unescape(s string) string {
	panic('http.unescape() was replaced with http.unescape_url()')
	return ''
}

pub fn escape(s string) string {
	panic('http.escape() was replaced with http.escape_url()')
	return ''
}

fn C.InternetReadFile(voidptr, voidptr, int, intptr) bool

