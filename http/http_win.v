// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

#flag -lwininet
#flag -lurlmon
// #include <WinInet.h>
#include "urlmon.h"
#include <shlwapi.h>
// #LPWSTR winstring(string s);
// # bool ok = InternetReadFile(request, buf, BUF_MAX, &nr_read);
import const (
	INTERNET_OPEN_TYPE_PRECONFIG
	INTERNET_DEFAULT_HTTP_PORT
	INTERNET_DEFAULT_HTTPS_PORT
	INTERNET_SERVICE_HTTP
)

fn (req &Request) do() Response {
	mut s := ''
	emptyresp := Response{}
	mut url := req.url
	println('\n\nhttp.do() WIN URL="$url" TYP=$req.typ data="$req.data" headers.len=req.headers.len"')
	println(req.headers)
	is_ssl := req.url.starts_with('https://')
	println('is ssl=$is_ssl')
	mut pos := url.index('/')
	url = url.right(pos + 2)
	mut host := url
	mut path := '/'
	pos = url.index('/')
	if pos > -1 {
		host = url.left(pos)
		host = host.clone()
		path = url.right(pos)
	}
	// println('HOST="$host"')
	// println('PATH="$path"')
	mut headers := ''
	mut resp_headers := ''
	// for header in req.headers {
	for entry in req.headers.entries {
		// headers += '$header\r\n'
		key := entry.key
		val := req.headers[key]
		headers += '$key: $val\r\n'
	}
	if req.typ == 'POST' {
		headers += 'Content-Type: application/x-www-form-urlencoded'
	}
	// headers = headers.trim_space()
	// println('!!! OLO REQ HEADERS WIN="$headers"')
	data := req.data
	// Retrieve default http user agent
	// char httpUseragent[512];
	// # char httpUseragent []= "";
	user_agent := ''
	// DWORD szhttpUserAgent = sizeof(httpUseragent);
	// ObtainUserAgentString(0, httpUseragent, &szhttpUserAgent);
	// # HINTERNET internet = InternetOpenA(httpUseragent, INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
	internet := C.InternetOpenA(user_agent.str, INTERNET_OPEN_TYPE_PRECONFIG, 0, 0, 0)
	// # if (!internet)
	if isnil(internet) {
		println('InternetOpen() failed')
		return emptyresp
	}
	// # INTERNET_PORT port = INTERNET_DEFAULT_HTTP_PORT;
	port := int(if is_ssl{INTERNET_DEFAULT_HTTPS_PORT} else { INTERNET_DEFAULT_HTTP_PORT})
	// if is_ssl {
	// # port = INTERNET_DEFAULT_HTTPS_PORT;
	// }
	connect := C.InternetConnectA(internet, host.str, port, 0, 0, INTERNET_SERVICE_HTTP, 0, 0)
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
	// # DWORD flags =
	#flags =
	# INTERNET_FLAG_HYPERLINK | INTERNET_FLAG_IGNORE_CERT_CN_INVALID |
	# INTERNET_FLAG_IGNORE_CERT_DATE_INVALID |
	# INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP |
	# INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS | INTERNET_FLAG_NO_AUTH |
	# INTERNET_FLAG_NO_CACHE_WRITE | INTERNET_FLAG_NO_UI |
	# INTERNET_FLAG_NO_COOKIES  |  // FUCK YOU MICROSOFT
	# INTERNET_FLAG_KEEP_CONNECTION |
	# INTERNET_FLAG_PRAGMA_NOCACHE | INTERNET_FLAG_RELOAD ;
	if is_ssl {
		#flags = flags | INTERNET_FLAG_SECURE;
	}
	request := C.HttpOpenRequest(connect, req.typ.str, path.str, 'HTTP/1.1', 0, 0, flags, 0)
	// request := C.InternetOpenUrl(connect, req.typ.str, path.str, 'HTTP/1.1', 0, 0, flags, 0)
	// # HINTERNET request = HttpOpenRequest(connect, req->typ.str, path.str, "HTTP/1.1",
	// # NULL, NULL, flags, NULL);
	// # if (!request)
	if isnil(request) {
		println('HttpOpenRequest() failed')
		return emptyresp
	}
	// println('LEN BEFORE SEND=$headers.len ; $headers')
	# bool ret =HttpSendRequest(request, headers.str, -1, data.str, data.len);
	# printf("RET=%d\n", ret);
	# int e = GetLastError();
	# printf("e=%d\n", e);
	// Get response headers
	// Todo call twice to get len
	# LPSTR h_buf = malloc(1024);
	# DWORD dwSize = 1024;
	// LPVOID lpOutBuffer=malloc(dwSize);
	# HttpQueryInfo(request, HTTP_QUERY_RAW_HEADERS_CRLF,
	# h_buf,&dwSize,NULL);
	# printf(" resp HEADERS %s\n", h_buf);
	// Get response  body
	// # const int BUF_MAX = 1024;
	// # TCHAR buf[BUF_MAX + 1];
	mut buf := [1025]byte
	mut nr_read := 0
	BUF_MAX := 1024
	// ok := C.InternetReadFile(request, buf, BUF_MAX, &nr_read)
	// # DWORD dwRead = 0;
	// /println('calling InternetReadFile()')
	// # bool ok = InternetReadFile(request, buf, BUF_MAX, &nr_read);
	// # if (!ok)
	// {
	// println('read not ok')
	// # int e = GetLastError();
	// # printf("%d\n", e);
	// }
	// # printf("dwread=%d\n", dwRead);
	// # while ((InternetReadFile(request, buf, BUF_MAX, &nr_read)) && nr_read > 0)
	for
	{
		println('111')
		ok := C.InternetReadFile(request, buf, BUF_MAX, &nr_read)
		println('222')
		if !ok {
			println('InternetReadFile() not ok ')
		}
		if ok && nr_read == 0 {
			println('ok && nr read == 0, breaking')
			C.printf('buf broken="%s"\n', buf)
			if req.url.contains('websocket') {
				println('win sleeping 2')
				time.sleep(2)
				continue
			}
			break
		}
		println('ireadfile()')
		buf[nr_read] = 0
		C.printf('buf="%s"\n', buf)
		s += tos2(buf)// TODO perf
		nr_read = 0
	}
	C.InternetCloseHandle(request)
	C.InternetCloseHandle(connect)
	C.InternetCloseHandle(internet)
	# resp_headers = tos2(h_buf);
	hh := resp_headers.split('\n')
	mut resp := Response {
		body: s
		headers: map[string]string{}
		// headers: resp_headers
	}
	// println('gen hh')
	for h in hh {
		// println('\n!')
		// println(h)
		vals := h.split(':')
		pos := h.index(':')
		if pos == -1 {
			continue
		}
		key := h.left(pos)
		val := h.right(pos + 1)
		// println('$key => $val')
		resp.headers[key] = val.trim_space()
	}
	println('END OF WIN req.do($req.url)')
	return resp
}

fn escape(s string) string {
	# DWORD size=1;
	# char *escaped = NULL;
	# char *empty_string = NULL;
	# HRESULT    res = UrlEscapeA(s.str, empty_string, &size, URL_ESCAPE_PERCENT | URL_ESCAPE_SEGMENT_ONLY);
	# if (res == E_POINTER)
	{
		# escaped = HeapAlloc(GetProcessHeap(), 0, size);
		# if (!escaped)
		# return s;
		# UrlEscapeA(s.str, escaped, &size, URL_ESCAPE_PERCENT | URL_ESCAPE_SEGMENT_ONLY);
		# return tos2(escaped);
	}
	return ''
}

fn C.InternetReadFile(voidptr, voidptr, int, intptr) bool

