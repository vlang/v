// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

import strings
import net.urllib

#flag windows -I @VROOT/thirdparty/vschannel
#flag -l ws2_32
#flag -l crypt32
 
#include "vschannel.c"

const (
	max_redirects = 4
)

fn init_module() {}

fn ssl_do(method, host_name, path string) Response { 
	C.vschannel_init()
	// TODO: joe-c
	// dynamically increase in vschannel if needed
	mut buff := malloc(80000)
	
	mut p := if path == '' { '/' } else { path }
	mut req := build_request_headers('', method, host_name, p)
	mut length := int(C.request(host_name.str, req.str, buff))

	mut resp := parse_response(tos(buff, length))
	mut no_redirects := 0

	for resp.status_code == 301 && no_redirects <= max_redirects {
		u := urllib.parse(resp.headers['Location']) or { break }
		p = if u.path == '' { '/' } else { u.path }
		req = build_request_headers('', method, u.hostname(), p)
		length = int(C.request(u.hostname().str, req.str, buff))
		resp = parse_response(tos(buff, length))
		no_redirects++
	}
	free(buff)
	C.vschannel_cleanup()
	return resp
}
