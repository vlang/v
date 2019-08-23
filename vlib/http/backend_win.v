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

fn init_module() {}

fn ssl_do(port int, method, host_name, path string) Response {
	C.vschannel_init()
	// TODO: joe-c
	// dynamically increase in vschannel.c if needed
	mut buff := malloc(44000)
	addr := host_name
	req := build_request_headers('', method, host_name, path)
	length := int(C.request(port, addr.str, req.str, buff))

	C.vschannel_cleanup()
	return parse_response(string(buff, length))
}
