// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

import strings 

#flag windows -I @VROOT/thirdparty/vschannel
#flag -lws2_32 -lcrypt32
 
#include "vschannel.c"


fn init_module() {}

fn ssl_do(method, host_name, path string) string { 
	mut buff := malloc(10000)
	
	req := '$method $path HTTP/1.0\r\nUser-Agent: v\r\nAccept:*/*\r\n\r\n'
	length := int(C.request(host_name.str, req.str, buff))
	
	if length == 0 {
		return ''
	}

	resp := tos(buff, length)
	
	return resp
}
