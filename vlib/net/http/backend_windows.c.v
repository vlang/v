// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

$if gcboehm ? {
	#define VSCHANNEL_REALLOC GC_REALLOC
}

#flag windows -I @VEXEROOT/thirdparty/vschannel
#flag -l ws2_32 -l crypt32 -l secur32 -l user32
#include "vschannel.c"

fn C.new_tls_context() C.TlsContext

fn (req &Request) ssl_do(port int, method Method, host_name string, path string) !Response {
	mut ctx := C.new_tls_context()
	C.vschannel_init(&ctx)
	mut buff := unsafe { malloc_noscan(C.vsc_init_resp_buff_size) }
	addr := host_name
	sdata := req.build_request_headers(method, host_name, path)
	$if trace_http_request ? {
		eprintln('> ${sdata}')
	}
	length := C.request(&ctx, port, addr.to_wide(), sdata.str, &buff)
	C.vschannel_cleanup(&ctx)
	response_text := unsafe { buff.vstring_with_len(length) }
	$if trace_http_response ? {
		eprintln('< ${response_text}')
	}
	return parse_response(response_text)
}
