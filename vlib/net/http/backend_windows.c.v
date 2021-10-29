// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

#flag windows -I @VEXEROOT/thirdparty/vschannel
#flag -l ws2_32 -l crypt32 -l secur32 -l user32
#include "vschannel.c"

fn C.new_tls_context() C.TlsContext

fn (mut req Request) ssl_do(port int, method Method, host_name string, path string) ?Response {
	if req.use_proxy == true {
		return error('not implemented')
	}

	mut ctx := C.new_tls_context()
	C.vschannel_init(&ctx)
	mut buff := unsafe { malloc_noscan(C.vsc_init_resp_buff_size) }
	addr := host_name
	sdata := req.build_request_headers(method, host_name, path)
	$if trace_http_request ? {
		eprintln('> $sdata')
	}

	if req.use_proxy == true {
		C.use_proxy = true
		C.proxy_fd = req.proxy.conn.fd
	}

	length := C.request(&ctx, port, addr.to_wide(), sdata.str, &buff)
	C.vschannel_cleanup(&ctx)

	response_text := unsafe { buff.vstring_with_len(length) }
	$if trace_http_response ? {
		eprintln('< $response_text')
	}

	return parse_response(response_text)
}

fn (mut proxy HttpProxy) create_ssl_tcp(hostname string, port int) ?ProxyConnLayer {
	mut ctx := C.new_tls_context()
	C.vschannel_init(&ctx)
	mut buff := unsafe { malloc_noscan(C.vsc_init_resp_buff_size) }

	if C.connect_to_server(&ctx, hostname, port) {
		return error('could not connect to host')
	}

	conn_fd := int(ctx.socket)

	C.vschannel_cleanup(&ctx)

	return error('not implemented')
}
