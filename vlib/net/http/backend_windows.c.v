// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

#flag windows -I @VEXEROOT/thirdparty/vschannel
#flag -l ws2_32 -l crypt32 -l secur32 -l user32
#include "vschannel.c"

fn C.new_tls_context() C.TlsContext
fn C.connect_to_server(&C.TlsContext, &u16, int) int
fn C.get_tls_context_fd(&C.TlsContext) int

fn (mut req Request) ssl_do(port int, method Method, host_name string, path string) ?Response {
	mut ctx := C.new_tls_context()
	C.vschannel_init(&ctx)
	mut buff := unsafe { malloc_noscan(C.vsc_init_resp_buff_size) }
	addr := host_name
	sdata := req.build_request_headers(method, host_name, path)
	$if trace_http_request ? {
		eprintln('> $sdata')
	}

	if req.use_proxy == true {
		req.proxy.prepare(req, '$host_name:$port') ?
		C.use_proxy = 1
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

	conn_res := C.connect_to_server(&ctx, hostname.to_wide(), port)
	if conn_res != 0 {
		return error('could not connect to host')
	}

	connection_fd := C.get_tls_context_fd(&ctx)

	if connection_fd < 0 {
		return error('could not create fd from socket')
	}

	C.vschannel_cleanup(&ctx)

	return ProxyConnLayer{
		fd: connection_fd
	}
}
