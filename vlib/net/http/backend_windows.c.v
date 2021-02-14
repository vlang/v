// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

#flag windows -I @VROOT/thirdparty/vschannel
#flag -l ws2_32 -l crypt32 -l secur32 -l user32
#include "vschannel.c"
fn C.new_tls_context() C.TlsContext

fn (req &Request) ssl_do(port int, method Method, host_name string, path string) ?Response {
	mut ctx := C.new_tls_context()
	C.vschannel_init(&ctx)
	mut buff := unsafe {malloc(C.vsc_init_resp_buff_size)}
	addr := host_name
	sdata := req.build_request_headers(method, host_name, path)
	length := int(C.request(&ctx, port, addr.to_wide(), sdata.str, &buff))
	C.vschannel_cleanup(&ctx)
	return parse_response(unsafe {buff.vstring_with_len(length)})
}
