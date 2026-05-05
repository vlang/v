// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

#flag windows -I @VEXEROOT/thirdparty/vschannel
#flag -l ws2_32 -l crypt32 -l secur32 -l user32
#include "vschannel.c"
// Win7 needs TLS 1.2 selected before Schannel credentials are acquired.
#define vschannel_use_tls12_client_protocol() (protocol = SP_PROT_TLS1_2_CLIENT)

pub struct C.TlsContext {}

const C.vsc_init_resp_buff_size int

fn C.new_tls_context() C.TlsContext
fn C.vschannel_use_tls12_client_protocol()
fn C.vschannel_init(tls_ctx &C.TlsContext, validate_server_certificate C.BOOL)
fn C.vschannel_last_error(tls_ctx &C.TlsContext) int

fn vschannel_ssl_do(req &Request, port int, method Method, host_name string, path string, effective_data string) !Response {
	mut ctx := C.new_tls_context()
	C.vschannel_use_tls12_client_protocol()
	C.vschannel_init(&ctx, C.BOOL(if req.validate { 1 } else { 0 }))
	mut buff := unsafe { malloc_noscan(C.vsc_init_resp_buff_size) }
	addr := host_name
	sdata := req.build_request_headers(method, host_name, port, path, effective_data)
	$if trace_http_request ? {
		eprintln('> ${sdata}')
	}
	length := C.request(&ctx, port, addr.to_wide(), sdata.str, sdata.len, &buff, v_realloc)
	err_code := C.vschannel_last_error(&ctx)
	C.vschannel_cleanup(&ctx)
	if length <= 0 {
		if err_code != 0 {
			return vschannel_request_error(err_code)
		}
		return error('http: vschannel request failed')
	}
	response_text := unsafe { buff.vstring_with_len(length) }
	if req.on_progress != unsafe { nil } {
		req.on_progress(req, unsafe { buff.vbytes(length) }, u64(length))!
	}
	$if trace_http_response ? {
		eprintln('< ${response_text}')
	}
	if req.on_finish != unsafe { nil } {
		req.on_finish(req, u64(response_text.len))!
	}
	return vschannel_parse_response(response_text, err_code)
}
