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
fn C.vschannel_alpn_supported() int

// vschannel_request_on_open mirrors C.request (declared in builtin/cfns.c.v) but
// runs over an already-open connection. See thirdparty/vschannel/vschannel.c.
fn C.vschannel_request_on_open(&C.TlsContext, &u8, u32, &&u8, fn (voidptr, isize) voidptr) i32

fn vschannel_ssl_do(req &Request, port int, method Method, host_name string, path string, data string, header Header) !Response {
	// When HTTP/2 is enabled (the default for https), advertise ALPN `h2` and,
	// if the server selects it, speak HTTP/2. Otherwise fall back to HTTP/1.1
	// over the same connection (see vschannel_h2_do). When HTTP/2 is opted out
	// of — or this Windows version's SChannel predates client-side ALPN
	// (pre-8.1), where injecting the ALPN buffer can fail the handshake
	// outright — use the original one-shot HTTP/1.1 path with no ALPN.
	if req.enable_http2 && C.vschannel_alpn_supported() != 0 {
		return vschannel_h2_do(req, port, method, host_name, path, data, header)!
	}
	return vschannel_h1_do(req, port, method, host_name, path, data, header)!
}

// vschannel_h1_do is the original one-shot HTTP/1.1 SChannel request path:
// connect, handshake, send the whole request, read the whole response,
// disconnect. It is used when HTTP/2 is disabled.
fn vschannel_h1_do(req &Request, port int, method Method, host_name string, path string, data string, header Header) !Response {
	mut ctx := C.new_tls_context()
	C.vschannel_use_tls12_client_protocol()
	C.vschannel_init(&ctx, C.BOOL(if req.validate { 1 } else { 0 }))
	mut buff := unsafe { malloc_noscan(C.vsc_init_resp_buff_size) }
	addr := host_name
	sdata := req.build_request_headers_with(method, host_name, port, path, data, header)
	$if trace_http_request ? {
		eprintln('> ${sdata}')
	}
	length := C.request(&ctx, port, addr.to_wide(), sdata.str, sdata.len, &buff, v_realloc)
	err_code := C.vschannel_last_error(&ctx)
	C.vschannel_cleanup(&ctx)
	return req.vschannel_finish_response(buff, length, err_code)!
}

// vschannel_h1_on_open runs the one-shot HTTP/1.1 request over a connection that
// vschannel_h2_connect() already opened, used as the fallback when the server
// did not negotiate `h2`. It consumes (and cleans up) `ctx`.
fn (req &Request) vschannel_h1_on_open(ctx &C.TlsContext, method Method, host_name string, port int, path string, data string, header Header) !Response {
	mut buff := unsafe { malloc_noscan(C.vsc_init_resp_buff_size) }
	sdata := req.build_request_headers_with(method, host_name, port, path, data, header)
	$if trace_http_request ? {
		eprintln('> ${sdata}')
	}
	length := C.vschannel_request_on_open(ctx, sdata.str, sdata.len, &buff, v_realloc)
	err_code := C.vschannel_last_error(ctx)
	C.vschannel_cleanup(ctx)
	return req.vschannel_finish_response(buff, length, err_code)!
}

// vschannel_finish_response turns the raw response buffer produced by the C
// request paths into a parsed Response, firing the progress/finish callbacks.
fn (req &Request) vschannel_finish_response(buff &u8, length int, err_code int) !Response {
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
