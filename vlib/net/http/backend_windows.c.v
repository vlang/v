// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

#flag windows -I @VEXEROOT/thirdparty/vschannel
#flag -l ws2_32 -l crypt32 -l secur32 -l user32
#include "vschannel.c"

fn C.new_tls_context() C.TlsContext
fn C.connect_to_server(&C.TlsContext, &u16, int) int

type FnVsChannelReadCallback = fn (channel &C.VsChannelConn, buffer &byte, len int) int

type FnVsChannelWriteCallback = fn (channel &C.VsChannelConn, const_data &byte, len int) int

type FnVsChannelFreeCallback = fn (channel &C.VsChannelConn)

struct C.VsChannelConn {
	data_ptr voidptr
	read     FnVsChannelReadCallback
	write    FnVsChannelWriteCallback
	free     FnVsChannelFreeCallback
}

/*
* SslConnLayer
*/
struct SslConnLayer {
	vschan &C.VsChannelConn
}

fn (mut l SslConnLayer) write(buf []byte) ?int {
	buf_ptr = &char(buf[0])
	return l.ctx.conn_layer.write(buf_ptr, buf.len)
}

fn (mut l SslConnLayer) read(mut bytes []byte) ?int {
	buf_ptr = &char(bytes[0])
	return l.ctx.conn_layer.read(buf_ptr, bytes.len)
}

fn (mut l SslConnLayer) close() ? {
	C.vschannel_cleanup(l.vschan.ctx)
}

// noops
fn (mut l SslConnLayer) set_read_timeout(t time.Duration) {}

fn (mut l SslConnLayer) set_write_timeout(t time.Duration) {}

// helpers

fn vschannel_set_proxy(ctx &C.TlsContext, mut proxy ProxyConnLayer) {
	// TODO: closures are not implemented on windows yet.
	eprintln('windows vschannel http proxy support is NOT implemented')
	/*
	fn chan_read := fn [mut proxy] (_ &C.VsChannelConn, buf &char, len int) {
        vbuf := buf.vbytes(len)
        return proxy.read(vbuf)
    }
    fn chan_write := fn [mut proxy] (_ &C.VsChannelConn, buf &char, len int) {
        vbuf := buf.vbytes(len)
        return proxy.write(vbuf)
    }
    fn chan_free := fn (_ &C.VsChannelConn) {}
    C.vschannel_set_proxy(ctx, 0, chan_read, chan_write, chan_free)
	*/
}

/*
* Request specific methods
*/
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

		vschannel_set_proxy(&ctx, req.proxy.conn)
	}

	length := C.request(&ctx, port, addr.to_wide(), sdata.str, &buff)
	C.vschannel_cleanup(&ctx)

	response_text := unsafe { buff.vstring_with_len(length) }
	$if trace_http_response ? {
		eprintln('< $response_text')
	}

	return parse_response(response_text)
}

/*
* HttpProxy specific methods
*/
fn (mut proxy HttpProxy) create_ssl_layer(hostname string, port int) ?ProxyConnLayer {
	mut ctx := C.new_tls_context()
	C.vschannel_init(&ctx)

	conn_res := C.connect_to_server(&ctx, hostname.to_wide(), port)
	if conn_res != 0 {
		return error('could not connect to host')
	}

	mut vschannel := C.vschannel_get_conn(&ctx)

	return SslConnLayer{
		vschan: vschannel
	}
}
