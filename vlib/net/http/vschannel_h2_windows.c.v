// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// HTTP/2 over the Windows SChannel backend. This wires the streaming transport
// primitives in thirdparty/vschannel/vschannel.c into the backend-agnostic
// HTTP/2 client (h2_conn.v / h2_client.v), mirroring what net_ssl_do does with
// net.ssl on the other platforms. See vlang/v#27383.

fn C.vschannel_h2_connect(tls_ctx &C.TlsContext, iport int, host &u16) int
fn C.vschannel_write(tls_ctx &C.TlsContext, buf &char, len int) int
fn C.vschannel_read(tls_ctx &C.TlsContext, buf &char, cap int) int
fn C.vschannel_h2_close(tls_ctx &C.TlsContext)

// VSchannelH2Transport adapts the SChannel streaming C API (vschannel_read /
// vschannel_write over an open TLS connection) to the H2Transport interface
// that H2Conn drives. It borrows a &C.TlsContext owned by the caller, which
// must outlive the transport.
struct VSchannelH2Transport {
mut:
	ctx &C.TlsContext = unsafe { nil }
}

// read fills `buf` with up to buf.len decrypted application bytes, returning the
// number read (0 at end of stream). H2Conn treats a 0/closed read as the
// connection closing, matching net.ssl semantics.
fn (mut t VSchannelH2Transport) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	n := C.vschannel_read(t.ctx, &char(buf.data), buf.len)
	if n < 0 {
		return error('http: vschannel_read failed')
	}
	return n
}

// write encrypts and sends all of `buf`, returning the number of bytes consumed.
fn (mut t VSchannelH2Transport) write(buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	n := C.vschannel_write(t.ctx, &char(buf.data), buf.len)
	if n < 0 {
		return error('http: vschannel_write failed')
	}
	return n
}

// vschannel_h2_do opens an SChannel TLS connection advertising ALPN
// `h2`/`http/1.1`. If the server selects `h2`, it runs the request over HTTP/2.
// Otherwise it speaks HTTP/1.1 over the *same* open connection (so a server
// that does not do HTTP/2 — or ALPN at all — costs no extra handshake, and
// single-connection servers are not broken by a probe-then-reconnect). Any
// error once HTTP/2 is in use propagates as-is.
fn vschannel_h2_do(req &Request, port int, method Method, host_name string, path string, data string, header Header) !Response {
	mut ctx := C.new_tls_context()
	C.vschannel_use_tls12_client_protocol()
	C.vschannel_init(&ctx, C.BOOL(if req.validate { 1 } else { 0 }))
	wire := alpn_wire(['h2', 'http/1.1'])
	C.vschannel_set_alpn(&ctx, &char(wire.data), wire.len)

	if C.vschannel_h2_connect(&ctx, port, host_name.to_wide()) != 0 {
		err_code := C.vschannel_last_error(&ctx)
		C.vschannel_cleanup(&ctx)
		if err_code != 0 {
			return vschannel_request_error(err_code)
		}
		return error('http: vschannel connect failed')
	}

	mut abuf := []u8{len: 16}
	an := C.vschannel_get_alpn(&ctx, &char(abuf.data), abuf.len)
	if an > 0 && abuf[..an].bytestr() == 'h2' {
		defer {
			C.vschannel_h2_close(&ctx)
		}
		mut transport := &VSchannelH2Transport{
			ctx: &ctx
		}
		mut conn := new_h2_conn(transport)
		return req.h2_exchange(mut conn, method, host_name, port, path, data, header)!
	}

	// Server chose HTTP/1.1 (or no ALPN): reuse the open connection for h1.
	return req.vschannel_h1_on_open(&ctx, method, host_name, port, path, data, header)!
}
