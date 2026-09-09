// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// ALPN (RFC 7301) support for the Windows SChannel backend. The handshake-level
// plumbing lives in thirdparty/vschannel/vschannel.c; this exposes it to V.
//
// This adds the *capability* to advertise ALPN and read the negotiated protocol
// on SChannel. The one-shot vschannel request() path still speaks HTTP/1.1, so
// it is not wired into fetch() yet (advertising `h2` without an HTTP/2 driver
// would let a server pick a protocol we cannot speak). Negotiation is exercised
// via schannel_alpn_probe(). Wiring the request path to HTTP/2 is the follow-up.
// See vlang/v#27383.

fn C.vschannel_set_alpn(tls_ctx &C.TlsContext, wire &char, len int)
fn C.vschannel_get_alpn(tls_ctx &C.TlsContext, out &char, out_cap int) int
fn C.vschannel_alpn_probe(tls_ctx &C.TlsContext, iport int, host &u16, out &char, out_cap int) int

// alpn_wire encodes ALPN protocol names into the wire format SChannel expects:
// each name preceded by a single length byte, e.g. `['h2', 'http/1.1']` becomes
// "\x02h2\x08http/1.1". Empty names, or names longer than 255 bytes, are skipped.
fn alpn_wire(protocols []string) []u8 {
	mut out := []u8{}
	for p in protocols {
		if p.len == 0 || p.len > 255 {
			continue
		}
		out << u8(p.len)
		out << p.bytes()
	}
	return out
}

// schannel_alpn_probe performs a TLS handshake to `host`:`port` advertising
// `protocols` via ALPN and returns the protocol the server selected (e.g. 'h2'),
// or '' if none was negotiated. It sends no application data, so it works as a
// pure ALPN-negotiation check independent of the HTTP/1.1 request path.
// Windows/SChannel only. Pass `validate` = false to skip certificate validation
// (e.g. against a local test server with a self-signed cert).
fn schannel_alpn_probe(host string, port int, protocols []string, validate bool) string {
	mut ctx := C.new_tls_context()
	C.vschannel_use_tls12_client_protocol()
	C.vschannel_init(&ctx, C.BOOL(if validate { 1 } else { 0 }))
	wire := alpn_wire(protocols)
	if wire.len > 0 {
		C.vschannel_set_alpn(&ctx, &char(wire.data), wire.len)
	}
	mut buf := []u8{len: 256}
	n := C.vschannel_alpn_probe(&ctx, port, host.to_wide(), &char(buf.data), buf.len)
	C.vschannel_cleanup(&ctx)
	if n <= 0 {
		return ''
	}
	return buf[..n].bytestr()
}
