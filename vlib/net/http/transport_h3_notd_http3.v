// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import sync
import time

// This file is the `-d http3`-OFF counterpart of transport_h3_d_http3.v (and
// the h3_*_d_http3.v files it needs). HTTP/3 is opt-in precisely because its
// implementation depends on net.quic, whose TLS 1.3 stack pulls in
// crypto.ecdsa and therefore hard-requires the OpenSSL development headers
// (<openssl/ecdsa.h>). Importing net.quic unconditionally from net.http made
// EVERY net.http user -- veb, the http client, and everything transitively
// built on them -- fail to compile on a machine without OpenSSL installed,
// even for plain HTTP that never touches QUIC. Gating the net.quic import
// behind `-d http3` keeps the common case dependency-free; a program that
// actually wants HTTP/3 opts in with `-d http3` (and must then have the
// OpenSSL headers available).
//
// Everything below is the minimal surface transport.v references so it keeps
// compiling with h3 disabled: the pool-state types (empty here, never
// populated because h3_round_trip fails before any dial) and the request
// entry point (a clear error instead of a QUIC request). The real versions,
// with identical names/signatures, live in the _d_http3.v files.

// H3MuxConn mirrors the real H3MuxConn's shape only as far as transport.v
// touches it: Transport.h3_conns holds &H3MuxConn, and close_idle/checkin/
// evict_oldest_idle_locked read these fields and call these methods. The map
// stays empty without `-d http3` (h3_round_trip errors before anything is
// pooled), so none of this runs -- it exists purely so transport.v type-checks.
@[heap]
pub struct H3MuxConn {
mut:
	qmu            &sync.Mutex = sync.new_mutex()
	active_streams int
	idle_since     time.Time
}

// shutdown_when_idle is a no-op stub; see H3MuxConn above.
pub fn (mut c H3MuxConn) shutdown_when_idle() {}

// release is a no-op stub; see H3MuxConn above.
pub fn (mut c H3MuxConn) release() {}

// H3DialCall is the singleflight-dial handle; only its type is referenced by
// Transport.h3_dialing when h3 is disabled, never its (absent) fields.
struct H3DialCall {}

// h3_round_trip is the entry point round_trip() calls for an
// `enable_http3: true` request. Without `-d http3` there is no QUIC stack to
// run it on, so fail with an actionable message instead of silently falling
// back to HTTP/1.1/2 (enable_http3 is a hard, explicit opt-in -- see its own
// doc comment in request.v).
fn (mut t Transport) h3_round_trip(_req &Request, _key string, _method Method, _host string, _port int, _path string, _data string, _header Header) !Response {
	return error('http: HTTP/3 (enable_http3) support is not compiled in; rebuild with `-d http3` (requires the OpenSSL development headers)')
}
