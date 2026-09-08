// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import io
import net

// h2c_preface_prefix is the leading bytes of the HTTP/2 client connection
// preface (h2_client_preface, h2_conn.v). RFC 9113 §3.4 chose "PRI" as the
// pseudo-method specifically because it is not a registered HTTP/1.1 method,
// so a plain-TCP listener can dispatch on it unambiguously before parsing
// anything as HTTP/1.1. This is the only h2c bootstrap mechanism implemented:
// the HTTP/1.1 `Upgrade: h2c` mechanism (RFC 7540 §3.2) was dropped entirely
// from RFC 9113 and is not supported here.
const h2c_preface_prefix = h2_client_preface[..4]

// H2cTransport adapts a buffered HTTP/1.1 connection reader to the
// H2Transport interface for prior-knowledge cleartext HTTP/2 (RFC 9113 §3.4).
// Reads MUST go through `reader`, never `conn` directly: peek() may already
// have pulled more than the peeked bytes into the reader's internal buffer,
// and reading from `conn` directly would silently lose them.
struct H2cTransport {
mut:
	reader &io.BufferedReader
	conn   &net.TcpConn
}

fn (mut t H2cTransport) read(mut buf []u8) !int {
	return t.reader.read(mut buf)
}

fn (mut t H2cTransport) write(buf []u8) !int {
	return t.conn.write(buf)
}

// try_serve_h2c peeks the first bytes of a fresh connection; if they match the
// start of the HTTP/2 client preface, the connection is handed off to the
// HTTP/2 server driver for prior-knowledge h2c (RFC 9113 §3.4) and this
// returns true. Returns false — a no-op, since peek() never consumes bytes —
// when the connection should be parsed as HTTP/1.1 instead, including when
// the peek itself fails (e.g. the client connected and sent nothing yet): the
// existing HTTP/1.1 path already handles that case the same way it always has.
fn try_serve_h2c(mut reader io.BufferedReader, mut conn net.TcpConn, mut handler Handler) bool {
	peeked := reader.peek(h2c_preface_prefix.len) or { return false }
	if peeked.bytestr() != h2c_preface_prefix {
		return false
	}
	mut transport := H2Transport(H2cTransport{
		reader: reader
		conn:   conn
	})
	serve_h2_conn(mut transport, mut handler) or {}
	return true
}
