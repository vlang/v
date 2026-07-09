// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import sync
import time

// VSchannelPooledTransport adapts a pooled Windows SChannel connection into
// the H2Transport interface (h2_conn.v), mirroring H2PooledTransport
// (h2_pooled_transport.v) for the mbedTLS/OpenSSL backends. SSPI's
// DecryptMessage/EncryptMessage have no documented thread-safety guarantee
// for concurrent use on one CtxtHandle, so this is exactly as conservative as
// H2PooledTransport: every vschannel_read/vschannel_write/vschannel_h2_close
// call is serialized behind one io_mu.
//
// `ctx` is embedded BY VALUE, not by pointer: unlike mbedTLS's SSL context
// (which wires an internal self-pointer via mbedtls_ssl_set_bio(&s.ssl, ...)
// at dial time, making a struct copy corrupt it), TlsContext contains only
// Windows handle types (SOCKET, CredHandle, CtxtHandle) plus independently
// heap-allocated buffers (recv_buf/plain_buf/send_buf) that are not
// self-referential to the struct's own address. Because VSchannelPooledTransport
// is always heap-allocated (@[heap]) and used only through a pointer receiver,
// &t.ctx is stable for the object's whole lifetime, matching every other
// self-referential-struct precedent in this file family (H2PooledTransport,
// H2MuxConn, H2DialCall) without needing a second heap allocation.
@[heap]
struct VSchannelPooledTransport {
mut:
	ctx    C.TlsContext
	io_mu  &sync.Mutex = sync.new_mutex()
	closed bool
}

fn C.vschannel_set_io_timeouts(tls_ctx &C.TlsContext, recv_timeout_ms u32, send_timeout_ms u32)
fn C.vschannel_wait_writable(tls_ctx &C.TlsContext, timeout_ms int) int

// new_vschannel_pooled_transport dials host:port over SChannel, advertising
// ALPN h2/http/1.1, and wraps the open connection for pooled H2MuxConn use.
// Mirrors vschannel_h2_do's connect sequence (vschannel_h2_windows.c.v), but
// leaves ALPN-selection branching to the caller (h2_dial_probe_vschannel)
// instead of driving the h2-vs-h1 fork itself, since the caller also needs
// the h1 fallback to go through the one-shot path, not this adapter.
fn new_vschannel_pooled_transport(req &Request, host string, port int) !&VSchannelPooledTransport {
	mut t := &VSchannelPooledTransport{
		ctx: C.new_tls_context()
	}
	C.vschannel_use_tls12_client_protocol()
	C.vschannel_init(&t.ctx, C.BOOL(if req.validate { 1 } else { 0 }))
	wire := alpn_wire(['h2', 'http/1.1'])
	C.vschannel_set_alpn(&t.ctx, &char(wire.data), wire.len)
	if C.vschannel_h2_connect(&t.ctx, port, host.to_wide()) != 0 {
		err_code := C.vschannel_last_error(&t.ctx)
		C.vschannel_cleanup(&t.ctx)
		if err_code != 0 {
			return vschannel_request_error(err_code)
		}
		return error('http: vschannel connect failed')
	}
	// Unlike mbedTLS/OpenSSL (one shared duration field reused for both
	// directions' internal retry loop, needing widen-then-restore around
	// every write -- see h2_pooled_transport.v), SO_RCVTIMEO/SO_SNDTIMEO are
	// independent Winsock options: set once here, never touched again.
	C.vschannel_set_io_timeouts(&t.ctx, u32(h2_pooled_io_timeout / time.millisecond),
		u32(h2_pooled_write_stall_limit / time.millisecond))
	return t
}

// negotiated_alpn returns the protocol SChannel selected during the
// handshake (e.g. 'h2'), or '' if none was negotiated.
fn (t &VSchannelPooledTransport) negotiated_alpn() string {
	mut abuf := []u8{len: 16}
	an := C.vschannel_get_alpn(&t.ctx, &char(abuf.data), abuf.len)
	if an <= 0 {
		return ''
	}
	return abuf[..an].bytestr()
}

// read implements H2Transport. Called only by H2MuxConn's single background
// reader thread (see read_loop in h2_mux_conn.v).
//
// Deliberately has NO outer readiness poll before the wrapped vschannel_read
// call, for the same reason as H2PooledTransport.read(): vschannel_read
// itself serves already-decrypted plaintext (plain_buf) and already-received
// buffered ciphertext (recv_buf) before ever touching the socket, so an outer
// select() could strand data that is already available. vschannel_read's own
// bounded recv() (SO_RCVTIMEO, set once at construction) is what keeps this
// call from blocking longer than h2_pooled_io_timeout when nothing is
// buffered.
fn (mut t VSchannelPooledTransport) read(mut buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	t.io_mu.lock()
	if t.closed {
		t.io_mu.unlock()
		return error('vschannel pooled transport: closed')
	}
	n := C.vschannel_read(&t.ctx, &char(buf.data), buf.len)
	t.io_mu.unlock()
	if n < 0 {
		if C.vschannel_last_error(&t.ctx) == int(net.WsaError.wsaetimedout) {
			return error('http: vschannel read timed out')
		}
		return error('http: vschannel_read failed')
	}
	return n
}

// write implements H2Transport. H2MuxConn serializes all its writers through
// its own wmu (see write_all_locked in h2_mux_conn.v), so at most one caller
// ever reaches this method at a time; io_mu here exists to mutually exclude
// that writer against the reader and against close().
//
// The outer, unlocked vschannel_wait_writable poll mirrors
// H2PooledTransport.write()'s own outer wait_for_write: nothing has entered
// the TLS stack yet, so waiting here is provably safe and keeps a slow
// upload from holding io_mu (and starving the reader) for the whole write
// stall budget.
fn (mut t VSchannelPooledTransport) write(buf []u8) !int {
	if buf.len == 0 {
		return 0
	}
	deadline := time.now().add(h2_pooled_write_stall_limit)
	for {
		ready := C.vschannel_wait_writable(&t.ctx, int(h2_pooled_poll_interval / time.millisecond))
		if ready < 0 {
			return error('http: vschannel_wait_writable failed')
		}
		if ready == 1 {
			break
		}
		t.io_mu.lock()
		is_closed := t.closed
		t.io_mu.unlock()
		if is_closed {
			return error('vschannel pooled transport: closed')
		}
		if time.now() >= deadline {
			return error('vschannel pooled transport: write stalled for ${h2_pooled_write_stall_limit}')
		}
	}
	t.io_mu.lock()
	if t.closed {
		t.io_mu.unlock()
		return error('vschannel pooled transport: closed')
	}
	n := C.vschannel_write(&t.ctx, &char(buf.data), buf.len)
	t.io_mu.unlock()
	if n < 0 {
		return error('http: vschannel_write failed')
	}
	return n
}

// close tears the underlying SChannel connection down, holding io_mu across
// the call so it cannot run concurrently with a locked read()/write() (which
// would free the TLS context out from under it). A second call finds
// `closed` already set and returns immediately: vschannel_h2_close is safe to
// call more than once (it guards its own teardown on context_initialized),
// but this flag avoids relying on that and matches H2PooledTransport.close()'s
// own idempotency discipline.
fn (mut t VSchannelPooledTransport) close() {
	t.io_mu.lock()
	if t.closed {
		t.io_mu.unlock()
		return
	}
	t.closed = true
	C.vschannel_h2_close(&t.ctx)
	t.io_mu.unlock()
}

// h2_dial_probe_vschannel is the Windows-native counterpart of
// h2_dial_probe_ssl (transport_h2.v): dials host:port over SChannel and
// reports whether ALPN selected h2. Unlike the mbedTLS/OpenSSL probe, a
// non-h2 result closes its own connection before returning rather than
// handing it back for h1 reuse -- h1-over-vschannel pooling is out of scope,
// so the caller falls back to the one-shot req.ssl_do instead (see
// h2_fallback_h1).
fn h2_dial_probe_vschannel(req &Request, host string, port int) !H2ProbeResult {
	mut t := new_vschannel_pooled_transport(req, host, port)!
	if t.negotiated_alpn() != 'h2' {
		t.close()
		return H2ProbeResult{
			is_h2: false
		}
	}
	return H2ProbeResult{
		is_h2:     true
		transport: H2Transport(t)
		closer:    H2TransportCloser(t)
	}
}
