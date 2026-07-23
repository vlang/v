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
	// write_stall_limit is how long write() waits for the socket to become
	// writable before failing the connection — set alongside the SO_RCVTIMEO/
	// SO_SNDTIMEO pair by set_timeouts(), so an h1-pool caller (a per-request
	// req.write_timeout, no multiplexed reader to keep responsive) and an
	// h2-pool caller (the fixed h2_pooled_write_stall_limit budget) each get
	// the value appropriate to how the connection is being used.
	write_stall_limit time.Duration = h2_pooled_write_stall_limit
}

fn C.vschannel_set_io_timeouts(tls_ctx &C.TlsContext, recv_timeout_ms u32, send_timeout_ms u32)
fn C.vschannel_wait_writable(tls_ctx &C.TlsContext, timeout_ms int) int

// new_vschannel_pooled_transport dials host:port over SChannel, advertising
// `alpn_protocols` (skipped entirely when empty, matching the original
// one-shot vschannel_h1_do's no-ALPN dial), and wraps the open connection for
// pooled use — either as an H2Transport (H2MuxConn, when ALPN selects h2) or
// as an H1StreamConn (H1PooledConn, plain h1 pooling — transport.v). Mirrors
// vschannel_h2_do's connect sequence (vschannel_h2_windows.c.v), but leaves
// ALPN-selection branching to the caller instead of driving the h2-vs-h1 fork
// itself, since different callers do different things with a non-h2 result
// (h2_dial_probe_vschannel hands the connection back for h1 reuse;
// vschannel_fresh_round_trip never advertises h2 at all when the request
// doesn't want it).
fn new_vschannel_pooled_transport(host string, port int, validate bool, alpn_protocols []string, read_timeout time.Duration, write_timeout time.Duration) !&VSchannelPooledTransport {
	mut t := &VSchannelPooledTransport{
		ctx: C.new_tls_context()
	}
	C.vschannel_use_tls12_client_protocol()
	C.vschannel_init(&t.ctx, C.BOOL(if validate { 1 } else { 0 }))
	if alpn_protocols.len > 0 {
		wire := alpn_wire(alpn_protocols)
		C.vschannel_set_alpn(&t.ctx, &char(wire.data), wire.len)
	}
	if C.vschannel_h2_connect(&t.ctx, port, host.to_wide()) != 0 {
		err_code := C.vschannel_last_error(&t.ctx)
		C.vschannel_cleanup(&t.ctx)
		if err_code != 0 {
			return vschannel_request_error(err_code)
		}
		return error('http: vschannel connect failed')
	}
	t.set_timeouts(read_timeout, write_timeout)
	return t
}

// set_timeouts (re)applies the pooled connection's socket-level read/write
// timeouts and write()'s stall budget together. Unlike mbedTLS/OpenSSL (one
// shared duration field reused for both directions' internal retry loop,
// needing widen-then-restore around every write -- see h2_pooled_transport.v),
// SO_RCVTIMEO/SO_SNDTIMEO are independent Winsock options that stay in effect
// until changed again. Called once at dial time with the caller's intended
// timeouts, and again by h2_dial_probe_vschannel when repurposing a probe
// connection dialed for h2 (short poll reads, long write stall) for h1
// pooling (a per-request read_timeout, no multiplexed reader to keep
// responsive) instead of dialing a second time.
fn (mut t VSchannelPooledTransport) set_timeouts(read_timeout time.Duration, write_timeout time.Duration) {
	t.write_stall_limit = write_timeout
	C.vschannel_set_io_timeouts(&t.ctx, u32(read_timeout / time.millisecond),
		u32(write_timeout / time.millisecond))
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
	deadline := time.now().add(t.write_stall_limit)
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
			return error('vschannel pooled transport: write stalled for ${t.write_stall_limit}')
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
// reports whether ALPN selected h2. A non-h2 result hands the still-open
// connection back via vsc_probe for h1 reuse, mirroring h2_dial_probe_ssl's
// own ssl_probe field (#27879 -- h1-over-vschannel pooling).
fn h2_dial_probe_vschannel(req &Request, host string, port int) !H2ProbeResult {
	if C.vschannel_alpn_supported() == 0 {
		// Pre-Windows 8.1 / Server 2012 R2 SChannel has no client-side ALPN:
		// injecting the SECBUFFER_APPLICATION_PROTOCOLS buffer that
		// new_vschannel_pooled_transport always installs can fail the whole
		// handshake outright, so h2 is unreachable on those systems. Report
		// the origin as h1-only WITHOUT dialing (no probe connection to hand
		// back either): the caller memoizes key_proto[key] = 1 and completes
		// this (and every later) request via h2_fallback_h1, which dials a
		// fresh h1-only vschannel connection through vschannel_fresh_round_trip
		// -- itself gated on the same vschannel_alpn_supported() check, so it
		// never re-attempts ALPN on these systems either (Codex P1, vlang/v#27712
		// pullrequestreview-4729311285).
		return H2ProbeResult{
			is_h2: false
		}
	}
	mut t := new_vschannel_pooled_transport(host, port, req.validate, ['h2', 'http/1.1'],
		h2_pooled_io_timeout, h2_pooled_write_stall_limit)!
	if t.negotiated_alpn() != 'h2' {
		// Hand the still-open, ALPN-probed connection back for h1 reuse
		// instead of closing it: widen its timeouts from the h2-poll-tuned
		// values (short reads, tuned for a multiplexed background reader) to
		// this request's real read/write timeouts first, matching
		// H1PooledConn.refresh_timeouts' own per-checkout behavior -- an h1
		// pooled connection has no multiplexed reader to keep responsive.
		t.set_timeouts(req.read_timeout, req.write_timeout)
		return H2ProbeResult{
			is_h2:     false
			vsc_probe: H1StreamConn(t)
		}
	}
	return H2ProbeResult{
		is_h2:     true
		transport: H2Transport(t)
		closer:    H2TransportCloser(t)
	}
}

// vschannel_fresh_round_trip is the native-Windows counterpart of
// tls_fresh_round_trip (transport.v): dials a fresh pooled SChannel
// connection and completes the request on it. Mirrors tls_fresh_round_trip's
// ALPN selection exactly -- advertise h2/http1.1 when the request wants h2,
// run the request on the existing one-shot HTTP/2 driver if the server
// selects it (unpooled, like the mbedTLS/OpenSSL sibling), otherwise pool the
// connection as an ordinary h1 keep-alive entry -- except ALPN is
// additionally gated on vschannel_alpn_supported(), matching
// vschannel_ssl_do/h2_dial_probe_vschannel's own guard: pre-Windows-8.1
// SChannel has no client-side ALPN, and injecting the ALPN buffer there can
// fail the handshake outright.
fn (mut t Transport) vschannel_fresh_round_trip(req &Request, key string, raw string, method Method, host string, port int, path string, data string, header Header) !Response {
	alpn := if req.enable_http2 && C.vschannel_alpn_supported() != 0 {
		['h2', 'http/1.1']
	} else {
		[]string{}
	}
	mut vt := new_vschannel_pooled_transport(host, port, req.validate, alpn, req.read_timeout,
		req.write_timeout)!
	if req.enable_http2 && vt.negotiated_alpn() == 'h2' {
		defer {
			vt.close()
		}
		mut conn := new_h2_conn(vt)
		return req.h2_exchange(mut conn, method, host, port, path, data, header)!
	}
	mut conn := &H1PooledConn{
		key: key
		vsc: H1StreamConn(vt)
	}
	resp, reusable := conn.exchange(req, raw) or {
		conn.close_conn()
		// Past the TLS handshake the request bytes may have been (partially)
		// written; a non-idempotent method must not be replayed by the outer
		// retry loop. (A dial/handshake failure above propagates before this
		// and stays retryable, since no request byte was sent.)
		if !transport_is_idempotent(method) {
			return error_with_code(err.msg(), transport_err_unsafe_retry)
		}
		return err
	}
	t.maybe_checkin(mut conn, header, reusable, resp)
	return resp
}
