// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import io
import net
import time
import net.mbedtls

const tls_accept_poll_timeout = 100 * time.millisecond

// tls_handshake_timeout is the default value for Server.tls_handshake_timeout,
// used as the fallback handshake budget when Server.accept_timeout is zero or
// net.infinite_timeout. The handshake now runs on a worker thread (the accept
// thread only does the raw TCP accept, polled at tls_accept_poll_timeout, so
// stop() is observed promptly regardless of handshakes); without a finite bound a
// client that completes the TCP connect and then stalls mid-handshake would tie
// up a worker indefinitely.
const tls_handshake_timeout = 30 * time.second

fn tls_accept_timeouts(accept_timeout time.Duration, handshake_fallback time.Duration) (time.Duration, time.Duration) {
	// A finite `accept_timeout` doubles as the handshake budget; when it is
	// zero or net.infinite_timeout (i64.max), fall back to `handshake_fallback`
	// so the handshake still times out and shutdown stays responsive.
	// net.infinite_timeout is positive (i64.max), so the > 0 check alone is
	// not enough — mbedtls's ssl_timeout_deadline treats it as an infinite
	// deadline exactly like 0 or negative values.
	is_finite := accept_timeout > 0 && accept_timeout != net.infinite_timeout
	handshake_timeout := if is_finite { accept_timeout } else { handshake_fallback }
	accept_poll_timeout := if is_finite && accept_timeout < tls_accept_poll_timeout {
		accept_timeout
	} else {
		tls_accept_poll_timeout
	}
	return accept_poll_timeout, handshake_timeout
}

// This file implements TLS termination for net.http.Server on top of the
// mbedtls SSL listener. It is gated to the default TLS backend; the matching
// `server_tls_d_use_openssl.v` provides a clear-error stub when the project is
// built with `-d use_openssl`.

// listen_and_serve_tls is the TLS counterpart of listen_and_serve. It is
// dispatched to by listen_and_serve when `s.cert` and `s.cert_key` are set.
fn (mut s Server) listen_and_serve_tls() {
	// Pick a default port that's distinct from the plain-HTTP default if the
	// user hasn't overridden it.
	addr := if s.addr == '' || s.addr == ':${default_server_port}' {
		':${default_https_server_port}'
	} else {
		s.addr
	}

	// When HTTP/2 is enabled, advertise ALPN `h2, http/1.1` on the listener.
	// Clients that select `h2` are dispatched to the HTTP/2 driver after the
	// handshake; clients that select `http/1.1` (or send no ALPN extension)
	// keep the existing HTTP/1.1 worker path.
	alpn := if s.enable_http2 { ['h2', 'http/1.1'] } else { []string{} }
	mut listener := mbedtls.new_ssl_listener(addr, mbedtls.SSLConnectConfig{
		cert:                   s.cert
		cert_key:               s.cert_key
		in_memory_verification: s.in_memory_verification
		validate:               false // accept any client; servers don't verify clients by default
		read_timeout:           s.read_timeout
		alpn_protocols:         alpn
	}) or {
		eprintln('Listening TLS on ${addr} failed, err: ${err}')
		return
	}
	defer {
		listener.shutdown() or {}
		if s.state == .stopped {
			s.state = .closed
			if s.on_closed != unsafe { nil } {
				s.on_closed(mut s)
			}
		}
	}
	s.addr = addr

	accept_poll_timeout, handshake_timeout := tls_accept_timeouts(s.accept_timeout,
		s.tls_handshake_timeout)
	ch := chan &mbedtls.SSLConn{cap: s.pool_channel_slots}
	mut idle_conns := &TlsIdleConnTracker{}
	mut ws := []thread{cap: s.worker_num}
	for wid in 0 .. s.worker_num {
		ws << new_tls_handler_worker(wid, ch, s.handler, s.max_keep_alive_requests,
			handshake_timeout, idle_conns)
	}

	if s.show_startup_message {
		println('Listening on https://${s.addr}/')
		flush_stdout()
	}

	time.sleep(20 * time.millisecond)
	s.state = .running
	if s.on_running != unsafe { nil } {
		s.on_running(mut s)
	}
	for s.state == .running {
		mut conn := listener.accept_raw_with_timeout(accept_poll_timeout) or {
			if s.state != .running {
				break
			}
			if err.code() == net.err_timed_out_code {
				continue
			}
			$if debug {
				eprintln('TLS accept failed: ${err}; skipping')
			}
			continue
		}
		// Hand the raw (not-yet-handshaked) conn to a worker. Don't use a bare
		// blocking `ch <- conn`: the accept loop, this send, and the post-loop
		// shutdown (ch.close + close_idle + ws.wait) all run on this one thread,
		// while stop()/close() only flip s.state from another thread. Under a slow-
		// handshake flood -- every worker blocked in complete_handshake and the
		// channel buffer full of untracked conns -- a blocking send would wedge the
		// accept thread so it never re-checks s.state nor reaches close_idle(),
		// delaying shutdown until a worker handshake times out. Poll s.state via a
		// select timeout so shutdown is still observed promptly.
		mut queued := false
		for s.state == .running && !queued {
			select {
				ch <- conn {
					queued = true
				}
				accept_poll_timeout {
					// channel full; loop re-checks s.state
				}
			}
		}
		if !queued {
			// Shutting down before this conn could be handed off. It was never
			// mark_idle'd, so close_idle() won't see it; close it here (exactly
			// once -- no worker ever received it) to avoid leaking the fd.
			conn.shutdown() or {}
		}
	}
	ch.close()
	idle_conns.close_idle()
	ws.wait()
}

// TlsHandlerWorker serves HTTP/1.1 requests on TLS-wrapped connections.
struct TlsHandlerWorker {
	id                      int
	ch                      chan &mbedtls.SSLConn
	max_keep_alive_requests int
	handshake_timeout       time.Duration
mut:
	idle_conns &TlsIdleConnTracker = unsafe { nil }
pub mut:
	handler Handler
}

fn new_tls_handler_worker(wid int, ch chan &mbedtls.SSLConn, handler Handler, max_keep_alive_requests int, handshake_timeout time.Duration, idle_conns &TlsIdleConnTracker) thread {
	mut w := &TlsHandlerWorker{
		id:                      wid
		ch:                      ch
		handler:                 handler
		max_keep_alive_requests: max_keep_alive_requests
		handshake_timeout:       handshake_timeout
		idle_conns:              idle_conns
	}
	return spawn w.process_requests()
}

fn (mut w TlsHandlerWorker) process_requests() {
	for {
		mut conn := <-w.ch or { break }
		w.handle_conn(mut conn)
	}
}

fn (mut w TlsHandlerWorker) handle_conn(mut conn mbedtls.SSLConn) {
	// For H2 connections, serve_h2_conn_with_idle_tracker's serve() owns the
	// mark_idle/unmark_idle lifetime (it marks idle once and unmarks in its
	// own defer). Calling unmark_idle here a second time would race: after
	// serve()'s defer fires the OS can recycle conn.handle for a new
	// connection that has already called mark_idle, and this stale unmark
	// would silently evict it, preventing close_idle from ever shutting it
	// down and leaking the reader goroutine.
	mut is_h2 := false
	defer {
		if !is_h2 {
			w.idle_conns.unmark_idle(conn.handle)
		}
		// close_idle may have already closed conn.handle on Windows (a forced
		// closesocket to wake this worker from a blocked read). conn.shutdown()
		// both frees the mbedtls TLS resources (SSL ctx, config, certs, RNG,
		// ALPN alloc) AND closes the socket; we must always do the former but
		// must not close the socket a second time — that would race process-wide
		// SOCKET reuse and could close an unrelated socket. So relinquish socket
		// ownership and still call shutdown for the TLS cleanup. On non-Windows
		// was_force_closed is always false and the worker remains the sole closer.
		if w.idle_conns.was_force_closed(conn.handle) {
			conn.owns_socket = false
		}
		conn.shutdown() or {}
	}
	// Run the TLS handshake here on the worker thread (the accept thread only does
	// the raw TCP accept), so handshakes proceed in parallel up to worker_num and a
	// client that stalls mid-handshake can't wedge the accept loop or delay stop().
	// mark_idle first so close_idle can interrupt a stalled handshake by force-
	// closing the fd, and so a conn handed off while the server is shutting down is
	// closed (mark_idle returns false) rather than handshaked. On success, unmark
	// before the serve path does its own idle marking, keeping each handle tracked
	// once. On any early return the defer above performs the single shutdown.
	if !w.idle_conns.mark_idle(conn.handle) {
		return
	}
	conn.complete_handshake(w.handshake_timeout) or {
		$if debug {
			eprintln('TLS handshake failed: ${err}')
		}
		return
	}
	w.idle_conns.unmark_idle(conn.handle)
	// If the TLS handshake negotiated HTTP/2 via ALPN, switch to the HTTP/2
	// driver; otherwise fall through to the existing HTTP/1.1 path unchanged.
	if conn.negotiated_alpn() == 'h2' {
		is_h2 = true
		serve_h2_conn_with_idle_tracker(mut conn, mut w.handler, w.idle_conns, conn.handle) or {
			$if debug {
				eprintln('h2 server error: ${err}')
			}
		}
		return
	}
	mut reader := io.new_buffered_reader(reader: conn)
	defer {
		unsafe {
			reader.free()
		}
	}

	mut request_count := 0
	for {
		if !w.idle_conns.mark_idle(conn.handle) {
			return
		}
		mut req := parse_request(mut reader) or {
			if err !is io.Eof {
				$if debug {
					eprintln('error parsing TLS request: ${err}')
				}
			}
			return
		}
		w.idle_conns.unmark_idle(conn.handle)
		request_count++
		// `conn.ip` is the peer's IPv4 address as populated by mbedtls'
		// accept(); blank for IPv6, which is acceptable for keep-alive logic.
		if conn.ip != '' {
			req.header.add_custom('Remote-Addr', conn.ip) or {}
		}

		mut resp := w.handler.handle(req)
		normalize_server_response(mut resp, req)

		if !resp.header.contains(.content_length) {
			resp.header.set(.content_length, '${resp.body.len}')
		}

		max_reached := w.max_keep_alive_requests > 0 && request_count >= w.max_keep_alive_requests
		req_conn := (req.header.get(.connection) or { '' }).to_lower()
		resp_conn := (resp.header.get(.connection) or { '' }).to_lower()
		keep_alive := if max_reached {
			false
		} else if resp_conn == 'close' {
			false
		} else if resp_conn == 'keep-alive' {
			true
		} else if req_conn == 'close' {
			false
		} else if req_conn == 'keep-alive' {
			true
		} else {
			req.version == .v1_1
		}
		if max_reached || !resp.header.contains(.connection) {
			if keep_alive {
				resp.header.set(.connection, 'keep-alive')
			} else {
				resp.header.set(.connection, 'close')
			}
		}

		conn.write(resp.bytes()) or {
			$if debug {
				eprintln('error sending TLS response: ${err}')
			}
			return
		}

		if !keep_alive {
			return
		}
	}
}
