// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import io
import net
import time
import net.mbedtls

const tls_accept_poll_timeout = 100 * time.millisecond

fn tls_accept_timeouts(accept_timeout time.Duration) (time.Duration, time.Duration) {
	handshake_timeout := accept_timeout
	accept_poll_timeout := if accept_timeout > 0 && accept_timeout < tls_accept_poll_timeout {
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

	ch := chan &mbedtls.SSLConn{cap: s.pool_channel_slots}
	mut idle_conns := &TlsIdleConnTracker{}
	mut ws := []thread{cap: s.worker_num}
	for wid in 0 .. s.worker_num {
		ws << new_tls_handler_worker(wid, ch, s.handler, s.max_keep_alive_requests, idle_conns)
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
	accept_poll_timeout, handshake_timeout := tls_accept_timeouts(s.accept_timeout)
	for s.state == .running {
		mut conn := listener.accept_with_timeouts(accept_poll_timeout, handshake_timeout) or {
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
		if s.read_timeout > 0 {
			conn.set_read_timeout(s.read_timeout)
		}
		ch <- conn
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
mut:
	idle_conns &TlsIdleConnTracker = unsafe { nil }
pub mut:
	handler Handler
}

fn new_tls_handler_worker(wid int, ch chan &mbedtls.SSLConn, handler Handler, max_keep_alive_requests int, idle_conns &TlsIdleConnTracker) thread {
	mut w := &TlsHandlerWorker{
		id:                      wid
		ch:                      ch
		handler:                 handler
		max_keep_alive_requests: max_keep_alive_requests
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
	defer {
		w.idle_conns.unmark_idle(conn.handle)
		conn.shutdown() or {}
	}
	// If the TLS handshake negotiated HTTP/2 via ALPN, switch to the HTTP/2
	// driver; otherwise fall through to the existing HTTP/1.1 path unchanged.
	if conn.negotiated_alpn() == 'h2' {
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
