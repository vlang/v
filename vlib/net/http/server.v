// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import io
import net
import time
import runtime

pub enum ServerStatus {
	closed
	running
	stopped
}

pub const default_server_port = 9009

pub struct Server {
mut:
	state ServerStatus = .closed
pub mut:
	addr                    string        = ':${default_server_port}'
	handler                 ServerHandler = DebugHandler{}
	read_timeout            time.Duration = 30 * time.second
	write_timeout           time.Duration = 30 * time.second
	accept_timeout          time.Duration = 30 * time.second
	pool_channel_slots      int           = 1024
	worker_num              int           = runtime.nr_jobs()
	max_keep_alive_requests int           = 100
	listener                net.TcpListener

	on_running fn (mut s Server) = unsafe { nil }
	on_stopped fn (mut s Server) = unsafe { nil }
	on_closed  fn (mut s Server) = unsafe { nil }

	max_request_body_size int  = 10_485_760 // 10 MB, same default as HTTP/2 and HTTP/3
	show_startup_message  bool = true
	cert_file             string // TLS cert; when set with key_file, enables HTTPS
	key_file             string
	enable_h3            bool   // when true and TLS enabled, also serve HTTP/3 on UDP
	tls_addr             string // optional TLS listen address for listen_and_serve_all()
	h3_addr              string // optional HTTP/3 listen address for listen_and_serve_all()
}

pub fn (mut s Server) listen_and_serve() {
	if s.handler is DebugHandler {
		eprintln('Server handler not set, using debug handler')
	}

	mut l := s.listener.addr() or {
		eprintln('Failed getting listener address, err: ${err}')
		return
	}
	if l.family() == net.AddrFamily.unspec {
		listening_address := if s.addr == '' || s.addr == ':0' { 'localhost:0' } else { s.addr }
		listen_family := net.AddrFamily.ip
		s.listener = net.listen_tcp(listen_family, listening_address) or {
			eprintln('Listening on ${s.addr} failed, err: ${err}')
			return
		}
		l = s.listener.addr() or {
			eprintln('Failed getting listener address 2, err: ${err}')
			return
		}
	}
	s.addr = l.str()
	s.listener.set_accept_timeout(s.accept_timeout)

	ch := chan &net.TcpConn{cap: s.pool_channel_slots}
	mut ws := []thread{cap: s.worker_num}
	for wid in 0 .. s.worker_num {
		ws << new_handler_worker(wid, ch, s.handler, s.max_keep_alive_requests, s.max_request_body_size)
	}

	if s.show_startup_message {
		println('Listening on http://${s.addr}/')
		flush_stdout()
	}

	time.sleep(20 * time.millisecond)
	s.state = .running
	if s.on_running != unsafe { nil } {
		s.on_running(mut s)
	}
	for s.state == .running {
		mut conn := s.listener.accept() or {
			if err.code() == net.err_timed_out_code {
				continue
			}
			eprintln('accept() failed, reason: ${err}; skipping')
			continue
		}
		conn.set_read_timeout(s.read_timeout)
		conn.set_write_timeout(s.write_timeout)
		ch <- conn
	}
	if s.state == .stopped {
		s.close()
	}
}

@[inline]
pub fn (mut s Server) stop() {
	s.state = .stopped
	if s.on_stopped != unsafe { nil } {
		s.on_stopped(mut s)
	}
}

@[inline]
pub fn (mut s Server) close() {
	s.state = .closed
	s.listener.close() or { return }
	if s.on_closed != unsafe { nil } {
		s.on_closed(mut s)
	}
}

@[inline]
pub fn (s &Server) status() ServerStatus {
	return s.state
}

@[params]
pub struct WaitTillRunningParams {
pub:
	max_retries     int = 100
	retry_period_ms int = 10
}

pub fn (mut s Server) wait_till_running(params WaitTillRunningParams) !int {
	mut i := 0
	for s.status() != .running && i < params.max_retries {
		time.sleep(params.retry_period_ms * time.millisecond)
		i++
	}
	if i >= params.max_retries {
		return error('maximum retries reached')
	}
	time.sleep(params.retry_period_ms)
	return i
}

struct HandlerWorker {
	id                      int
	ch                      chan &net.TcpConn
	max_keep_alive_requests int
	max_request_body_size   int
pub mut:
	handler ServerHandler
}

fn new_handler_worker(wid int, ch chan &net.TcpConn, handler ServerHandler, max_keep_alive_requests int, max_request_body_size int) thread {
	mut w := &HandlerWorker{
		id:                      wid
		ch:                      ch
		handler:                 handler
		max_keep_alive_requests: max_keep_alive_requests
		max_request_body_size:   max_request_body_size
	}
	return spawn w.process_requests()
}

fn (mut w HandlerWorker) process_requests() {
	for {
		mut conn := <-w.ch or { break }
		w.handle_conn(mut conn)
	}
}

fn (mut w HandlerWorker) handle_conn(mut conn net.TcpConn) {
	defer {
		conn.close() or { eprintln('close() failed: ${err}') }
	}

	mut reader := io.new_buffered_reader(reader: conn)
	defer {
		unsafe {
			reader.free()
		}
	}

	mut request_count := 0
	for {
		mut req := parse_request_with_limit(mut reader, w.max_request_body_size) or {
			if err.msg().starts_with('request body too large') {
				conn.write('HTTP/1.1 413 Payload Too Large\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()) or {}
			} else {
				$if debug {
					eprintln('error parsing request: ${err}')
				}
			}
			return
		}
		request_count++

		remote_ip := conn.peer_ip() or { '0.0.0.0' }
		set_server_only_header(mut req.header, 'Remote-Addr', remote_ip)

		server_req := request_to_server_request(&req)
		server_resp := w.handler.handle(server_req)
		mut resp := server_response_to_response(server_resp, req.version)

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
			eprintln('error sending response: ${err}')
			return
		}

		if !keep_alive {
			return
		}
	}
}
