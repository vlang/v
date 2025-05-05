// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import io
import net
import time
import runtime
// ServerStatus is the current status of the server.
// .closed means that the server is completely inactive (the default on creation, and after calling .close()).
// .running means that the server is active and serving (after .listen_and_serve()).
// .stopped means that the server is not active but still listening (after .stop() ).

pub enum ServerStatus {
	closed
	running
	stopped
}

pub interface Handler {
mut:
	handle(Request) Response
}

pub const default_server_port = 9009

pub struct Server {
mut:
	state ServerStatus = .closed
pub mut:
	addr               string        = ':${default_server_port}'
	handler            Handler       = DebugHandler{}
	read_timeout       time.Duration = 30 * time.second
	write_timeout      time.Duration = 30 * time.second
	accept_timeout     time.Duration = 30 * time.second
	pool_channel_slots int           = 1024
	worker_num         int           = runtime.nr_jobs()
	listener           net.TcpListener

	on_running fn (mut s Server) = unsafe { nil } // Blocking cb. If set, ran by the web server on transitions to its .running state.
	on_stopped fn (mut s Server) = unsafe { nil } // Blocking cb. If set, ran by the web server on transitions to its .stopped state.
	on_closed  fn (mut s Server) = unsafe { nil } // Blocking cb. If set, ran by the web server on transitions to its .closed state.

	show_startup_message bool = true // set to false, to remove the default `Listening on ...` message.
}

// listen_and_serve listens on the server port `s.port` over TCP network and
// uses `s.parse_and_respond` to handle requests on incoming connections with `s.handler`.
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
		// listen_family := $if windows { net.AddrFamily.ip } $else { net.AddrFamily.ip6 }
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

	// Create tcp connection channel
	ch := chan &net.TcpConn{cap: s.pool_channel_slots}
	// Create workers
	mut ws := []thread{cap: s.worker_num}
	for wid in 0 .. s.worker_num {
		ws << new_handler_worker(wid, ch, s.handler)
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
				// Skip network timeouts, they are normal
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

// stop signals the server that it should not respond anymore.
@[inline]
pub fn (mut s Server) stop() {
	s.state = .stopped
	if s.on_stopped != unsafe { nil } {
		s.on_stopped(mut s)
	}
}

// close immediately closes the port and signals the server that it has been closed.
@[inline]
pub fn (mut s Server) close() {
	s.state = .closed
	s.listener.close() or { return }
	if s.on_closed != unsafe { nil } {
		s.on_closed(mut s)
	}
}

// status indicates whether the server is running, stopped, or closed.
@[inline]
pub fn (s &Server) status() ServerStatus {
	return s.state
}

// WaitTillRunningParams allows for parametrising the calls to s.wait_till_running()
@[params]
pub struct WaitTillRunningParams {
pub:
	max_retries     int = 100 // how many times to check for the status, for each single s.wait_till_running() call
	retry_period_ms int = 10  // how much time to wait between each check for the status, in milliseconds
}

// wait_till_running allows you to synchronise your calling (main) thread, with the state of the server
// (when the server is running in another thread).
// It returns an error, after params.max_retries * params.retry_period_ms
// milliseconds have passed, without that expected server transition.
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
	id int
	ch chan &net.TcpConn
pub mut:
	handler Handler
}

fn new_handler_worker(wid int, ch chan &net.TcpConn, handler Handler) thread {
	mut w := &HandlerWorker{
		id:      wid
		ch:      ch
		handler: handler
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
	mut req := parse_request(mut reader) or {
		$if debug {
			// only show in debug mode to prevent abuse
			eprintln('error parsing request: ${err}')
		}
		return
	}

	remote_ip := conn.peer_ip() or { '0.0.0.0' }
	req.header.add_custom('Remote-Addr', remote_ip) or {}

	mut resp := w.handler.handle(req)
	if resp.version() == .unknown {
		resp.set_version(req.version)
	}

	// Implemented by developers?
	if !resp.header.contains(.content_length) {
		resp.header.set(.content_length, '${resp.body.len}')
	}

	conn.write(resp.bytes()) or { eprintln('error sending response: ${err}') }
}

// DebugHandler implements the Handler interface by echoing the request
// in the response.
struct DebugHandler {}

fn (d DebugHandler) handle(req Request) Response {
	$if debug {
		eprintln('[${time.now()}] ${req.method} ${req.url}\n\r${req.header}\n\r${req.data} - 200 OK')
	} $else {
		eprintln('[${time.now()}] ${req.method} ${req.url} - 200')
	}
	mut r := Response{
		body:   req.data
		header: req.header
	}
	r.set_status(.ok)
	r.set_version(req.version)
	return r
}
