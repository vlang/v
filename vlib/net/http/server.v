// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import io
import net
import time
import runtime
// ServerStatus is the current status of the server.
// .running means that the server is active and serving.
// .stopped means that the server is not active but still listening.
// .closed means that the server is completely inactive.

pub enum ServerStatus {
	running
	stopped
	closed
}

interface Handler {
mut:
	handle(Request) Response
}

pub struct Server {
mut:
	state    ServerStatus = .closed
	listener net.TcpListener
pub mut:
	port               int           = 8080
	handler            Handler       = DebugHandler{}
	read_timeout       time.Duration = 30 * time.second
	write_timeout      time.Duration = 30 * time.second
	accept_timeout     time.Duration = 30 * time.second
	pool_channel_slots int = 1024
	worker_num         int = runtime.nr_jobs()
}

// listen_and_serve listens on the server port `s.port` over TCP network and
// uses `s.parse_and_respond` to handle requests on incoming connections with `s.handler`.
pub fn (mut s Server) listen_and_serve() {
	if s.handler is DebugHandler {
		eprintln('Server handler not set, using debug handler')
	}
	s.listener = net.listen_tcp(.ip6, ':${s.port}') or {
		eprintln('Listening on :${s.port} failed')
		return
	}
	s.listener.set_accept_timeout(s.accept_timeout)

	// Create tcp connection channel
	ch := chan &net.TcpConn{cap: s.pool_channel_slots}

	// Create workers
	mut ws := []thread{cap: s.worker_num}
	for wid in 0 .. s.worker_num {
		ws << new_handler_worker(wid, ch, s.handler)
	}

	eprintln('Listening on :${s.port}')
	s.state = .running
	for {
		// break if we have a stop signal
		if s.state != .running {
			break
		}
		mut conn := s.listener.accept() or {
			if err.code() == net.err_timed_out_code {
				// just skip network timeouts, they are normal
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
[inline]
pub fn (mut s Server) stop() {
	s.state = .stopped
}

// close immediately closes the port and signals the server that it has been closed.
[inline]
pub fn (mut s Server) close() {
	s.state = .closed
	s.listener.close() or { return }
}

// status indicates whether the server is running, stopped, or closed.
[inline]
pub fn (s &Server) status() ServerStatus {
	return s.state
}

struct HandlerWorker {
	id int
	ch chan &net.TcpConn
pub mut:
	handler Handler
}

fn new_handler_worker(wid int, ch chan &net.TcpConn, handler Handler) thread {
	mut w := &HandlerWorker{
		id: wid
		ch: ch
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

	remote_ip := conn.peer_ip() or { '' }
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
		body: req.data
		header: req.header
	}
	r.set_status(.ok)
	r.set_version(req.version)
	return r
}
