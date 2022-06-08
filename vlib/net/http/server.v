// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import io
import net
import time

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
	port           int           = 8080
	handler        Handler       = DebugHandler{}
	read_timeout   time.Duration = 30 * time.second
	write_timeout  time.Duration = 30 * time.second
	accept_timeout time.Duration = 30 * time.second
}

pub fn (mut s Server) listen_and_serve() ? {
	if s.handler is DebugHandler {
		eprintln('Server handler not set, using debug handler')
	}
	s.listener = net.listen_tcp(.ip6, ':$s.port')?
	s.listener.set_accept_timeout(s.accept_timeout)
	eprintln('Listening on :$s.port')
	s.state = .running
	for {
		// break if we have a stop signal
		if s.state != .running {
			break
		}
		mut conn := s.listener.accept() or {
			if err.msg() != 'net: op timed out' {
				eprintln('accept() failed: $err; skipping')
			}
			continue
		}
		conn.set_read_timeout(s.read_timeout)
		conn.set_write_timeout(s.write_timeout)
		// TODO: make concurrent
		s.parse_and_respond(mut conn)
	}
	if s.state == .stopped {
		s.close()
	}
}

// stop signals the server that it should not respond anymore
[inline]
pub fn (mut s Server) stop() {
	s.state = .stopped
}

// close immediatly closes the port and signals the server that it has been closed
[inline]
pub fn (mut s Server) close() {
	s.state = .closed
	s.listener.close() or { return }
}

[inline]
pub fn (s &Server) status() ServerStatus {
	return s.state
}

fn (mut s Server) parse_and_respond(mut conn net.TcpConn) {
	defer {
		conn.close() or { eprintln('close() failed: $err') }
	}

	mut reader := io.new_buffered_reader(reader: conn)
	defer {
		reader.free()
	}
	req := parse_request(mut reader) or {
		$if debug {
			// only show in debug mode to prevent abuse
			eprintln('error parsing request: $err')
		}
		return
	}
	mut resp := s.handler.handle(req)
	if resp.version() == .unknown {
		resp.set_version(req.version)
	}
	conn.write(resp.bytes()) or { eprintln('error sending response: $err') }
}

// DebugHandler implements the Handler interface by echoing the request
// in the response
struct DebugHandler {}

fn (d DebugHandler) handle(req Request) Response {
	$if debug {
		eprintln('[$time.now()] $req.method $req.url\n\r$req.header\n\r$req.data - 200 OK')
	} $else {
		eprintln('[$time.now()] $req.method $req.url - 200')
	}
	mut r := Response{
		body: req.data
		header: req.header
	}
	r.set_status(.ok)
	r.set_version(req.version)
	return r
}
