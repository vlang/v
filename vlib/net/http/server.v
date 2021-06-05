// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import io
import net
import time

pub struct Server {
pub mut:
	port int = 8080
	handler fn(Request) Response
	read_timeout time.Duration = 30 * time.second
	write_timeout time.Duration = 30 * time.second
}

pub fn (mut s Server) listen_and_serve() ? {
	if voidptr(s.handler) == 0 {
		eprintln('Server handler not set, using debug handler')
		s.handler = fn(req Request) Response {
			$if debug {
				eprintln('[$time.now()] $req.method $req.url\n\r$req.header\n\r$req.data - 200 OK')
			} $else {
				eprintln('[$time.now()] $req.method $req.url - 200')
			}
			return Response{
				version: req.version
				text: req.data
				header: req.header
				cookies: req.cookies
				status_code: int(Status.ok)
			}
		}
	}
	mut l := net.listen_tcp(s.port) ?
	eprintln('Listening on :$s.port')
	for {
		mut conn := l.accept() or {
			eprintln('accept() failed: $err; skipping')
			continue
		}
		conn.set_read_timeout(s.read_timeout)
		conn.set_write_timeout(s.write_timeout)
		// TODO: make concurrent
		s.parse_and_respond(mut conn)
	}
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
	mut resp := s.handler(req)
	if resp.version == .unknown {
		resp.version = req.version
	}
	conn.write(resp.bytes()) or {
		eprintln('error sending response: $err')
	}
}
