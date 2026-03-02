// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import net
import time

// Simple HTTP/2 Server Implementation

// ServerConfig holds server configuration
pub struct ServerConfig {
pub:
	addr                   string        = '0.0.0.0:8080'
	max_concurrent_streams u32           = 100
	initial_window_size    u32           = 65535
	max_frame_size         u32           = 16384
	read_timeout           time.Duration = 30 * time.second
	write_timeout          time.Duration = 30 * time.second
}

// ServerRequest represents an HTTP/2 request
pub struct ServerRequest {
pub:
	method    string
	path      string
	headers   map[string]string
	body      []u8
	stream_id u32
}

// ServerResponse represents an HTTP/2 response
pub struct ServerResponse {
pub:
	status_code int = 200
	headers     map[string]string
	body        []u8
}

// Handler processes requests
pub type Handler = fn (ServerRequest) ServerResponse

// ClientSettings holds the peer's SETTINGS values parsed from its SETTINGS frame.
// These are tracked per RFC 7540 §6.5.2 and may affect encoding, flow control, etc.
pub struct ClientSettings {
pub mut:
	header_table_size      u32 = 4096 // SETTINGS_HEADER_TABLE_SIZE (0x1)
	max_concurrent_streams u32 // SETTINGS_MAX_CONCURRENT_STREAMS (0x3); 0 = no limit (initial)
	initial_window_size    u32 = 65535 // SETTINGS_INITIAL_WINDOW_SIZE (0x4)
	max_frame_size         u32 = 16384 // SETTINGS_MAX_FRAME_SIZE (0x5)
	max_header_list_size   u32 // SETTINGS_MAX_HEADER_LIST_SIZE (0x6); 0 = unlimited (initial)
}

// Server is an HTTP/2 server.
//
// NOTE: This implementation uses plain TCP (net.TcpConn), NOT TLS.
// Real HTTP/2 deployments require TLS with ALPN "h2" negotiation for
// browser clients (RFC 7540 §3.3). The `tls` field is reserved for
// future implementation.
//
// TODO: Implement TLS with ALPN h2 negotiation (e.g. via net.ssl.SSLConn)
// to support browser-compatible HTTP/2 over TLS (h2).
// The plain-TCP mode ("h2c") is only supported by clients that explicitly
// opt in (e.g. `curl --http2-prior-knowledge`).
pub struct Server {
pub mut:
	// tls indicates whether TLS should be used. Currently always false;
	// TLS with ALPN h2 negotiation is not yet implemented.
	tls bool
mut:
	config   ServerConfig
	handler  ?Handler
	listener net.TcpListener
	running  bool
}

// new_server creates a new HTTP/2 server with the given configuration and handler.
// The returned server uses plain TCP (h2c). TLS is not yet implemented.
pub fn new_server(config ServerConfig, handler Handler) !&Server {
	listener := net.listen_tcp(.ip, config.addr)!

	return &Server{
		config:   config
		handler:  handler
		listener: listener
	}
}

// listen_and_serve starts the HTTP/2 server and begins accepting connections.
// It blocks until stop() is called. The server uses plain TCP (h2c mode).
// For TLS-based h2, TLS with ALPN negotiation must be implemented first.
pub fn (mut s Server) listen_and_serve() ! {
	s.running = true
	$if debug {
		eprintln('[HTTP/2] Server listening on ${s.config.addr}')
	}

	for s.running {
		mut conn := s.listener.accept() or {
			if s.running {
				eprintln('[HTTP/2] Accept error: ${err}')
			}
			continue
		}

		spawn s.handle_connection(mut conn)
	}
}

// stop stops the HTTP/2 server and closes the listener.
pub fn (mut s Server) stop() {
	s.running = false
	s.listener.close() or {}
}

// read_exact_tcp reads exactly `needed` bytes from a TCP connection into buf[0..needed].
// It loops on partial reads as required for TCP streams (TCP is a stream protocol and
// a single read() call may return fewer bytes than requested).
// Returns the number of bytes read (always == needed on success).
// Returns an error if the connection closes or an I/O error occurs before needed bytes arrive.
fn read_exact_tcp(mut conn net.TcpConn, mut buf []u8, needed int) !int {
	mut total := 0
	for total < needed {
		n := conn.read(mut buf[total..needed]) or { return error('read_exact_tcp: ${err}') }
		if n == 0 {
			return error('read_exact_tcp: connection closed after ${total}/${needed} bytes')
		}
		total += n
	}
	return total
}

// handle_connection handles a single client TCP connection.
fn (mut s Server) handle_connection(mut conn net.TcpConn) {
	defer {
		conn.close() or {}
	}

	// Read HTTP/2 preface
	mut preface_buf := []u8{len: preface.len}
	conn.set_read_timeout(s.config.read_timeout)

	read_exact_tcp(mut conn, mut preface_buf, preface.len) or {
		eprintln('[HTTP/2] Failed to read preface: ${err}')
		return
	}

	if preface_buf.bytestr() != preface {
		eprintln('[HTTP/2] Invalid preface')
		return
	}

	$if debug {
		eprintln('[HTTP/2] Preface received')
	}

	// Send SETTINGS frame
	s.write_settings(mut conn) or {
		eprintln('[HTTP/2] Failed to send settings: ${err}')
		return
	}

	// Create encoder/decoder
	mut encoder := new_encoder()
	mut decoder := new_decoder()

	// Track settings received from the client
	mut client_settings := ClientSettings{}

	// Main loop
	for {
		// Read frame
		frame := s.read_frame(mut conn) or {
			if err.msg().contains('EOF') {
				break
			}
			eprintln('[HTTP/2] Read frame error: ${err}')
			break
		}

		// Process frame
		match frame.header.frame_type {
			.settings {
				s.handle_settings(mut conn, frame, mut client_settings) or {
					eprintln('[HTTP/2] Settings error: ${err}')
				}
			}
			.headers {
				s.handle_headers(mut conn, frame, mut decoder, mut encoder) or {
					eprintln('[HTTP/2] Headers error: ${err}')
				}
			}
			.data {
				// DATA frames are handled with HEADERS
			}
			.ping {
				s.handle_ping(mut conn, frame) or { eprintln('[HTTP/2] Ping error: ${err}') }
			}
			else {
				// Ignore other frame types
			}
		}
	}

	$if debug {
		eprintln('[HTTP/2] Connection closed')
	}
}
