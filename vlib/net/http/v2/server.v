// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import net
import net.mbedtls
import sync
import time

// Simple HTTP/2 Server Implementation

// ServerConfig holds server configuration.
pub struct ServerConfig {
pub:
	addr                   string        = '0.0.0.0:8080'
	max_concurrent_streams u32           = 100
	initial_window_size    u32           = 65535
	max_frame_size         u32           = 16384
	read_timeout           time.Duration = 30 * time.second
	write_timeout          time.Duration = 30 * time.second
	// TLS configuration. When both cert_file and key_file are set, the
	// server uses TLS with ALPN "h2" negotiation (via net.mbedtls).
	// When empty, the server runs in plain TCP h2c mode.
	cert_file string
	key_file  string
}

// ServerRequest represents an HTTP/2 request.
pub struct ServerRequest {
pub:
	method    string
	path      string
	headers   map[string]string
	body      []u8
	stream_id u32
}

// ServerResponse represents an HTTP/2 response.
pub struct ServerResponse {
pub:
	status_code int = 200
	headers     map[string]string
	body        []u8
}

// Handler processes requests.
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

// ServerStreamState tracks the state of an HTTP/2 stream during request assembly.
// Each stream accumulates headers and body data from HEADERS and DATA frames
// until END_STREAM is received, at which point the handler is dispatched.
struct ServerStreamState {
mut:
	method     string
	path       string
	header_map map[string]string
	body       []u8
}

// Server is an HTTP/2 server.
//
// Supports plain TCP (h2c) mode by default. When cert_file and key_file
// are set in ServerConfig, the server uses TLS via net.mbedtls.SSLListener
// with ALPN "h2" negotiation for browser-compatible HTTP/2 (RFC 7540 §3.3).
// The plain-TCP mode ("h2c") is only supported by clients that explicitly
// opt in (e.g. `curl --http2-prior-knowledge`).
pub struct Server {
pub mut:
	// tls indicates whether TLS is enabled.
	// Set automatically when cert_file and key_file are provided.
	tls bool
mut:
	config            ServerConfig
	handler           ?Handler
	listener          net.TcpListener
	ssl_listener      &mbedtls.SSLListener = unsafe { nil }
	running           bool
	connections       []ServerConn
	conn_mu           sync.Mutex
	highest_stream_id u32
}

// new_server creates a new HTTP/2 server with the given configuration and handler.
// Uses plain TCP (h2c) when no cert/key files are configured.
// TLS with ALPN "h2" requires V's net.mbedtls.SSLListener, which binds its own
// socket — see the TLS limitation note in listen_and_serve().
pub fn new_server(config ServerConfig, handler Handler) !&Server {
	tls_enabled := config.cert_file != '' && config.key_file != ''
	if tls_enabled {
		// TLS mode: SSLListener does its own bind in listen_and_serve().
		// We still create a dummy TCP listener to keep the struct valid.
		// The actual TLS listener is created in listen_and_serve().
		return &Server{
			config:  config
			handler: handler
			tls:     true
		}
	}

	listener := net.listen_tcp(.ip, config.addr)!
	return &Server{
		config:   config
		handler:  handler
		listener: listener
	}
}

// listen_and_serve starts the HTTP/2 server and begins accepting connections.
// It blocks until stop() is called.
//
// In h2c mode (no TLS), uses plain TCP via net.TcpListener.
// In h2 mode (TLS enabled via cert_file/key_file), uses net.mbedtls.SSLListener
// with ALPN "h2" negotiation for browser-compatible HTTP/2 (RFC 7540 §3.3).
pub fn (mut s Server) listen_and_serve() ! {
	if s.tls {
		return s.listen_and_serve_tls()
	}

	s.running = true
	$if debug {
		eprintln('[HTTP/2] Server listening on ${s.config.addr} (h2c mode)')
	}

	for s.running {
		mut conn := s.listener.accept() or {
			if s.running {
				eprintln('[HTTP/2] Accept error: ${err}')
			}
			continue
		}
		conn.set_read_timeout(s.config.read_timeout)
		conn.set_write_timeout(s.config.write_timeout)

		spawn s.handle_connection(mut conn)
	}
}

// listen_and_serve_tls starts the HTTP/2 server in TLS mode using
// net.mbedtls.SSLListener with ALPN "h2" negotiation.
fn (mut s Server) listen_and_serve_tls() ! {
	s.ssl_listener = mbedtls.new_ssl_listener(s.config.addr, mbedtls.SSLConnectConfig{
		cert:           s.config.cert_file
		cert_key:       s.config.key_file
		alpn_protocols: ['h2']
	})!

	s.running = true
	$if debug {
		eprintln('[HTTP/2] Server listening on ${s.config.addr} (h2 TLS mode)')
	}

	for s.running {
		mut conn := s.ssl_listener.accept() or {
			if s.running {
				eprintln('[HTTP/2] TLS Accept error: ${err}')
			}
			continue
		}

		spawn s.handle_connection(mut conn)
	}
}

// stop sends GOAWAY to all active connections and shuts down the server.
// Per RFC 7540 §6.8, GOAWAY informs the peer of the last stream the server
// processed and that no further streams will be accepted.
pub fn (mut s Server) stop() {
	s.running = false
	s.send_goaway_to_all()
	if s.tls {
		if s.ssl_listener != unsafe { nil } {
			s.ssl_listener.shutdown() or {}
		}
	} else {
		s.listener.close() or {}
	}
}

// read_exact reads exactly `needed` bytes from a connection into buf[0..needed].
// It loops on partial reads as required for stream-oriented transports (TCP/TLS
// may return fewer bytes than requested in a single read() call).
// Returns the number of bytes read (always == needed on success).
// Returns an error if the connection closes or an I/O error occurs before needed bytes arrive.
fn read_exact(mut conn ServerConn, mut buf []u8, needed int) !int {
	mut total := 0
	for total < needed {
		n := conn.read(mut buf[total..needed]) or { return error('read_exact: ${err}') }
		if n == 0 {
			return error('read_exact: connection closed after ${total}/${needed} bytes')
		}
		total += n
	}
	return total
}

// handle_connection handles a single client connection with full HTTP/2
// stream multiplexing. Frames are read sequentially (single reader), while
// completed request handlers are dispatched concurrently via spawn.
// Accepts any connection satisfying ServerConn (TcpConn for h2c, SSLConn for h2).
fn (mut s Server) handle_connection(mut conn ServerConn) {
	mut ctx := &ConnContext{
		encoder: new_encoder()
	}

	defer {
		ctx.wg.wait()
		s.deregister_connection(conn)
		conn.close() or {}
	}

	s.register_connection(conn)

	s.read_preface(mut conn) or {
		eprintln('[HTTP/2] Preface error: ${err}')
		return
	}

	s.exchange_settings(mut conn) or {
		eprintln('[HTTP/2] Settings exchange error: ${err}')
		return
	}

	highest_stream_id := s.run_frame_loop(mut conn, mut ctx)

	// Update global highest stream ID for GOAWAY
	s.conn_mu.lock()
	if highest_stream_id > s.highest_stream_id {
		s.highest_stream_id = highest_stream_id
	}
	s.conn_mu.unlock()

	$if debug {
		eprintln('[HTTP/2] Connection closed')
	}
}

// read_preface reads and validates the HTTP/2 connection preface from the client.
// Returns an error if the preface is missing or invalid (RFC 7540 §3.5).
fn (mut s Server) read_preface(mut conn ServerConn) ! {
	mut preface_buf := []u8{len: preface.len}

	read_exact(mut conn, mut preface_buf, preface.len) or {
		return error('failed to read preface: ${err}')
	}

	if preface_buf.bytestr() != preface {
		return error('invalid preface')
	}

	$if debug {
		eprintln('[HTTP/2] Preface received')
	}
}

// exchange_settings sends the server SETTINGS and reads the client's SETTINGS ACK.
fn (mut s Server) exchange_settings(mut conn ServerConn) ! {
	s.write_settings(mut conn)!
}

// register_connection adds a connection to the active connections list.
fn (mut s Server) register_connection(conn ServerConn) {
	s.conn_mu.lock()
	s.connections << conn
	s.conn_mu.unlock()
}

// deregister_connection removes a connection from the active connections list.
fn (mut s Server) deregister_connection(conn ServerConn) {
	s.conn_mu.lock()
	s.connections = s.connections.filter(it != conn)
	s.conn_mu.unlock()
}

// send_goaway_to_all sends a GOAWAY frame to all active connections.
// Called during shutdown to gracefully notify peers.
fn (mut s Server) send_goaway_to_all() {
	s.conn_mu.lock()
	last_stream := s.highest_stream_id
	mut conns := s.connections.clone()
	s.conn_mu.unlock()

	goaway := GoAwayFrame{
		last_stream_id: last_stream
		error_code:     .no_error
	}
	frame_bytes := goaway.to_frame().encode()

	for mut c in conns {
		c.write(frame_bytes) or {}
	}
}
