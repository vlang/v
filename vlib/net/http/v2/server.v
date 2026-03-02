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

// write_settings sends the server's initial SETTINGS frame with a pre-allocated buffer.
fn (mut s Server) write_settings(mut conn net.TcpConn) ! {
	// Pre-allocate payload with exact size (3 settings * 6 bytes = 18 bytes)
	mut payload := []u8{cap: 18}

	// SETTINGS_MAX_CONCURRENT_STREAMS (0x03)
	payload << [u8(0), u8(3)]
	payload << [u8(s.config.max_concurrent_streams >> 24), u8(s.config.max_concurrent_streams >> 16),
		u8(s.config.max_concurrent_streams >> 8), u8(s.config.max_concurrent_streams)]

	// SETTINGS_INITIAL_WINDOW_SIZE (0x04)
	payload << [u8(0), u8(4)]
	payload << [u8(s.config.initial_window_size >> 24), u8(s.config.initial_window_size >> 16),
		u8(s.config.initial_window_size >> 8), u8(s.config.initial_window_size)]

	// SETTINGS_MAX_FRAME_SIZE (0x05)
	payload << [u8(0), u8(5)]
	payload << [u8(s.config.max_frame_size >> 24), u8(s.config.max_frame_size >> 16),
		u8(s.config.max_frame_size >> 8), u8(s.config.max_frame_size)]

	frame := Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .settings
			flags:      0
			stream_id:  0
		}
		payload: payload
	}

	s.write_frame(mut conn, frame)!
	$if debug {
		eprintln('[HTTP/2] Sent SETTINGS')
	}
}

// handle_settings processes a SETTINGS frame from the client.
// If the frame is an ACK, it is silently accepted.
// Otherwise, each setting key-value pair is parsed and stored in client_settings,
// and a SETTINGS ACK is sent back per RFC 7540 §6.5.
fn (mut s Server) handle_settings(mut conn net.TcpConn, frame Frame, mut client_settings ClientSettings) ! {
	// Check for ACK
	if frame.header.flags & u8(FrameFlags.ack) != 0 {
		$if debug {
			eprintln('[HTTP/2] Received SETTINGS ACK')
		}
		return
	}

	$if debug {
		eprintln('[HTTP/2] Received SETTINGS')
	}

	// Parse and apply client settings (each setting is 6 bytes: 2-byte ID + 4-byte value)
	payload := frame.payload
	mut i := 0
	for i + 6 <= payload.len {
		id := (u16(payload[i]) << 8) | u16(payload[i + 1])
		val := (u32(payload[i + 2]) << 24) | (u32(payload[i + 3]) << 16) | (u32(payload[i + 4]) << 8) | u32(payload[
			i + 5])
		i += 6

		match id {
			u16(SettingId.header_table_size) {
				client_settings.header_table_size = val
			}
			u16(SettingId.max_concurrent_streams) {
				client_settings.max_concurrent_streams = val
			}
			u16(SettingId.initial_window_size) {
				client_settings.initial_window_size = val
			}
			u16(SettingId.max_frame_size) {
				client_settings.max_frame_size = val
			}
			u16(SettingId.max_header_list_size) {
				client_settings.max_header_list_size = val
			}
			else {
				// Unknown setting identifiers must be ignored per RFC 7540 §6.5
			}
		}
	}

	// Send SETTINGS ACK
	ack := Frame{
		header:  FrameHeader{
			length:     0
			frame_type: .settings
			flags:      u8(FrameFlags.ack)
			stream_id:  0
		}
		payload: []u8{}
	}

	s.write_frame(mut conn, ack)!
	$if debug {
		eprintln('[HTTP/2] Sent SETTINGS ACK')
	}
}

// handle_headers processes a HEADERS frame, decodes the header block,
// invokes the request handler, and sends the response.
fn (mut s Server) handle_headers(mut conn net.TcpConn, frame Frame, mut decoder Decoder, mut encoder Encoder) ! {
	stream_id := frame.header.stream_id

	if stream_id == 0 {
		return error('HEADERS on stream 0')
	}

	// Decode headers
	headers := decoder.decode(frame.payload)!

	// Build request
	mut method := ''
	mut path := ''
	mut header_map := map[string]string{}

	for h in headers {
		match h.name {
			':method' { method = h.value }
			':path' { path = h.value }
			else { header_map[h.name] = h.value }
		}
	}

	request := ServerRequest{
		method:    method
		path:      path
		headers:   header_map
		body:      []u8{}
		stream_id: stream_id
	}

	$if debug {
		eprintln('[HTTP/2] Request: ${method} ${path}')
	}

	// Call handler
	h := s.handler or {
		error_response := ServerResponse{
			status_code: 500
			headers:     {
				'content-type': 'text/plain'
			}
			body:        'no handler configured'.bytes()
		}
		s.send_response(mut conn, stream_id, error_response, mut encoder) or {
			eprintln('[HTTP/2] Failed to send error response: ${err}')
		}
		return
	}

	response := h(request)

	// Send response
	s.send_response(mut conn, stream_id, response, mut encoder)!
}

// send_response encodes and sends an HTTP/2 response (HEADERS + optional DATA frame).
fn (mut s Server) send_response(mut conn net.TcpConn, stream_id u32, response ServerResponse, mut encoder Encoder) ! {
	// Pre-allocate headers array with capacity
	mut resp_headers := []HeaderField{cap: 2 + response.headers.len}
	resp_headers << HeaderField{
		name:  ':status'
		value: response.status_code.str()
	}

	for key, value in response.headers {
		resp_headers << HeaderField{
			name:  key
			value: value
		}
	}

	if response.body.len > 0 && 'content-length' !in response.headers {
		resp_headers << HeaderField{
			name:  'content-length'
			value: response.body.len.str()
		}
	}

	// Encode headers
	encoded := encoder.encode(resp_headers)

	// Send HEADERS frame
	headers_flags := if response.body.len == 0 {
		u8(FrameFlags.end_headers) | u8(FrameFlags.end_stream)
	} else {
		u8(FrameFlags.end_headers)
	}

	headers_frame := Frame{
		header:  FrameHeader{
			length:     u32(encoded.len)
			frame_type: .headers
			flags:      headers_flags
			stream_id:  stream_id
		}
		payload: encoded
	}

	s.write_frame(mut conn, headers_frame)!

	// Send DATA frame if needed
	if response.body.len > 0 {
		data_frame := Frame{
			header:  FrameHeader{
				length:     u32(response.body.len)
				frame_type: .data
				flags:      u8(FrameFlags.end_stream)
				stream_id:  stream_id
			}
			payload: response.body
		}

		s.write_frame(mut conn, data_frame)!
	}

	$if debug {
		eprintln('[HTTP/2] Response sent: ${response.status_code} (${response.body.len} bytes)')
	}
}

// handle_ping processes a PING frame and sends a PING ACK per RFC 7540 §6.7.
fn (mut s Server) handle_ping(mut conn net.TcpConn, frame Frame) ! {
	// Send PING ACK
	ack := Frame{
		header:  FrameHeader{
			length:     frame.header.length
			frame_type: .ping
			flags:      u8(FrameFlags.ack)
			stream_id:  0
		}
		payload: frame.payload.clone()
	}

	s.write_frame(mut conn, ack)!
	$if debug {
		eprintln('[HTTP/2] PING/PONG')
	}
}

// read_frame reads a complete HTTP/2 frame from the connection.
// Uses read_exact to ensure all bytes of both the 9-byte header and
// the variable-length payload are fully received before returning.
fn (mut s Server) read_frame(mut conn net.TcpConn) !Frame {
	// Read header (9 bytes) — use read_exact to handle partial TCP reads
	mut header_buf := []u8{len: frame_header_size}
	conn.set_read_timeout(s.config.read_timeout)

	read_exact_tcp(mut conn, mut header_buf, frame_header_size) or {
		return error('read header: ${err}')
	}

	header := parse_frame_header(header_buf) or {
		// Per RFC 7540 §4.1: unknown frame type — discard this frame
		return error('unknown frame type, frame discarded')
	}

	// Read payload — use read_exact to handle partial TCP reads
	mut payload := []u8{len: int(header.length)}
	if header.length > 0 {
		read_exact_tcp(mut conn, mut payload, int(header.length)) or {
			return error('read payload: ${err}')
		}
	}

	return Frame{
		header:  header
		payload: payload
	}
}

// write_frame encodes a frame and writes it to the connection.
fn (mut s Server) write_frame(mut conn net.TcpConn, frame Frame) ! {
	data := encode_frame(frame)
	conn.set_write_timeout(s.config.write_timeout)
	conn.write(data)!
}
