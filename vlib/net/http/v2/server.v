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

// Server is an HTTP/2 server
pub struct Server {
mut:
	config   ServerConfig
	handler  Handler = unsafe { nil }
	listener net.TcpListener
	running  bool
}

// new_server creates a new server
pub fn new_server(config ServerConfig, handler Handler) !&Server {
	listener := net.listen_tcp(.ip, config.addr)!

	return &Server{
		config:   config
		handler:  handler
		listener: listener
	}
}

// listen_and_serve starts the server
pub fn (mut s Server) listen_and_serve() ! {
	s.running = true
	println('[HTTP/2] Server listening on ${s.config.addr}')

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

// stop stops the server
pub fn (mut s Server) stop() {
	s.running = false
	s.listener.close() or {}
}

// handle_connection handles a client connection
fn (mut s Server) handle_connection(mut conn net.TcpConn) {
	defer {
		conn.close() or {}
	}

	// Read HTTP/2 preface
	mut preface_buf := []u8{len: preface.len}
	conn.set_read_timeout(s.config.read_timeout)

	n := conn.read(mut preface_buf) or {
		eprintln('[HTTP/2] Failed to read preface: ${err}')
		return
	}

	if n != preface.len || preface_buf.bytestr() != preface {
		eprintln('[HTTP/2] Invalid preface')
		return
	}

	println('[HTTP/2] Preface received')

	// Send SETTINGS frame
	s.send_settings(mut conn) or {
		eprintln('[HTTP/2] Failed to send settings: ${err}')
		return
	}

	// Create encoder/decoder
	mut encoder := new_encoder()
	mut decoder := new_decoder()

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
				s.handle_settings(mut conn, frame) or {
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

	println('[HTTP/2] Connection closed')
}

// send_settings sends initial SETTINGS frame
fn (mut s Server) send_settings(mut conn net.TcpConn) ! {
	mut payload := []u8{}

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
	println('[HTTP/2] Sent SETTINGS')
}

// handle_settings processes SETTINGS frame
fn (mut s Server) handle_settings(mut conn net.TcpConn, frame Frame) ! {
	// Check for ACK
	if frame.header.flags & u8(FrameFlags.ack) != 0 {
		println('[HTTP/2] Received SETTINGS ACK')
		return
	}

	println('[HTTP/2] Received SETTINGS')

	// Send ACK
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
	println('[HTTP/2] Sent SETTINGS ACK')
}

// handle_headers processes HEADERS frame
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

	println('[HTTP/2] Request: ${method} ${path}')

	// Call handler
	response := s.handler(request)

	// Send response
	s.send_response(mut conn, stream_id, response, mut encoder)!
}

// send_response sends HTTP/2 response
fn (mut s Server) send_response(mut conn net.TcpConn, stream_id u32, response ServerResponse, mut encoder Encoder) ! {
	// Build response headers
	mut resp_headers := []HeaderField{}
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

	if response.body.len > 0 {
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

	println('[HTTP/2] Response sent: ${response.status_code} (${response.body.len} bytes)')
}

// handle_ping processes PING frame
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
	println('[HTTP/2] PING/PONG')
}

// read_frame reads a frame from connection
fn (mut s Server) read_frame(mut conn net.TcpConn) !Frame {
	// Read header (9 bytes)
	mut header_buf := []u8{len: frame_header_size}
	conn.set_read_timeout(s.config.read_timeout)

	n := conn.read(mut header_buf) or { return error('read header: ${err}') }

	if n == 0 {
		return error('EOF')
	}

	if n != frame_header_size {
		return error('incomplete header')
	}

	header := parse_frame_header(header_buf)!

	// Read payload
	mut payload := []u8{len: int(header.length)}
	if header.length > 0 {
		bytes_read := conn.read(mut payload) or { return error('read payload: ${err}') }

		if bytes_read != int(header.length) {
			return error('incomplete payload')
		}
	}

	return Frame{
		header:  header
		payload: payload
	}
}

// write_frame writes a frame to connection
fn (mut s Server) write_frame(mut conn net.TcpConn, frame Frame) ! {
	data := encode_frame(frame)
	conn.set_write_timeout(s.config.write_timeout)
	conn.write(data)!
}
