// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

import net
import net.quic

// HTTP/3 Server Implementation
// Supports server-side HTTP/3 connections over QUIC

// ServerConfig holds HTTP/3 server configuration
pub struct ServerConfig {
pub:
	addr                   string = '0.0.0.0:4433'
	max_concurrent_streams u32    = 100
	// TLS configuration
	cert_file string
	key_file  string
	// Handler
	handler fn (ServerRequest) ServerResponse = default_server_handler
	// QUIC settings
	max_stream_data  u64 = 1048576  // 1MB
	max_data         u64 = 10485760 // 10MB
	max_idle_timeout u64 = 30000    // 30 seconds
}

// ServerRequest represents an HTTP/3 server request
pub struct ServerRequest {
pub:
	method    string
	path      string
	headers   map[string]string
	body      []u8
	stream_id u64
}

// ServerResponse represents an HTTP/3 server response
pub struct ServerResponse {
pub:
	status_code int = 200
	headers     map[string]string
	body        []u8
}

// Server represents an HTTP/3 server
pub struct Server {
mut:
	config      ServerConfig
	udp_socket  net.UdpConn
	connections map[string]&ServerConnection
	running     bool
}

// ServerConnection represents a single HTTP/3 connection
struct ServerConnection {
mut:
	quic_conn   quic.Connection
	crypto_ctx  quic.CryptoContext
	streams     map[u64]&ServerStream
	settings    Settings
	remote_addr string
}

// ServerStream represents a single HTTP/3 stream on the server
struct ServerStream {
mut:
	id      u64
	headers []HeaderField
	data    []u8
	closed  bool
}

// new_server creates a new HTTP/3 server
pub fn new_server(config ServerConfig) !Server {
	// Validate configuration
	if config.cert_file == '' || config.key_file == '' {
		return error('cert_file and key_file are required for HTTP/3 server')
	}

	// Create UDP socket
	udp_socket := net.listen_udp(config.addr) or {
		return error('failed to create UDP socket: ${err}')
	}

	return Server{
		config:     config
		udp_socket: udp_socket
		running:    false
	}
}

// start starts the HTTP/3 server
pub fn (mut s Server) start() ! {
	s.running = true
	println('HTTP/3 server listening on ${s.config.addr}')
	println('Using QUIC over UDP')

	mut buf := []u8{len: 65536}

	for s.running {
		// Read UDP packet
		n, addr := s.udp_socket.read(mut buf) or {
			if s.running {
				eprintln('Failed to read packet: ${err}')
			}
			continue
		}

		if n == 0 {
			continue
		}

		// Get or create connection for this address
		addr_str := '${addr.str()}'
		mut conn := s.connections[addr_str] or {
			// New connection
			new_conn := s.create_connection(addr_str) or {
				eprintln('Failed to create connection: ${err}')
				continue
			}
			s.connections[addr_str] = new_conn
			new_conn
		}

		// Handle packet in separate thread
		packet_data := buf[..n].clone()
		spawn s.handle_packet(mut conn, packet_data)
	}
}

// stop stops the HTTP/3 server
pub fn (mut s Server) stop() {
	s.running = false

	// Close all connections
	for _, mut conn in s.connections {
		conn.quic_conn.close()
	}

	s.udp_socket.close() or {}
}

// create_connection creates a new server connection
fn (mut s Server) create_connection(remote_addr string) !&ServerConnection {
	// Create QUIC connection
	quic_config := quic.ConnectionConfig{
		remote_addr:                 remote_addr
		alpn:                        ['h3']
		max_stream_data_bidi_local:  s.config.max_stream_data
		max_stream_data_bidi_remote: s.config.max_stream_data
		max_data:                    s.config.max_data
		max_idle_timeout:            s.config.max_idle_timeout
	}

	mut quic_conn := quic.new_connection(quic_config) or {
		return error('failed to create QUIC connection: ${err}')
	}

	// Create crypto context
	crypto_ctx := quic.new_crypto_context_server(s.config.cert_file, s.config.key_file,
		['h3']) or { return error('failed to create crypto context: ${err}') }

	// Perform handshake
	quic_conn.perform_handshake_server(s.config.cert_file, s.config.key_file) or {
		eprintln('Handshake failed: ${err}')
		// Continue anyway, will retry
	}

	return &ServerConnection{
		quic_conn:   quic_conn
		crypto_ctx:  crypto_ctx
		remote_addr: remote_addr
		settings:    Settings{
			max_field_section_size:   8192
			qpack_max_table_capacity: 4096
			qpack_blocked_streams:    100
		}
	}
}

// handle_packet handles a received packet
fn (mut s Server) handle_packet(mut conn ServerConnection, packet []u8) {
	// Decrypt packet
	decrypted := conn.crypto_ctx.decrypt_packet(packet, []u8{}) or {
		eprintln('Failed to decrypt packet: ${err}')
		return
	}

	// Parse frames
	mut idx := 0
	for idx < decrypted.len {
		// Decode frame type
		frame_type_val, bytes_read := decode_varint(decrypted[idx..]) or {
			eprintln('Failed to decode frame type: ${err}')
			return
		}
		idx += bytes_read

		// Decode frame length
		frame_length, bytes_read2 := decode_varint(decrypted[idx..]) or {
			eprintln('Failed to decode frame length: ${err}')
			return
		}
		idx += bytes_read2

		// Read payload
		if idx + int(frame_length) > decrypted.len {
			eprintln('Incomplete frame')
			return
		}

		payload := decrypted[idx..idx + int(frame_length)]
		idx += int(frame_length)

		frame_type := unsafe { FrameType(frame_type_val) }

		// Process frame
		match frame_type {
			.headers {
				s.handle_headers_frame(mut conn, payload) or {
					eprintln('Failed to handle HEADERS frame: ${err}')
				}
			}
			.data {
				s.handle_data_frame(mut conn, payload) or {
					eprintln('Failed to handle DATA frame: ${err}')
				}
			}
			.settings {
				s.handle_settings_frame(mut conn, payload) or {
					eprintln('Failed to handle SETTINGS frame: ${err}')
				}
			}
			else {
				// Ignore unknown frames
			}
		}
	}
}

// handle_headers_frame handles a HEADERS frame
fn (mut s Server) handle_headers_frame(mut conn ServerConnection, payload []u8) ! {
	// Decode headers using QPACK
	headers := decode_headers(payload)!

	// Extract stream ID (simplified - should be from frame header)
	stream_id := u64(1) // TODO: Get from frame header

	// Get or create stream
	mut stream := conn.streams[stream_id] or {
		new_stream := &ServerStream{
			id: stream_id
		}
		conn.streams[stream_id] = new_stream
		new_stream
	}

	stream.headers << headers

	// Check if this is a complete request (no body expected)
	// Process request
	s.process_request(mut conn, stream)!
}

// handle_data_frame handles a DATA frame
fn (mut s Server) handle_data_frame(mut conn ServerConnection, payload []u8) ! {
	// Extract stream ID (simplified)
	stream_id := u64(1) // TODO: Get from frame header

	mut stream := conn.streams[stream_id] or { return error('stream ${stream_id} not found') }

	// Append data
	stream.data << payload

	// Process request if complete
	s.process_request(mut conn, stream)!
}

// handle_settings_frame handles a SETTINGS frame
fn (mut s Server) handle_settings_frame(mut conn ServerConnection, payload []u8) ! {
	// Parse settings
	mut idx := 0
	for idx < payload.len {
		// Decode setting ID
		setting_id, bytes_read := decode_varint(payload[idx..])!
		idx += bytes_read

		// Decode setting value
		setting_value, bytes_read2 := decode_varint(payload[idx..])!
		idx += bytes_read2

		// Update settings
		match setting_id {
			0x01 { conn.settings.qpack_max_table_capacity = setting_value }
			0x06 { conn.settings.max_field_section_size = setting_value }
			0x07 { conn.settings.qpack_blocked_streams = setting_value }
			else {}
		}
	}

	// Send SETTINGS ACK (simplified)
	println('Received SETTINGS from client')
}

// process_request processes a complete HTTP/3 request
fn (mut s Server) process_request(mut conn ServerConnection, stream &ServerStream) ! {
	// Build request from headers and data
	mut method := ''
	mut path := ''
	mut headers := map[string]string{}

	for header in stream.headers {
		if header.name == ':method' {
			method = header.value
		} else if header.name == ':path' {
			path = header.value
		} else if !header.name.starts_with(':') {
			headers[header.name] = header.value
		}
	}

	request := ServerRequest{
		method:    method
		path:      path
		headers:   headers
		body:      stream.data
		stream_id: stream.id
	}

	println('[HTTP/3] ${method} ${path}')

	// Call handler
	response := s.config.handler(request)

	// Send response
	s.send_response(mut conn, stream.id, response)!
}

// send_response sends an HTTP/3 response
fn (mut s Server) send_response(mut conn ServerConnection, stream_id u64, response ServerResponse) ! {
	// Build response headers
	mut headers := []HeaderField{}
	headers << HeaderField{':status', response.status_code.str()}

	for key, value in response.headers {
		headers << HeaderField{key, value}
	}

	// Add content-length if not present
	if 'content-length' !in response.headers {
		headers << HeaderField{'content-length', response.body.len.str()}
	}

	// Encode headers using QPACK
	encoded_headers := encode_headers(headers)

	// Build HEADERS frame
	mut frame_data := []u8{}
	frame_data << encode_varint(u64(FrameType.headers))
	frame_data << encode_varint(u64(encoded_headers.len))
	frame_data << encoded_headers

	// Build DATA frame if body exists
	if response.body.len > 0 {
		frame_data << encode_varint(u64(FrameType.data))
		frame_data << encode_varint(u64(response.body.len))
		frame_data << response.body
	}

	// Encrypt packet
	encrypted := conn.crypto_ctx.encrypt_packet(frame_data, []u8{}) or {
		return error('failed to encrypt response: ${err}')
	}

	// Send via QUIC
	conn.quic_conn.send_with_crypto(stream_id, encrypted, &conn.crypto_ctx) or {
		return error('failed to send response: ${err}')
	}

	println('[HTTP/3] Response sent: ${response.status_code}')
}

// default_server_handler is the default request handler
fn default_server_handler(req ServerRequest) ServerResponse {
	body := 'Hello from HTTP/3 server!\nPath: ${req.path}\nMethod: ${req.method}\nProtocol: HTTP/3 (QUIC)'.bytes()

	return ServerResponse{
		status_code: 200
		headers:     {
			'content-type': 'text/plain'
			'server':       'V HTTP/3 Server'
		}
		body:        body
	}
}
