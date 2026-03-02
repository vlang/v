// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

import net
import net.quic
import sync

// HTTP/3 Server Implementation
// Supports server-side HTTP/3 connections over QUIC

// ServerConfig holds HTTP/3 server configuration
pub struct ServerConfig {
pub mut:
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
	// next_client_stream_id tracks client-initiated bidirectional stream IDs.
	// Per RFC 9114 (and RFC 9000), client-initiated bidirectional streams use
	// IDs: 0, 4, 8, 12, ... (multiples of 4).
	// This counter is incremented by 4 for each new request stream.
	// NOTE: This is a placeholder until proper QUIC stream demuxing is
	// implemented; a real QUIC layer would supply stream IDs directly.
	next_client_stream_id u64
	// rx_packet_number is an incrementing counter used as the GCM packet_number
	// argument to prevent GCM nonce reuse across packets on this connection.
	// TODO: replace with the actual QUIC packet number from the decrypted header
	// once full per-packet-key derivation is wired into the handshake.
	rx_packet_number u64
	mu               sync.Mutex
	encoder          Encoder
}

// ServerStream represents a single HTTP/3 stream on the server
struct ServerStream {
mut:
	id      u64
	headers []HeaderField
	data    []u8
	closed  bool
	// request_complete is set to true once process_request has been called for
	// this stream, preventing duplicate processing when both HEADERS and DATA
	// frames are received (Issue #7).
	request_complete bool
}

// new_server creates a new HTTP/3 server with the given configuration
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

// listen_and_serve starts the HTTP/3 server and begins accepting QUIC connections
pub fn (mut s Server) listen_and_serve() ! {
	s.running = true
	$if debug {
		eprintln('HTTP/3 server listening on ${s.config.addr}')
		eprintln('Using QUIC over UDP')
	}

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

// stop stops the HTTP/3 server and closes all active connections
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
		quic_conn:             quic_conn
		crypto_ctx:            crypto_ctx
		remote_addr:           remote_addr
		next_client_stream_id: 0
		encoder:               new_qpack_encoder(4096, 100)
		settings:              Settings{
			max_field_section_size:   8192
			qpack_max_table_capacity: 4096
			qpack_blocked_streams:    100
		}
	}
}

// handle_packet handles a received QUIC packet.
// Each call runs in its own goroutine; all mutations to conn are protected by
// conn.mu to prevent data races (Issue #31).
fn (mut s Server) handle_packet(mut conn ServerConnection, packet []u8) {
	// Decrypt packet.
	// base_iv is 12 bytes of zeros as a placeholder until proper per-key
	// IV derivation is wired in from the QUIC handshake.
	// rx_packet_number is incremented per packet to prevent GCM nonce reuse
	// (M5 fix — previously always 0, which fatally breaks GCM security).
	base_iv := []u8{len: 12}
	conn.mu.lock()
	pkt_num := conn.rx_packet_number
	conn.rx_packet_number++
	conn.mu.unlock()
	decrypted := conn.crypto_ctx.decrypt_packet(packet, []u8{}, base_iv, pkt_num) or {
		eprintln('Failed to decrypt packet: ${err}')
		return
	}

	// Parse frames.
	// NOTE: In a real QUIC implementation, stream_id is carried in the QUIC
	// STREAM frame header and would be provided by the QUIC layer.  Here we
	// maintain a per-connection counter that follows RFC 9000 §2.1:
	//   client-initiated bidirectional: 0, 4, 8, 12, …
	// current_stream_id is updated inside the mutex whenever a new HEADERS
	// frame opens a fresh client stream (Issue #6).
	mut idx := 0
	mut current_stream_id := u64(0)

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

		frame_type := frame_type_from_u64(frame_type_val) or {
			// Ignore unknown frame types per RFC 9114
			continue
		}

		if frame_type == .headers {
			// Allocate next client-initiated bidirectional stream ID
			// (0, 4, 8, 12, …) under the mutex (Issue #31, #6).
			conn.mu.lock()
			current_stream_id = conn.next_client_stream_id
			conn.next_client_stream_id += 4
			conn.mu.unlock()
		}

		// Process frame with current stream_id
		match frame_type {
			.headers {
				s.handle_headers_frame(mut conn, current_stream_id, payload) or {
					eprintln('Failed to handle HEADERS frame: ${err}')
				}
			}
			.data {
				s.handle_data_frame(mut conn, current_stream_id, payload) or {
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
