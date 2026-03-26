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

// ServerConnection represents a single HTTP/3 client connection on the server side.
// It manages the QUIC transport, QPACK codec state, stream tracking, crypto context,
// and packet numbering for one connected peer. Protected by mutex for concurrent access.
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
	// for decryption nonce derivation. Once full QUIC header parsing is
	// integrated, this should be replaced with the packet number extracted
	// from the incoming QUIC packet header via extract_packet_number().
	rx_packet_number u64
	// tx_packet_number is a monotonically increasing counter used as the
	// packet_number for encrypt_packet() nonce derivation (RFC 9000 §17.1).
	tx_packet_number u64
	mu               sync.Mutex
	encoder          Encoder
	decoder          Decoder
	// uni manages the HTTP/3 unidirectional control streams (control, QPACK encoder,
	// QPACK decoder) required by RFC 9114 §6.2.
	uni UniStreamManager
}

// ServerStream represents a single HTTP/3 request stream on the server side.
// It accumulates headers and data frames for one client request, tracked by stream ID.
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
	mut crypto_ctx := quic.new_crypto_context_server(s.config.cert_file, s.config.key_file,
		['h3']) or { return error('failed to create crypto context: ${err}') }

	// Perform handshake — abort on failure to prevent unauthenticated connections
	quic_conn.perform_handshake_server(s.config.cert_file, s.config.key_file) or {
		eprintln('Handshake failed: ${err}')
		quic_conn.close()
		return error('handshake failed: ${err}')
	}

	// Derive initial secrets from the destination connection ID (RFC 9001 §5.2)
	// Server: tx_secret = server_secret, rx_secret = client_secret
	server_secret, client_secret := quic.derive_initial_secrets(quic_conn.conn_id, true) or {
		eprintln('Failed to derive initial secrets: ${err}')
		// Fall back: leave keys empty — packets will fail decrypt gracefully
		return &ServerConnection{
			quic_conn:             quic_conn
			crypto_ctx:            crypto_ctx
			remote_addr:           remote_addr
			next_client_stream_id: 0
			encoder:               new_qpack_encoder(4096, 100)
			decoder:               new_qpack_decoder(4096, 100)
			settings:              Settings{
				max_field_section_size:   8192
				qpack_max_table_capacity: 4096
				qpack_blocked_streams:    100
			}
		}
	}

	crypto_ctx.tx_secret = server_secret
	crypto_ctx.rx_secret = client_secret

	// Derive AES-128-GCM keys and IVs from the traffic secrets (RFC 9001 §5.1)
	crypto_ctx.derive_traffic_keys() or { eprintln('Failed to derive traffic keys: ${err}') }

	return &ServerConnection{
		quic_conn:             quic_conn
		crypto_ctx:            crypto_ctx
		remote_addr:           remote_addr
		next_client_stream_id: 0
		encoder:               new_qpack_encoder(4096, 100)
		decoder:               new_qpack_decoder(4096, 100)
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
	decrypted := s.decrypt_incoming_packet(mut conn, packet) or {
		eprintln('Failed to decrypt packet: ${err}')
		return
	}

	// Parse frames. current_stream_id is updated inside the mutex whenever
	// a new HEADERS frame opens a fresh client stream (Issue #6).
	mut idx := 0
	mut current_stream_id := u64(0)

	for idx < decrypted.len {
		frame_type_val, bytes_read := decode_varint(decrypted[idx..]) or {
			eprintln('Failed to decode frame type: ${err}')
			return
		}
		idx += bytes_read

		frame_length, bytes_read2 := decode_varint(decrypted[idx..]) or {
			eprintln('Failed to decode frame length: ${err}')
			return
		}
		idx += bytes_read2

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

		s.dispatch_server_frame(mut conn, frame_type, current_stream_id, payload)
	}
}

// decrypt_incoming_packet derives the base IV, extracts the packet number,
// and decrypts the incoming QUIC packet.
// When HP keys are available (rx_hp_key derived), the packet number is
// extracted via the RFC 9001 §5.4 pipeline: remove header protection first,
// then read the PN. When HP keys are not yet available (during the handshake),
// a monotonic counter is used as a fallback.
fn (mut s Server) decrypt_incoming_packet(mut conn ServerConnection, packet []u8) ![]u8 {
	// Prefer the derived rx_iv from traffic keys;
	// fall back to a zero IV when keys have not been derived yet.
	base_iv := if conn.crypto_ctx.rx_iv.len == 12 {
		conn.crypto_ctx.rx_iv
	} else {
		[]u8{len: 12}
	}

	conn.mu.lock()
	pkt_num := if conn.crypto_ctx.rx_hp_key.len > 0 {
		// HP keys derived: use the proper RFC 9001 §5.4 pipeline
		extracted_pn, _, _ := conn.crypto_ctx.extract_and_unprotect_pn(packet, conn.quic_conn.conn_id.len) or {
			// Fall back to monotonic counter on extraction failure
			pn := conn.rx_packet_number
			conn.rx_packet_number++
			conn.mu.unlock()
			return conn.crypto_ctx.decrypt_packet(packet, []u8{}, base_iv, pn)
		}
		conn.rx_packet_number = extracted_pn + 1
		conn.mu.unlock()
		extracted_pn
	} else {
		// No HP keys yet (handshake phase): use monotonic counter
		pn := conn.rx_packet_number
		conn.rx_packet_number++
		conn.mu.unlock()
		pn
	}

	return conn.crypto_ctx.decrypt_packet(packet, []u8{}, base_iv, pkt_num)
}

// dispatch_server_frame dispatches a parsed HTTP/3 frame to the appropriate
// handler based on its type.
fn (mut s Server) dispatch_server_frame(mut conn ServerConnection, frame_type FrameType, stream_id u64, payload []u8) {
	match frame_type {
		.headers {
			s.handle_headers_frame(mut conn, stream_id, payload) or {
				eprintln('Failed to handle HEADERS frame: ${err}')
			}
		}
		.data {
			s.handle_data_frame(mut conn, stream_id, payload) or {
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
