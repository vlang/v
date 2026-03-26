module v3

// HTTP/3 server over QUIC.
import net
import net.quic
import sync

// ServerConfig holds HTTP/3 server configuration.
pub struct ServerConfig {
pub mut:
	addr                   string = '0.0.0.0:4433'
	max_concurrent_streams u32    = 100
	cert_file              string
	key_file               string
	handler                fn (ServerRequest) ServerResponse = default_server_handler
	max_stream_data        u64 = 1048576
	max_data               u64 = 10485760
	max_idle_timeout       u64 = 30000
}

// ServerRequest represents an HTTP/3 server request.
pub struct ServerRequest {
pub:
	method    string
	path      string
	headers   map[string]string
	body      []u8
	stream_id u64
}

// ServerResponse represents an HTTP/3 server response.
pub struct ServerResponse {
pub:
	status_code int = 200
	headers     map[string]string
	body        []u8
}

// Server represents an HTTP/3 server.
pub struct Server {
mut:
	config      ServerConfig
	udp_socket  net.UdpConn
	connections map[string]&ServerConnection
	running     bool
	mu          sync.Mutex
}

struct ServerConnection {
mut:
	quic_conn                  quic.Connection
	crypto_ctx                 quic.CryptoContext
	streams                    map[u64]&ServerStream
	settings                   Settings
	remote_addr                string
	next_client_stream_id      u64
	rx_packet_number           u64
	tx_packet_number           u64
	mu                         sync.Mutex
	encoder                    Encoder
	decoder                    Decoder
	uni                        UniStreamManager
	last_peer_goaway_stream_id u64
}

struct ServerStream {
mut:
	id               u64
	headers          []HeaderField
	data             []u8
	closed           bool
	request_complete bool
	headers_received bool
}

// new_server creates an HTTP/3 server with the given configuration.
pub fn new_server(config ServerConfig) !Server {
	if config.cert_file == '' || config.key_file == '' {
		return error('cert_file and key_file are required for HTTP/3 server')
	}

	udp_socket := net.listen_udp(config.addr) or {
		return error('failed to create UDP socket: ${err}')
	}

	return Server{
		config:     config
		udp_socket: udp_socket
		running:    false
	}
}

// default_cid_len is the default connection ID length in bytes used by
// the server for parsing short header DCID fields (RFC 9000 §5.2).
const default_cid_len = 18

// listen_and_serve starts the server and begins accepting QUIC connections.
pub fn (mut s Server) listen_and_serve() ! {
	s.running = true
	$if debug {
		eprintln('HTTP/3 server listening on ${s.config.addr}')
		eprintln('Using QUIC over UDP')
	}

	mut buf := []u8{len: 65536}

	for s.running {
		n, addr := s.udp_socket.read(mut buf) or {
			if s.running {
				eprintln('Failed to read packet: ${err}')
			}
			continue
		}

		if n == 0 {
			continue
		}

		packet_data := buf[..n].clone()
		mut conn := s.lookup_or_create_connection(packet_data, addr) or {
			eprintln('Failed to handle connection: ${err}')
			continue
		}

		spawn s.handle_packet(mut conn, packet_data)
	}
}

// stop stops the server gracefully using 2-phase GOAWAY (RFC 9114 §5.2).
// Phase 1: GOAWAY with max_varint signals "stopping soon" to peers.
// Phase 2: GOAWAY with the actual last processed stream ID.
pub fn (mut s Server) stop() {
	s.running = false

	s.mu.lock()
	for _, mut conn in s.connections {
		goaway_frames := s.build_goaway_shutdown_frames(mut conn)
		if conn.uni.control_stream_id >= 0 {
			ctrl_id := u64(conn.uni.control_stream_id)
			for frame_data in goaway_frames {
				conn.quic_conn.send(ctrl_id, frame_data) or {}
			}
		}
		conn.quic_conn.close()
	}
	s.mu.unlock()

	s.udp_socket.close() or {}
}

// extract_dcid_from_packet extracts the destination connection ID from a QUIC
// packet header as a hex string. For short headers (bit 7 = 0), DCID starts at
// byte 1 with length cid_len. For long headers (bit 7 = 1), byte 5 holds the
// DCID length and DCID starts at byte 6 (RFC 9000 §5.2).
pub fn extract_dcid_from_packet(packet []u8, cid_len int) !string {
	if packet.len < 2 {
		return error('packet too short to extract DCID')
	}

	is_long := (packet[0] & 0x80) != 0

	if is_long {
		return extract_dcid_long_header(packet)
	}
	return extract_dcid_short_header(packet, cid_len)
}

// extract_dcid_short_header reads DCID from a short header packet where the
// DCID starts at byte 1 and has the given cid_len (RFC 9000 §17.3).
fn extract_dcid_short_header(packet []u8, cid_len int) !string {
	end := 1 + cid_len
	if packet.len < end {
		return error('packet too short for short header DCID (need ${end}, have ${packet.len})')
	}
	return bytes_to_hex(packet[1..end])
}

// extract_dcid_long_header reads DCID from a long header packet where byte 5
// holds the DCID length and DCID starts at byte 6 (RFC 9000 §17.2).
fn extract_dcid_long_header(packet []u8) !string {
	if packet.len < 6 {
		return error('packet too short for long header DCID length field')
	}
	dcid_len := int(packet[5])
	end := 6 + dcid_len
	if packet.len < end {
		return error('packet too short for long header DCID (need ${end}, have ${packet.len})')
	}
	if dcid_len == 0 {
		return ''
	}
	return bytes_to_hex(packet[6..end])
}

// bytes_to_hex converts a byte slice to a lowercase hex string.
fn bytes_to_hex(data []u8) string {
	hex_chars := '0123456789abcdef'
	mut result := []u8{cap: data.len * 2}
	for b in data {
		result << hex_chars[b >> 4]
		result << hex_chars[b & 0x0f]
	}
	return result.bytestr()
}

// build_goaway_shutdown_frames builds the 2-phase GOAWAY frame pair for graceful
// shutdown per RFC 9114 §5.2. Returns two encoded GOAWAY frames: first with
// max_varint (initial signal), second with the actual last stream ID.
pub fn (s &Server) build_goaway_shutdown_frames(mut conn ServerConnection) [][]u8 {
	initial := build_goaway_frame(max_varint) or { return [][]u8{} }
	final_frame := build_goaway_frame(conn.next_client_stream_id) or { return [][]u8{} }
	return [initial, final_frame]
}

// lookup_or_create_connection finds an existing connection by DCID or creates
// a new one. Uses CID-based lookup per RFC 9000 §5.2; falls back to creating
// a new connection for unknown CIDs (initial packets).
fn (mut s Server) lookup_or_create_connection(packet []u8, addr net.Addr) !&ServerConnection {
	addr_str := '${addr.str()}'
	dcid := extract_dcid_from_packet(packet, default_cid_len) or { '' }

	s.mu.lock()
	if dcid.len > 0 {
		if mut existing := s.connections[dcid] {
			s.mu.unlock()
			return existing
		}
	}

	new_conn := s.create_connection(addr_str) or {
		s.mu.unlock()
		return error('failed to create connection: ${err}')
	}

	cid_key := if dcid.len > 0 {
		dcid
	} else {
		bytes_to_hex(new_conn.quic_conn.conn_id)
	}
	s.connections[cid_key] = new_conn
	s.mu.unlock()
	return new_conn
}

fn (mut s Server) create_connection(remote_addr string) !&ServerConnection {
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

	mut crypto_ctx := quic.new_crypto_context_server(s.config.cert_file, s.config.key_file,
		['h3']) or { return error('failed to create crypto context: ${err}') }

	quic_conn.perform_handshake_server(s.config.cert_file, s.config.key_file) or {
		eprintln('Handshake failed: ${err}')
		quic_conn.close()
		return error('handshake failed: ${err}')
	}

	server_secret, client_secret := quic.derive_initial_secrets(quic_conn.conn_id, true) or {
		eprintln('Failed to derive initial secrets: ${err}')
		mut conn := new_server_connection(quic_conn, crypto_ctx, remote_addr)
		open_server_uni_streams(mut conn)
		return conn
	}

	crypto_ctx.tx_secret = server_secret
	crypto_ctx.rx_secret = client_secret

	crypto_ctx.derive_traffic_keys() or { eprintln('Failed to derive traffic keys: ${err}') }

	mut conn := new_server_connection(quic_conn, crypto_ctx, remote_addr)
	open_server_uni_streams(mut conn)
	return conn
}

// new_server_connection creates a ServerConnection with default QPACK and settings.
fn new_server_connection(quic_conn quic.Connection, crypto_ctx quic.CryptoContext, remote_addr string) &ServerConnection {
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

// open_server_uni_streams opens all 3 unidirectional streams (control, encoder, decoder)
// and sends initial SETTINGS on the control stream.
fn open_server_uni_streams(mut conn ServerConnection) {
	conn.uni.open_streams(mut conn.quic_conn) or {
		$if debug {
			eprintln('warning: failed to open server unidirectional streams: ${err}')
		}
		return
	}
	send_server_settings(mut conn) or {
		$if debug {
			eprintln('warning: failed to send server SETTINGS: ${err}')
		}
	}
}

fn send_server_settings(mut conn ServerConnection) ! {
	if conn.uni.control_stream_id < 0 {
		return error('server control stream not opened')
	}
	ctrl_id := u64(conn.uni.control_stream_id)

	mut payload := []u8{}
	payload << encode_varint(u64(0x06))!
	payload << encode_varint(conn.settings.max_field_section_size)!
	payload << encode_varint(u64(0x01))!
	payload << encode_varint(conn.settings.qpack_max_table_capacity)!
	payload << encode_varint(u64(0x07))!
	payload << encode_varint(conn.settings.qpack_blocked_streams)!

	mut data := []u8{}
	data << encode_varint(u64(FrameType.settings))!
	data << encode_varint(u64(payload.len))!
	data << payload

	conn.quic_conn.send(ctrl_id, data)!
}

fn (mut s Server) handle_packet(mut conn ServerConnection, packet []u8) {
	decrypted := s.decrypt_incoming_packet(mut conn, packet) or {
		eprintln('Failed to decrypt packet: ${err}')
		return
	}

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

		frame_type := frame_type_from_u64(frame_type_val) or { continue }

		if frame_type == .headers {
			conn.mu.lock()
			current_stream_id = conn.next_client_stream_id
			conn.next_client_stream_id += 4
			conn.mu.unlock()
		}

		s.dispatch_server_frame(mut conn, frame_type, current_stream_id, payload)
	}
}

fn (mut s Server) decrypt_incoming_packet(mut conn ServerConnection, packet []u8) ![]u8 {
	base_iv := if conn.crypto_ctx.rx_iv.len == 12 {
		conn.crypto_ctx.rx_iv
	} else {
		[]u8{len: 12}
	}

	conn.mu.lock()
	pkt_num := if conn.crypto_ctx.rx_hp_key.len > 0 {
		extracted_pn, _, _ := conn.crypto_ctx.extract_and_unprotect_pn(packet, conn.quic_conn.conn_id.len) or {
			pn := conn.rx_packet_number
			conn.rx_packet_number++
			conn.mu.unlock()
			return conn.crypto_ctx.decrypt_packet(packet, []u8{}, base_iv, pn)
		}
		conn.rx_packet_number = extracted_pn + 1
		conn.mu.unlock()
		extracted_pn
	} else {
		pn := conn.rx_packet_number
		conn.rx_packet_number++
		conn.mu.unlock()
		pn
	}

	return conn.crypto_ctx.decrypt_packet(packet, []u8{}, base_iv, pkt_num)
}

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
		.goaway {
			if payload.len > 0 {
				goaway_id, _ := decode_varint(payload) or {
					eprintln('Failed to decode GOAWAY stream ID: ${err}')
					return
				}
				conn.last_peer_goaway_stream_id = goaway_id
				$if debug {
					eprintln('Received GOAWAY with stream ID ${goaway_id}')
				}
			}
		}
		else {}
	}
}
