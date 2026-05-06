module v3

// HTTP/3 server over QUIC.
import net
import net.http.common
import net.quic
import sync

// ServerConfig holds HTTP/3 server configuration.
pub struct ServerConfig {
pub mut:
	addr                   string = '0.0.0.0:4433'
	max_concurrent_streams u32    = 100
	cert_file              string
	key_file               string
	handler                fn (common.ServerRequest) common.ServerResponse = default_server_handler
	max_stream_data        u64 = 1048576
	max_data               u64 = 10485760
	max_idle_timeout       u64 = 30000
	max_connections        int = 1000
	max_request_body_size  int = 10_485_760
}

pub type ServerRequest = common.ServerRequest

pub type ServerResponse = common.ServerResponse

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
	// next_client_stream_id tracks the next HTTP/3 stream ID to assign.
	// Starts at 0 and increments by 4, matching QUIC client-initiated
	// bidirectional stream IDs (0, 4, 8, 12, ...) per RFC 9000 §2.1.
	// This alignment is REQUIRED so that synthesized H3 stream IDs match
	// the real QUIC stream IDs reported by ngtcp2 FIN/close callbacks.
	next_client_stream_id      u64
	rx_packet_number           u64
	tx_packet_number           u64
	// mu guards fine-grained field access (encoder, decoder, streams, counters).
	mu                         sync.Mutex
	// packet_mu serializes entire packet processing per connection.
	// handle_packet is spawned per UDP packet; concurrent packets for the
	// same connection would race on ngtcp2_conn, stream_events, and stream
	// maps without this coarse-grained lock.
	packet_mu                  sync.Mutex
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
		conn.free()
	}
	s.mu.unlock()

	s.udp_socket.close() or {}
}

// free releases OpenSSL resources held by the server connection's crypto context.
fn (mut conn ServerConnection) free() {
	conn.crypto_ctx.free()
}

// build_goaway_shutdown_frames builds the 2-phase GOAWAY frame pair for graceful
// shutdown per RFC 9114 §5.2. Returns two encoded GOAWAY frames: first with
// max_varint (initial signal), second with the actual last stream ID.
pub fn (s &Server) build_goaway_shutdown_frames(mut conn ServerConnection) [][]u8 {
	initial := build_goaway_frame(max_varint) or { return [][]u8{} }
	final_frame := build_goaway_frame(conn.next_client_stream_id) or { return [][]u8{} }
	return [initial, final_frame]
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
