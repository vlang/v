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
}

struct ServerConnection {
mut:
	quic_conn             quic.Connection
	crypto_ctx            quic.CryptoContext
	streams               map[u64]&ServerStream
	settings              Settings
	remote_addr           string
	next_client_stream_id u64
	rx_packet_number      u64
	tx_packet_number      u64
	mu                    sync.Mutex
	encoder               Encoder
	decoder               Decoder
	uni                   UniStreamManager
}

struct ServerStream {
mut:
	id               u64
	headers          []HeaderField
	data             []u8
	closed           bool
	request_complete bool
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

		addr_str := '${addr.str()}'
		mut conn := s.connections[addr_str] or {
			new_conn := s.create_connection(addr_str) or {
				eprintln('Failed to create connection: ${err}')
				continue
			}
			s.connections[addr_str] = new_conn
			new_conn
		}

		packet_data := buf[..n].clone()
		spawn s.handle_packet(mut conn, packet_data)
	}
}

// stop stops the server and closes all active connections.
pub fn (mut s Server) stop() {
	s.running = false

	for _, mut conn in s.connections {
		conn.quic_conn.close()
	}

	s.udp_socket.close() or {}
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
		else {}
	}
}
