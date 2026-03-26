module v2

// HTTP/2 server supporting both plain TCP (h2c) and TLS (h2) modes.
import net
import net.mbedtls
import sync
import time

// ServerConfig holds server configuration.
pub struct ServerConfig {
pub:
	addr                   string        = '0.0.0.0:8080'
	max_concurrent_streams u32           = 100
	initial_window_size    u32           = 65535
	max_frame_size         u32           = 16384
	read_timeout           time.Duration = 30 * time.second
	write_timeout          time.Duration = 30 * time.second
	// TLS configuration: when both are set, uses TLS with ALPN "h2";
	// when empty, runs in plain TCP h2c mode.
	cert_file             string
	key_file              string
	max_connections       int = 1000
	max_request_body_size int = 10_485_760
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

// ClientSettings holds the peer's SETTINGS values per RFC 7540 §6.5.2.
pub struct ClientSettings {
pub mut:
	header_table_size      u32 = 4096
	max_concurrent_streams u32
	initial_window_size    u32 = 65535
	max_frame_size         u32 = 16384
	max_header_list_size   u32
}

// ServerStreamState tracks the state of an HTTP/2 stream during request assembly.
struct ServerStreamState {
mut:
	method     string
	path       string
	header_map map[string]string
	body       []u8
}

// Server is an HTTP/2 server.
pub struct Server {
pub mut:
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
pub fn new_server(config ServerConfig, handler Handler) !&Server {
	tls_enabled := config.cert_file != '' && config.key_file != ''
	if tls_enabled {
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

		if s.at_connection_limit() {
			conn.close() or {}
			continue
		}

		conn.set_read_timeout(s.config.read_timeout)
		conn.set_write_timeout(s.config.write_timeout)

		spawn s.handle_connection(mut conn)
	}
}

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

		if s.at_connection_limit() {
			conn.close() or {}
			continue
		}

		spawn s.handle_connection(mut conn)
	}
}

// stop sends GOAWAY to all active connections and shuts down the server.
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

	upgrade_req := s.negotiate_protocol(mut conn) or {
		eprintln('[HTTP/2] Protocol negotiation error: ${err}')
		return
	}

	s.exchange_settings(mut conn) or {
		eprintln('[HTTP/2] Settings exchange error: ${err}')
		return
	}

	// RFC 7540 §3.2: The upgrade request becomes stream 1, half-closed (remote).
	if upgrade_req.stream_id > 0 {
		ctx.wg.add(1)
		spawn s.dispatch_stream(mut conn, upgrade_req, mut ctx)
	}

	highest_stream_id := s.run_frame_loop(mut conn, mut ctx)

	s.conn_mu.lock()
	if highest_stream_id > s.highest_stream_id {
		s.highest_stream_id = highest_stream_id
	}
	s.conn_mu.unlock()

	$if debug {
		eprintln('[HTTP/2] Connection closed')
	}
}

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

fn (mut s Server) exchange_settings(mut conn ServerConn) ! {
	s.write_settings(mut conn)!
}

fn (mut s Server) register_connection(conn ServerConn) {
	s.conn_mu.lock()
	s.connections << conn
	s.conn_mu.unlock()
}

// at_connection_limit checks whether the server has reached max_connections.
fn (mut s Server) at_connection_limit() bool {
	max := s.config.max_connections
	if max <= 0 {
		return false
	}
	s.conn_mu.lock()
	count := s.connections.len
	s.conn_mu.unlock()
	return count >= max
}

fn (mut s Server) deregister_connection(conn ServerConn) {
	s.conn_mu.lock()
	s.connections = s.connections.filter(it != conn)
	s.conn_mu.unlock()
}

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
