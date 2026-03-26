module quic

// QUIC connection management and configuration using ngtcp2.
import net
import time

// Connection represents a QUIC connection backed by ngtcp2.
pub struct Connection {
pub mut:
	remote_addr    string
	conn_id        []u8
	streams        map[u64]&Stream
	next_stream_id u64 = 1
	closed         bool
	ngtcp2_conn    voidptr
	udp_socket     net.UdpConn
	handshake_done bool
	send_buf       []u8
	recv_buf       []u8
	crypto_ctx     CryptoContext
	path           Ngtcp2PathStruct
	path_addrs     QuicPathAddrs
	migration      ConnectionMigration
	zero_rtt       ZeroRTTConnection
	session_cache  &SessionCache = unsafe { nil }
}

// Stream represents a QUIC stream.
pub struct Stream {
pub mut:
	id     u64
	data   []u8
	closed bool
}

// ConnectionConfig holds QUIC connection configuration.
pub struct ConnectionConfig {
pub:
	remote_addr                 string
	alpn                        []string = ['h3']
	enable_0rtt                 bool
	session_cache               &SessionCache = unsafe { nil }
	max_stream_data_bidi_local  u64           = 1048576
	max_stream_data_bidi_remote u64           = 1048576
	max_stream_data_uni         u64           = 1048576
	max_data                    u64           = 10485760
	max_streams_bidi            u64           = 100
	max_streams_uni             u64           = 100
	max_idle_timeout            u64           = 30000
}

// new_connection creates a new QUIC client connection using ngtcp2.
pub fn new_connection(config ConnectionConfig) !Connection {
	addr_parts := config.remote_addr.split(':')
	if addr_parts.len != 2 {
		return error('invalid remote address format, expected host:port')
	}
	host := addr_parts[0]
	port := addr_parts[1].int()

	mut ngtcp2_setup := setup_ngtcp2(host, port, config)!

	mut crypto_ctx := new_crypto_context_client(config.alpn) or {
		conn_del(ngtcp2_setup.ngtcp2_conn)
		ngtcp2_setup.udp_socket.close() or {}
		return error('failed to create crypto context: ${err}')
	}

	setup_crypto(ngtcp2_setup.ngtcp2_conn, voidptr(crypto_ctx.ssl), host) or {
		crypto_ctx.free()
		conn_del(ngtcp2_setup.ngtcp2_conn)
		ngtcp2_setup.udp_socket.close() or {}
		return error('failed to setup crypto: ${err}')
	}

	return Connection{
		remote_addr:   config.remote_addr
		conn_id:       ngtcp2_setup.conn_id
		ngtcp2_conn:   ngtcp2_setup.ngtcp2_conn
		udp_socket:    ngtcp2_setup.udp_socket
		send_buf:      []u8{len: 65536}
		recv_buf:      []u8{len: 65536}
		crypto_ctx:    crypto_ctx
		path:          ngtcp2_setup.path
		path_addrs:    ngtcp2_setup.path_addrs
		migration:     init_migration_subsystem(host)
		zero_rtt:      init_zero_rtt_subsystem(config, host)
		session_cache: config.session_cache
	}
}

struct Ngtcp2ConnectionSetup {
pub mut:
	ngtcp2_conn voidptr
	path        Ngtcp2PathStruct
	path_addrs  QuicPathAddrs
	udp_socket  net.UdpConn
	conn_id     []u8
}

fn setup_ngtcp2(host string, port int, config ConnectionConfig) !Ngtcp2ConnectionSetup {
	mut udp_socket := net.dial_udp('${host}:${port}') or {
		return error('failed to create UDP socket: ${err}')
	}

	mut dcid := Ngtcp2CidStruct{
		datalen: 18
	}
	mut scid := Ngtcp2CidStruct{
		datalen: 18
	}
	if C.RAND_bytes(&dcid.data[0], 18) != 1 {
		udp_socket.close() or {}
		return error('failed to generate random DCID: RNG failure')
	}
	if C.RAND_bytes(&scid.data[0], 18) != 1 {
		udp_socket.close() or {}
		return error('failed to generate random SCID: RNG failure')
	}

	mut path := Ngtcp2PathStruct{}
	mut path_addrs := QuicPathAddrs{}
	rv := C.quic_resolve_and_set_path(&path, &path_addrs, &char(host.str), port)
	if rv != 0 {
		udp_socket.close() or {}
		return error('failed to resolve remote address: ${host}:${port}')
	}

	mut callbacks := Ngtcp2CallbacksStruct{}
	C.quic_init_callbacks(&callbacks)

	settings := configure_ngtcp2_settings()
	params := configure_transport_params(config)
	quic_version := u32(0x00000001)

	ngtcp2_conn := conn_client_new(&dcid, &scid, &path, quic_version, &callbacks, &settings,
		&params, unsafe { nil }) or {
		udp_socket.close() or {}
		return error('failed to create ngtcp2 connection: ${err}')
	}

	return Ngtcp2ConnectionSetup{
		ngtcp2_conn: ngtcp2_conn
		path:        path
		path_addrs:  path_addrs
		udp_socket:  udp_socket
		conn_id:     scid.data[0..int(scid.datalen)].clone()
	}
}

fn configure_ngtcp2_settings() Ngtcp2SettingsStruct {
	mut settings := Ngtcp2SettingsStruct{
		qlog_write:         unsafe { nil }
		log_printf:         unsafe { nil }
		token:              unsafe { nil }
		rand_ctx:           unsafe { nil }
		preferred_versions: unsafe { nil }
		available_versions: unsafe { nil }
		pmtud_probes:       unsafe { nil }
	}
	settings_default(&settings)
	settings.initial_ts = u64(time.now().unix_milli()) * 1000000
	return settings
}

fn configure_transport_params(config ConnectionConfig) Ngtcp2TransportParamsStruct {
	mut params := Ngtcp2TransportParamsStruct{
		version_info: Ngtcp2VersionInfo{
			available_versions: unsafe { nil }
		}
	}
	transport_params_default(&params)
	params.initial_max_stream_data_bidi_local = config.max_stream_data_bidi_local
	params.initial_max_stream_data_bidi_remote = config.max_stream_data_bidi_remote
	params.initial_max_stream_data_uni = config.max_stream_data_uni
	params.initial_max_data = config.max_data
	params.initial_max_streams_bidi = config.max_streams_bidi
	params.initial_max_streams_uni = config.max_streams_uni
	params.max_idle_timeout = config.max_idle_timeout * 1000000
	return params
}

fn init_migration_subsystem(host string) ConnectionMigration {
	mig_local := net.resolve_addrs('0.0.0.0', .ip, .udp) or { []net.Addr{} }
	mig_remote := net.resolve_addrs(host, .ip, .udp) or { []net.Addr{} }
	if mig_local.len > 0 && mig_remote.len > 0 {
		return new_connection_migration(mig_local[0], mig_remote[0])
	}
	return ConnectionMigration{}
}

fn init_zero_rtt_subsystem(config ConnectionConfig, host string) ZeroRTTConnection {
	if config.enable_0rtt && config.session_cache != unsafe { nil } {
		mut sc := config.session_cache
		if ticket := sc.get(host) {
			mut zero_rtt_conn := new_zero_rtt_connection(ZeroRTTConfig{
				enabled:        true
				max_early_data: ticket.max_early_data
			})
			zero_rtt_conn.ticket = ticket
			return zero_rtt_conn
		}
	}
	return ZeroRTTConnection{}
}

// get_expiry returns the next timer expiry time for the connection in nanoseconds.
pub fn get_expiry(conn &Connection) u64 {
	return conn_get_expiry(conn.ngtcp2_conn)
}

// handle_expiry notifies ngtcp2 that the connection timer has fired.
pub fn handle_expiry(mut conn Connection) ! {
	if conn.closed {
		return error('connection closed')
	}
	if conn.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}
	ts := time.sys_mono_now()
	conn_handle_expiry(conn.ngtcp2_conn, ts)!
}

// check_and_handle_timers checks whether the timer has expired and processes it. Returns true if the timer fired.
pub fn check_and_handle_timers(mut conn Connection) !bool {
	if conn.closed {
		return error('connection closed')
	}
	if conn.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}
	now := time.sys_mono_now()
	expiry := get_expiry(&conn)
	if now >= expiry {
		handle_expiry(mut conn)!
		return true
	}
	return false
}
