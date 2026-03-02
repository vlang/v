// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

import net
import time

// QUIC connection implementation using ngtcp2
// This replaces the placeholder implementation with a real QUIC connection

// Connection represents a QUIC connection using ngtcp2
pub struct Connection {
pub mut:
	remote_addr    string
	conn_id        []u8
	streams        map[u64]&Stream
	next_stream_id u64 = 1
	closed         bool
	// ngtcp2 connection handle
	ngtcp2_conn voidptr
	// UDP socket
	udp_socket net.UdpConn
	// Connection state
	handshake_done bool
	// Packet buffer
	send_buf []u8
	recv_buf []u8
	// TLS/crypto context
	crypto_ctx CryptoContext
	// Network path (persistent for the connection lifetime)
	path Ngtcp2PathStruct
	// Per-connection address storage; path.local.addr and path.remote.addr
	// point into this struct, so it must outlive path.
	path_addrs QuicPathAddrs
}

// Stream represents a QUIC stream
pub struct Stream {
pub mut:
	id     u64
	data   []u8
	closed bool
}

// ConnectionConfig holds QUIC connection configuration
pub struct ConnectionConfig {
pub:
	remote_addr string
	alpn        []string = ['h3']
	enable_0rtt bool
	// Connection limits
	max_stream_data_bidi_local  u64 = 1048576  // 1MB
	max_stream_data_bidi_remote u64 = 1048576  // 1MB
	max_stream_data_uni         u64 = 1048576  // 1MB
	max_data                    u64 = 10485760 // 10MB
	max_streams_bidi            u64 = 100
	max_streams_uni             u64 = 100
	max_idle_timeout            u64 = 30000 // 30 seconds (milliseconds)
}

// new_connection creates a new QUIC connection using ngtcp2 library
pub fn new_connection(config ConnectionConfig) !Connection {
	// Parse remote address
	addr_parts := config.remote_addr.split(':')
	if addr_parts.len != 2 {
		return error('invalid remote address format, expected host:port')
	}
	host := addr_parts[0]
	port := addr_parts[1].int()

	// Create UDP socket
	mut udp_socket := net.dial_udp('${host}:${port}') or {
		return error('failed to create UDP socket: ${err}')
	}

	// Generate connection IDs using crypto-quality random
	mut dcid := Ngtcp2CidStruct{
		datalen: 18
	}
	mut scid := Ngtcp2CidStruct{
		datalen: 18
	}
	C.RAND_bytes(&dcid.data[0], 18)
	C.RAND_bytes(&scid.data[0], 18)

	// Setup path with resolved remote address
	mut path := Ngtcp2PathStruct{}
	mut path_addrs := QuicPathAddrs{}
	rv := C.quic_resolve_and_set_path(&path, &path_addrs, &char(host.str), port)
	if rv != 0 {
		udp_socket.close() or {}
		return error('failed to resolve remote address: ${host}:${port}')
	}

	// Setup callbacks with ngtcp2_crypto helper implementations
	mut callbacks := Ngtcp2CallbacksStruct{}
	C.quic_init_callbacks(&callbacks)
	// Setup settings
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
	settings.initial_ts = u64(time.now().unix_milli()) * 1000000 // ms to ns

	// Setup transport parameters
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
	params.max_idle_timeout = config.max_idle_timeout * 1000000 // ms to ns

	// Create ngtcp2 connection (QUIC version 1, RFC 9000)
	quic_version := u32(0x00000001)

	ngtcp2_conn := conn_client_new(&dcid, &scid, &path, quic_version, &callbacks, &settings,
		&params, unsafe { nil }) or {
		udp_socket.close() or {}
		return error('failed to create ngtcp2 connection: ${err}')
	}

	// Create crypto context for TLS 1.3
	mut crypto_ctx := new_crypto_context_client(config.alpn) or {
		conn_del(ngtcp2_conn)
		udp_socket.close() or {}
		return error('failed to create crypto context: ${err}')
	}

	// Setup crypto integration (ossl_ctx, conn_ref, configure SSL)
	setup_crypto(ngtcp2_conn, voidptr(crypto_ctx.ssl), host) or {
		crypto_ctx.free()
		conn_del(ngtcp2_conn)
		udp_socket.close() or {}
		return error('failed to setup crypto: ${err}')
	}

	return Connection{
		remote_addr: config.remote_addr
		conn_id:     scid.data[0..int(scid.datalen)].clone()
		ngtcp2_conn: ngtcp2_conn
		udp_socket:  udp_socket
		send_buf:    []u8{len: 65536} // 64KB buffer
		recv_buf:    []u8{len: 65536}
		crypto_ctx:  crypto_ctx
		path:        path
		path_addrs:  path_addrs
	}
}

// send sends data on a QUIC stream with the given stream ID
pub fn (mut c Connection) send(stream_id u64, data []u8) ! {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	// Check if handshake is complete
	if !c.handshake_done {
		c.handshake_done = conn_get_handshake_completed(c.ngtcp2_conn)
		if !c.handshake_done {
			return error('handshake not completed')
		}
	}

	// Open stream if needed
	if stream_id !in c.streams {
		// Stream should already be opened
		return error('stream ${stream_id} not found')
	}

	// Write stream data using ngtcp2
	ts := u64(time.now().unix_milli()) * 1000000
	mut pi := Ngtcp2PktInfo{}

	nwritten, _ := conn_writev_stream(c.ngtcp2_conn, &c.path, &pi, c.send_buf, i64(stream_id),
		data, ts) or { return error('failed to write stream data: ${err}') }

	// Send packet via UDP
	if nwritten > 0 {
		c.udp_socket.write(c.send_buf[..nwritten]) or {
			return error('failed to send UDP packet: ${err}')
		}
	}

	// Update stream data
	mut stream := c.streams[stream_id] or { return error('stream not found') }
	stream.data << data
}

// recv receives data from a QUIC stream with the given stream ID
pub fn (mut c Connection) recv(stream_id u64) ![]u8 {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	// Read packet from UDP
	n, _ := c.udp_socket.read(mut c.recv_buf) or {
		return error('failed to read UDP packet: ${err}')
	}

	if n == 0 {
		return []u8{}
	}

	// Process packet with ngtcp2
	ts := u64(time.now().unix_milli()) * 1000000
	mut pi := Ngtcp2PktInfo{}

	conn_read_pkt(c.ngtcp2_conn, &c.path, &pi, c.recv_buf[..n], ts) or {
		// Non-fatal errors can be ignored
		if !err_is_fatal(ngtcp2_err_invalid_argument) {
			return []u8{}
		}
		return error('failed to read packet: ${err}')
	}

	// Return stream data
	stream := c.streams[stream_id] or { return error('stream not found') }
	return stream.data.clone()
}

// open_stream opens a new bidirectional QUIC stream and returns its ID
pub fn (mut c Connection) open_stream() !u64 {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	// Open bidirectional stream
	stream_id := conn_open_bidi_stream(c.ngtcp2_conn, unsafe { nil }) or {
		return error('failed to open stream: ${err}')
	}

	// Create stream object
	c.streams[u64(stream_id)] = &Stream{
		id: u64(stream_id)
	}

	return u64(stream_id)
}

// close_stream closes a QUIC stream with the given stream ID
pub fn (mut c Connection) close_stream(stream_id u64) ! {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	// Shutdown stream
	conn_shutdown_stream(c.ngtcp2_conn, i64(stream_id), 0) or {
		return error('failed to close stream: ${err}')
	}

	// Remove from map
	c.streams.delete(stream_id)
}

// close closes the QUIC connection and releases all resources
pub fn (mut c Connection) close() {
	if c.closed {
		return
	}

	c.closed = true

	// Close all streams
	c.streams.clear()

	// Delete ngtcp2 connection
	if c.ngtcp2_conn != unsafe { nil } {
		conn_del(c.ngtcp2_conn)
		c.ngtcp2_conn = unsafe { nil }
	}

	// Free crypto context
	c.crypto_ctx.free()

	// Close UDP socket
	c.udp_socket.close() or {}
}

// is_0rtt_available checks if 0-RTT (zero round-trip time) is available for early data
pub fn (c Connection) is_0rtt_available() bool {
	// 0-RTT requires session resumption data
	// This would check if we have valid session tickets
	return false
}

// migrate_connection migrates the QUIC connection to a new network path with the given address
pub fn (mut c Connection) migrate_connection(new_addr string) ! {
	if c.closed {
		return error('connection closed')
	}

	// Connection migration allows maintaining the connection
	// when the client's IP address changes (e.g., switching networks)
	c.remote_addr = new_addr

	// In a real implementation, this would:
	// 1. Send PATH_CHALLENGE frame
	// 2. Wait for PATH_RESPONSE
	// 3. Update connection state
}

// get_expiry returns the next timer expiry time for the connection in nanoseconds.
// Compare the returned value against time.sys_mono_now() to decide when to
// call handle_expiry.
pub fn get_expiry(conn &Connection) u64 {
	return conn_get_expiry(conn.ngtcp2_conn)
}

// handle_expiry notifies ngtcp2 that the connection timer has fired.
// ts is the current monotonic time in nanoseconds (time.sys_mono_now()).
// After calling this, invoke write_pkt or conn_write_pkt to flush any
// retransmission packets that ngtcp2 has queued as a result of the expiry.
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

// check_and_handle_timers checks whether the connection timer has expired and,
// if so, calls handle_expiry to process it.  After this returns without error,
// the caller should send any pending packets via write_pkt so that ngtcp2
// retransmissions and keep-alives are delivered to the peer.
// Returns true when handle_expiry was invoked (timer fired), false otherwise.
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
