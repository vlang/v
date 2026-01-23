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

// new_connection creates a new QUIC connection using ngtcp2
pub fn new_connection(config ConnectionConfig) !Connection {
	// Check if ngtcp2 is available
	version := get_version()
	$if debug {
		eprintln('ngtcp2 version: ${unsafe { cstring_to_vstring(version.version_str) }}')
	}

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

	// Generate connection IDs
	mut dcid := Ngtcp2CidStruct{
		datalen: 18
	}
	mut scid := Ngtcp2CidStruct{
		datalen: 18
	}

	// Fill with random data (simplified - should use crypto random)
	for i in 0 .. 18 {
		dcid.data[i] = u8(time.now().unix() % 256)
		scid.data[i] = u8((time.now().unix() + i64(i)) % 256)
	}

	// Setup path
	mut path := Ngtcp2PathStruct{}

	// Setup callbacks (simplified - would need proper callback implementations)
	mut callbacks := Ngtcp2CallbacksStruct{}

	// Setup settings
	mut settings := Ngtcp2SettingsStruct{}
	settings_default(&settings)
	settings.initial_max_stream_data_bidi_local = config.max_stream_data_bidi_local
	settings.initial_max_stream_data_bidi_remote = config.max_stream_data_bidi_remote
	settings.initial_max_stream_data_uni = config.max_stream_data_uni
	settings.initial_max_data = config.max_data
	settings.initial_max_streams_bidi = config.max_streams_bidi
	settings.initial_max_streams_uni = config.max_streams_uni
	settings.max_idle_timeout = config.max_idle_timeout

	// Setup transport parameters
	mut params := Ngtcp2TransportParamsStruct{}
	transport_params_default(&params)
	params.initial_max_stream_data_bidi_local = config.max_stream_data_bidi_local
	params.initial_max_stream_data_bidi_remote = config.max_stream_data_bidi_remote
	params.initial_max_stream_data_uni = config.max_stream_data_uni
	params.initial_max_data = config.max_data
	params.initial_max_streams_bidi = config.max_streams_bidi
	params.initial_max_streams_uni = config.max_streams_uni
	params.max_idle_timeout = config.max_idle_timeout

	// Create ngtcp2 connection
	// QUIC version 1 (RFC 9000)
	quic_version := u32(0x00000001)

	ngtcp2_conn := conn_client_new(&dcid, &scid, &path, quic_version, &callbacks, &settings,
		&params, unsafe { nil }) or {
		udp_socket.close() or {}
		return error('failed to create ngtcp2 connection: ${err}')
	}

	return Connection{
		remote_addr: config.remote_addr
		conn_id:     []u8{len: int(scid.datalen), init: scid.data[index]}
		ngtcp2_conn: ngtcp2_conn
		udp_socket:  udp_socket
		send_buf:    []u8{len: 65536} // 64KB buffer
		recv_buf:    []u8{len: 65536}
	}
}

// send sends data on a QUIC stream
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
	ts := u64(time.now().unix_milli())
	mut path := Ngtcp2PathStruct{}
	mut pi := Ngtcp2PktInfo{}

	nwritten, datalen := conn_writev_stream(c.ngtcp2_conn, &path, &pi, c.send_buf, i64(stream_id),
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

// recv receives data from a QUIC stream
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
	ts := u64(time.now().unix_milli())
	mut path := Ngtcp2PathStruct{}
	mut pi := Ngtcp2PktInfo{}

	conn_read_pkt(c.ngtcp2_conn, &path, &pi, c.recv_buf[..n], ts) or {
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

// open_stream opens a new bidirectional stream
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

// close_stream closes a QUIC stream
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

// close closes the QUIC connection
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

	// Close UDP socket
	c.udp_socket.close() or {}
}

// is_0rtt_available checks if 0-RTT is available
pub fn (c Connection) is_0rtt_available() bool {
	// 0-RTT requires session resumption data
	// This would check if we have valid session tickets
	// For now, return false until we implement session resumption
	return false
}

// migrate_connection migrates the connection to a new network path
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
	// For now, just update the address
}
