module quic

// QUIC connection I/O operations: send, recv, stream management, and migration.
import net
import time

// send sends data on a QUIC stream.
pub fn (mut c Connection) send(stream_id u64, data []u8) ! {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	if !c.handshake_done {
		c.handshake_done = conn_get_handshake_completed(c.ngtcp2_conn)
		if !c.handshake_done {
			return error('handshake not completed')
		}
	}

	if stream_id !in c.streams {
		return error('stream ${stream_id} not found')
	}

	ts := u64(time.now().unix_milli()) * 1000000
	mut pi := Ngtcp2PktInfo{}

	nwritten, _ := conn_writev_stream(c.ngtcp2_conn, &c.path, &pi, c.send_buf, i64(stream_id),
		data, ts) or { return error('failed to write stream data: ${err}') }

	if nwritten > 0 {
		c.udp_socket.write(c.send_buf[..nwritten]) or {
			return error('failed to send UDP packet: ${err}')
		}
	}

	mut stream := c.streams[stream_id] or { return error('stream not found') }
	stream.data << data
}

// recv receives data from a QUIC stream.
pub fn (mut c Connection) recv(stream_id u64) ![]u8 {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	n, _ := c.udp_socket.read(mut c.recv_buf) or {
		return error('failed to read UDP packet: ${err}')
	}

	if n == 0 {
		return []u8{}
	}

	ts := u64(time.now().unix_milli()) * 1000000
	mut pi := Ngtcp2PktInfo{}

	conn_read_pkt(c.ngtcp2_conn, &c.path, &pi, c.recv_buf[..n], ts) or {
		if !err_is_fatal(ngtcp2_err_invalid_argument) {
			return []u8{}
		}
		return error('failed to read packet: ${err}')
	}

	stream := c.streams[stream_id] or { return error('stream not found') }
	return stream.data.clone()
}

// open_stream opens a new bidirectional QUIC stream and returns its ID.
pub fn (mut c Connection) open_stream() !u64 {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	stream_id := conn_open_bidi_stream(c.ngtcp2_conn, unsafe { nil }) or {
		return error('failed to open stream: ${err}')
	}

	c.streams[u64(stream_id)] = &Stream{
		id: u64(stream_id)
	}

	return u64(stream_id)
}

// open_uni_stream opens a new unidirectional QUIC stream and returns its ID.
pub fn (mut c Connection) open_uni_stream() !i64 {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	stream_id := conn_open_uni_stream(c.ngtcp2_conn, unsafe { nil }) or {
		return error('failed to open unidirectional stream: ${err}')
	}

	c.streams[u64(stream_id)] = &Stream{
		id: u64(stream_id)
	}

	return stream_id
}

// close_stream closes a QUIC stream.
pub fn (mut c Connection) close_stream(stream_id u64) ! {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	conn_shutdown_stream(c.ngtcp2_conn, i64(stream_id), 0) or {
		return error('failed to close stream: ${err}')
	}

	c.streams.delete(stream_id)
}

// close closes the QUIC connection and releases all resources.
pub fn (mut c Connection) close() {
	if c.closed {
		return
	}

	c.closed = true

	c.streams.clear()

	if c.ngtcp2_conn != unsafe { nil } {
		conn_del(c.ngtcp2_conn)
		c.ngtcp2_conn = unsafe { nil }
	}

	c.crypto_ctx.free()

	c.udp_socket.close() or {}
}

// is_0rtt_available checks if 0-RTT is available for early data.
pub fn (c &Connection) is_0rtt_available() bool {
	return c.zero_rtt.state == .accepted
}

// migrate_connection migrates the QUIC connection to a new network path.
pub fn (mut c Connection) migrate_connection(new_addr string) ! {
	if c.closed {
		return error('connection closed')
	}

	addr_parts := new_addr.split(':')
	if addr_parts.len != 2 {
		return error('invalid address format, expected host:port')
	}
	host := addr_parts[0]

	new_remote_addrs := net.resolve_addrs(host, .ip, .udp) or {
		return error('failed to resolve address: ${err}')
	}
	if new_remote_addrs.len == 0 {
		return error('no addresses resolved for ${host}')
	}

	local_addr := c.migration.current_path.local_addr
	c.migration.probe_path(local_addr, new_remote_addrs[0]) or {
		return error('failed to probe new path: ${err}')
	}
}

// complete_migration completes a pending migration after receiving a PATH_RESPONSE.
pub fn (mut c Connection) complete_migration(response PathResponse) ! {
	if c.closed {
		return error('connection closed')
	}

	if c.migration.alternative_paths.len == 0 {
		return error('no pending migration')
	}

	last_path := c.migration.alternative_paths.last()
	validated := c.migration.validate_path(last_path, response) or {
		return error('path validation failed: ${err}')
	}

	if !validated {
		return error('path response does not match challenge')
	}

	for p in c.migration.alternative_paths {
		if p.validated {
			c.migration.migrate_to_path(p) or { return error('failed to migrate to path: ${err}') }
			c.remote_addr = c.migration.current_path.remote_addr.str()
			// NOTE: c.path (Ngtcp2PathStruct) and c.udp_socket should also be
			// updated for a complete migration with ngtcp2. This requires calling
			// C.quic_resolve_and_set_path and reconnecting the UDP socket.
			return
		}
	}

	return error('no validated path found after validation')
}

// check_path_degradation checks if the current network path has degraded.
pub fn (c &Connection) check_path_degradation() bool {
	return c.migration.detect_path_degradation(0.0, c.migration.current_path.rtt)
}

// save_session_ticket stores a session ticket in the shared session cache.
pub fn (mut c Connection) save_session_ticket(ticket SessionTicket) {
	if c.session_cache != unsafe { nil } {
		mut sc := c.session_cache
		sc.store(ticket.server_name, ticket)
	}
}

// send_early_data sends data as 0-RTT early data on the given stream.
pub fn (mut c Connection) send_early_data(stream_id u64, data []u8) ! {
	if c.closed {
		return error('connection closed')
	}

	if !c.zero_rtt.can_send_early_data() {
		return error('cannot send early data in current state')
	}

	c.zero_rtt.add_early_data(data, stream_id) or {
		return error('failed to buffer early data: ${err}')
	}

	// NOTE: In a full implementation, this would use ngtcp2's early data write
	// path (conn_writev_stream with early data flags) to send the data
	// immediately as part of the initial flight.
}
