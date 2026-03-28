module quic

// QUIC connection I/O operations: send, recv, and stream management.

// process_stream_fin_events drains pending FIN events from QuicStreamEvents
// and sets fin_received on matching streams.
fn process_stream_fin_events(mut events QuicStreamEvents, mut streams map[u64]&Stream) {
	for i in 0 .. events.fin_count {
		sid := u64(events.fin_stream_ids[i])
		if mut s := streams[sid] {
			s.fin_received = true
		} else {
			streams[sid] = &Stream{
				id: sid
				fin_received: true
			}
		}
	}
	events.fin_count = 0
}

// process_stream_close_events drains pending close events from QuicStreamEvents
// and sets closed on matching streams.
fn process_stream_close_events(mut events QuicStreamEvents, mut streams map[u64]&Stream) {
	for i in 0 .. events.closed_count {
		sid := u64(events.closed_stream_ids[i])
		if mut s := streams[sid] {
			s.closed = true
		} else {
			streams[sid] = &Stream{
				id: sid
				closed: true
			}
		}
	}
	events.closed_count = 0
}

// drain_stream_events processes pending FIN and close events recorded by
// ngtcp2 C callbacks. Call this after conn_read_pkt to propagate stream
// state changes to the V-side Stream objects.
// If the event buffer overflowed (>64 events between drains), a warning
// is logged, the overflow flag is cleared, and an error is returned so
// callers can treat overflow as a connection-level error.
pub fn (mut c Connection) drain_stream_events() ! {
	if c.stream_events != unsafe { nil } {
		// Capture FIN stream IDs before process_stream_fin_events clears fin_count.
		// Callers use pending_fin_streams for targeted completion checks.
		for i in 0 .. c.stream_events.fin_count {
			c.pending_fin_streams << u64(c.stream_events.fin_stream_ids[i])
		}
		process_stream_fin_events(mut c.stream_events, mut c.streams)
		process_stream_close_events(mut c.stream_events, mut c.streams)
		if c.stream_events.overflow != 0 {
			eprintln('WARN: QUIC stream event buffer overflow — events were dropped')
			c.stream_events.overflow = 0
			return error('stream event buffer overflow: events were dropped, connection should be reset')
		}
	}
}

// process_incoming_packet feeds a raw QUIC packet into the ngtcp2 stack for
// connection state tracking and callback processing (FIN detection, stream
// close). After ngtcp2 processes the packet, pending stream events are drained.
// Errors from ngtcp2 and stream event overflow are propagated to the caller.
pub fn (mut c Connection) process_incoming_packet(packet []u8) ! {
	if c.ngtcp2_conn == unsafe { nil } {
		return
	}
	ts := ngtcp2_timestamp()
	mut pi := Ngtcp2PktInfo{}
	conn_read_pkt(c.ngtcp2_conn, &c.path, &pi, packet, ts) or {
		return error('packet processing failed: ${err}')
	}
	c.drain_stream_events()!
}

// ensure_stream creates a QUIC stream entry if it doesn't exist.
// Returns the stream, creating it if necessary.
pub fn (mut c Connection) ensure_stream(stream_id u64) &Stream {
	if s := c.streams[stream_id] {
		return s
	}
	s := &Stream{id: stream_id}
	c.streams[stream_id] = s
	return s
}

// stream_has_fin returns whether the given stream has received a FIN.
pub fn (c &Connection) stream_has_fin(stream_id u64) bool {
	if s := c.streams[stream_id] {
		return s.fin_received
	}
	return false
}

// stream_exists returns whether the stream is registered.
pub fn (c &Connection) stream_exists(stream_id u64) bool {
	return stream_id in c.streams
}

// send sends data on a QUIC stream.
pub fn (mut c Connection) send(stream_id u64, data []u8) ! {
	c.ensure_open()!
	c.ensure_conn()!
	if data.len > 0 {
		max_data := C.ngtcp2_conn_get_max_data_left(c.ngtcp2_conn)
		if max_data == 0 {
			return error('flow control: no data window available')
		}
	}
	c.send_with_flags(stream_id, data, ngtcp2_write_stream_flag_none)!
}

// send_with_fin sends data on a QUIC stream with the FIN flag set,
// signaling that this is the last data on the stream.
pub fn (mut c Connection) send_with_fin(stream_id u64, data []u8) ![]u8 {
	nwritten := c.send_with_flags(stream_id, data, ngtcp2_write_stream_flag_fin)!
	return c.send_buf[..nwritten].clone()
}

// send_with_flags is the internal helper that contains the shared write logic
// for both send() and send_with_fin(). It validates connection state, writes
// stream data with the given flags, sends the UDP packet, and appends data
// to the stream buffer. Returns the number of bytes written to the packet.
fn (mut c Connection) send_with_flags(stream_id u64, data []u8, flags u32) !int {
	c.ensure_open()!
	c.ensure_conn()!

	if !c.handshake_done {
		c.handshake_done = conn_get_handshake_completed(c.ngtcp2_conn)
		if !c.handshake_done {
			return error('handshake not completed')
		}
	}

	if stream_id !in c.streams {
		return error('stream ${stream_id} not found')
	}

	ts := ngtcp2_timestamp()
	mut pi := Ngtcp2PktInfo{}

	nwritten, _ := conn_writev_stream(c.ngtcp2_conn, &c.path, &pi, c.send_buf, i64(stream_id),
		data, ts, flags) or { return error('failed to write stream data: ${err}') }

	if nwritten > 0 {
		c.udp_socket.write(c.send_buf[..nwritten]) or {
			return error('failed to send UDP packet: ${err}')
		}
	}

	mut stream := c.streams[stream_id] or { return error('stream not found') }
	stream.data << data

	return nwritten
}

// send_fin sends a FIN (end-of-stream) signal on a QUIC stream without data.
pub fn (mut c Connection) send_fin(stream_id u64) ![]u8 {
	return c.send_with_fin(stream_id, []u8{})
}
pub fn (mut c Connection) recv(stream_id u64) ![]u8 {
	c.ensure_open()!
	c.ensure_conn()!

	n, _ := c.udp_socket.read(mut c.recv_buf) or {
		return error('failed to read UDP packet: ${err}')
	}

	if n == 0 {
		return []u8{}
	}

	ts := ngtcp2_timestamp()
	mut pi := Ngtcp2PktInfo{}

	conn_read_pkt(c.ngtcp2_conn, &c.path, &pi, c.recv_buf[..n], ts) or {
		if !err_is_fatal(err.code()) {
			return []u8{}
		}
		return error('failed to read packet: ${err}')
	}

	c.idle_monitor.record_activity()

	c.drain_stream_events()!

	stream := c.streams[stream_id] or { return error('stream not found') }
	return stream.data.clone()
}

// open_stream opens a new bidirectional QUIC stream and returns its ID.
pub fn (mut c Connection) open_stream() !u64 {
	c.ensure_open()!
	c.ensure_conn()!

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
	c.ensure_open()!
	c.ensure_conn()!

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
	c.ensure_open()!
	c.ensure_conn()!

	conn_shutdown_stream(c.ngtcp2_conn, i64(stream_id), 0) or {
		return error('failed to close stream: ${err}')
	}

	c.streams.delete(stream_id)
}

// close_with_error sends a CONNECTION_CLOSE frame with the given error code and
// reason, then closes the connection and releases all resources.
pub fn (mut c Connection) close_with_error(error_code u64, reason string) ! {
	if c.closed {
		return
	}
	c.closed = true

	// Best-effort CONNECTION_CLOSE frame (RFC 9000 §10.2)
	if c.ngtcp2_conn != unsafe { nil } {
		mut buf := []u8{len: 1200}
		ts := ngtcp2_timestamp()
		bytes := conn_write_connection_close(c.ngtcp2_conn, buf, error_code, reason, ts) or { 0 }
		if bytes > 0 {
			c.udp_socket.write(buf[..bytes]) or {}
		}
	}

	c.streams.clear()

	if c.ngtcp2_conn != unsafe { nil } {
		conn_del(c.ngtcp2_conn)
		c.ngtcp2_conn = unsafe { nil }
	}

	c.crypto_ctx.free()

	c.udp_socket.close() or {}
}

// close closes the QUIC connection and releases all resources.
pub fn (mut c Connection) close() {
	c.close_with_error(0, '') or {}
}

// max_data_left returns the number of bytes the connection is allowed to send.
// Returns 0 if the connection handle is not initialized.
pub fn (c &Connection) max_data_left() u64 {
	if c.ngtcp2_conn == unsafe { nil } {
		return 0
	}
	return C.ngtcp2_conn_get_max_data_left(c.ngtcp2_conn)
}

// streams_bidi_left returns the number of bidirectional streams the peer allows to open.
// Returns 0 if the connection handle is not initialized.
pub fn (c &Connection) streams_bidi_left() u64 {
	if c.ngtcp2_conn == unsafe { nil } {
		return 0
	}
	return C.ngtcp2_conn_get_streams_bidi_left(c.ngtcp2_conn)
}

// streams_uni_left returns the number of unidirectional streams the peer allows to open.
// Returns 0 if the connection handle is not initialized.
pub fn (c &Connection) streams_uni_left() u64 {
	if c.ngtcp2_conn == unsafe { nil } {
		return 0
	}
	return C.ngtcp2_conn_get_streams_uni_left(c.ngtcp2_conn)
}

// reset_stream sends a RESET_STREAM frame for the given stream.
pub fn (mut c Connection) reset_stream(stream_id u64, app_error_code u64) ! {
	c.ensure_open()!
	c.ensure_conn()!
	rv := shutdown_stream_write(c.ngtcp2_conn, 0, i64(stream_id), app_error_code)
	if rv < 0 {
		return error('reset_stream failed: ${strerror(rv)}')
	}
	shutdown_stream_read(c.ngtcp2_conn, 0, i64(stream_id), app_error_code)
	c.streams.delete(stream_id)
}

// stop_sending sends a STOP_SENDING frame for the given stream.
pub fn (mut c Connection) stop_sending(stream_id u64, app_error_code u64) ! {
	c.ensure_open()!
	c.ensure_conn()!
	rv := shutdown_stream_read(c.ngtcp2_conn, 0, i64(stream_id), app_error_code)
	if rv < 0 {
		return error('stop_sending failed: ${strerror(rv)}')
	}
}
