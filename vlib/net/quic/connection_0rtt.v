module quic

// QUIC 0-RTT early data and session ticket operations.

// is_0rtt_available checks if 0-RTT is available for early data.
pub fn (c &Connection) is_0rtt_available() bool {
	return c.zero_rtt.state == .accepted
}

// save_session_ticket stores a session ticket in the shared session cache.
pub fn (mut c Connection) save_session_ticket(ticket SessionTicket) {
	if c.session_cache != unsafe { nil } {
		mut sc := c.session_cache
		sc.store(ticket.server_name, ticket)
	}
}

// send_early_data sends data as 0-RTT early data on the given stream.
// It tries an immediate send via the normal path because ngtcp2
// dispatches to 0-RTT or 1-RTT packets based on handshake state.
// If the send fails (e.g. handshake not ready), data is buffered
// for later flush after handshake completion (RFC 9000 §4.1).
pub fn (mut c Connection) send_early_data(stream_id u64, data []u8) ! {
	c.ensure_open()!

	if c.zero_rtt.state != .accepted && c.zero_rtt.state != .attempting {
		return error('0-RTT not available')
	}

	// Try immediate send via normal path (ngtcp2 handles 0-RTT transparently)
	c.send(stream_id, data) or {
		// Send failed (handshake not ready); buffer for later flush
		c.zero_rtt.add_early_data(data, stream_id) or {
			return error('failed to buffer early data: ${err}')
		}
		return
	}
	c.zero_rtt.bytes_sent += u32(data.len)
}

// flush_early_data sends any buffered 0-RTT early data via the normal send path.
// Called after handshake completes and 0-RTT is accepted (RFC 9000 §4.1).
pub fn (mut c Connection) flush_early_data() ! {
	c.ensure_open()!
	if c.zero_rtt.state != .accepted {
		return
	}
	early_data := c.zero_rtt.get_early_data()
	for ed in early_data {
		c.send(ed.stream_id, ed.data) or {
			// Non-fatal: early data send failed, will be retried via normal path
		}
	}
}
