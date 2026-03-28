module quic

// QUIC TLS 1.3 handshake using ngtcp2 crypto callbacks.
import time

// HandshakeState represents the state of the QUIC handshake.
pub enum HandshakeState {
	initial
	handshake_in_progress
	handshake_complete
	failed
}

// perform_handshake performs the client-side QUIC TLS 1.3 handshake.
pub fn (mut c Connection) perform_handshake() ! {
	c.ensure_open()!
	c.ensure_conn()!

	$if trace_quic ? {
		eprintln('[QUIC] Starting client handshake...')
	}

	max_attempts := 50
	for attempt := 0; attempt < max_attempts; attempt++ {
		ts := ngtcp2_timestamp()
		mut pi := Ngtcp2PktInfo{}

		c.send_handshake_packet(ts, mut pi)!

		if conn_get_handshake_completed(c.ngtcp2_conn) {
			c.handshake_done = true
			c.finalize_zero_rtt()
			$if trace_quic ? {
				eprintln('[QUIC] Client handshake complete')
			}
			return
		}

		c.recv_handshake_packet(ts, mut pi, 'client')!

		if conn_get_handshake_completed(c.ngtcp2_conn) {
			c.handshake_done = true
			c.finalize_zero_rtt()
			$if trace_quic ? {
				eprintln('[QUIC] Client handshake complete')
			}
			return
		}
	}

	return error('handshake timeout after ${max_attempts} attempts')
}

// perform_handshake_server performs the server-side QUIC handshake.
pub fn (mut c Connection) perform_handshake_server(cert_file string, key_file string) ! {
	c.ensure_open()!
	c.ensure_conn()!

	$if trace_quic ? {
		eprintln('[QUIC] Starting server handshake...')
	}

	max_attempts := 50
	for attempt := 0; attempt < max_attempts; attempt++ {
		ts := ngtcp2_timestamp()
		mut pi := Ngtcp2PktInfo{}

		c.recv_handshake_packet(ts, mut pi, 'server')!

		c.send_handshake_packet(ts, mut pi) or { continue }

		if conn_get_handshake_completed(c.ngtcp2_conn) {
			c.handshake_done = true
			$if trace_quic ? {
				eprintln('[QUIC] Server handshake complete')
			}
			return
		}
	}

	return error('server handshake timeout')
}

fn (mut c Connection) send_handshake_packet(ts u64, mut pi Ngtcp2PktInfo) ! {
	nwritten := conn_write_pkt(c.ngtcp2_conn, &c.path, &pi, c.send_buf, ts) or {
		return error('failed to write handshake packet: ${err}')
	}

	if nwritten > 0 {
		c.udp_socket.write(c.send_buf[..nwritten]) or {
			return error('failed to send handshake packet: ${err}')
		}
	}
}

// finalize_zero_rtt handles 0-RTT state after handshake completion.
// Marks 0-RTT as rejected if still attempting, extracts the session ticket
// for future 0-RTT resumption (RFC 9001 §8), then flushes accepted early data.
fn (mut c Connection) finalize_zero_rtt() {
	if c.zero_rtt.state == .attempting {
		c.zero_rtt.reject()
	}
	if ticket := c.crypto_ctx.extract_session_ticket(c.remote_addr) {
		c.save_session_ticket(ticket)
	}
	c.flush_early_data() or {}
}

fn (mut c Connection) recv_handshake_packet(ts u64, mut pi Ngtcp2PktInfo, role string) ! {
	c.udp_socket.set_read_timeout(2 * time.second)
	n, _ := c.udp_socket.read(mut c.recv_buf) or {
		if err.msg().contains('timed out') || err.msg().contains('timeout') {
			return
		}
		return error('failed to read packet: ${err}')
	}

	if n == 0 {
		return
	}

	conn_read_pkt(c.ngtcp2_conn, &c.path, &pi, c.recv_buf[..n], ts) or {
		err_str := err.msg()
		if err_str.contains('DISCARD_PKT') || err_str.contains('discard') {
			$if trace_quic ? {
				eprintln('[QUIC] discarded packet during ${role} handshake: ${err_str}')
			}
			return
		}
		return error('${role} handshake read error: ${err}')
	}
}

// send_with_crypto sends data with encryption via ngtcp2 callbacks.
pub fn (mut c Connection) send_with_crypto(stream_id u64, data []u8, crypto_ctx &CryptoContext) ! {
	c.ensure_open()!

	if !c.handshake_done {
		return error('handshake not completed')
	}

	ts := ngtcp2_timestamp()
	mut pi := Ngtcp2PktInfo{}

	nwritten, _ := conn_writev_stream(c.ngtcp2_conn, &c.path, &pi, c.send_buf, i64(stream_id),
		data, ts, ngtcp2_write_stream_flag_none) or { return error('failed to write stream data: ${err}') }

	if nwritten > 0 {
		c.udp_socket.write(c.send_buf[..nwritten]) or {
			return error('failed to send packet: ${err}')
		}
	}

	mut stream := c.streams[stream_id] or { return error('stream not found') }
	stream.data << data
}

// recv_with_crypto receives data with decryption via ngtcp2 callbacks.
pub fn (mut c Connection) recv_with_crypto(stream_id u64, crypto_ctx &CryptoContext) ![]u8 {
	c.ensure_open()!

	if !c.handshake_done {
		return error('handshake not completed')
	}

	n, _ := c.udp_socket.read(mut c.recv_buf) or { return error('failed to read packet: ${err}') }

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

	stream := c.streams[stream_id] or { return error('stream not found') }
	return stream.data.clone()
}
