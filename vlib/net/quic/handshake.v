// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

import time

// QUIC Handshake Implementation
// Uses ngtcp2's built-in TLS 1.3 handshake via crypto callbacks.
// The ngtcp2_crypto_ossl callbacks handle the actual TLS operations.

// HandshakeState represents the state of the QUIC handshake
pub enum HandshakeState {
	initial
	handshake_in_progress
	handshake_complete
	failed
}

// perform_handshake performs the QUIC handshake using ngtcp2 callbacks.
// The crypto callbacks (client_initial, recv_crypto_data, etc.) drive
// the TLS 1.3 handshake automatically through ngtcp2_conn_write_pkt
// and ngtcp2_conn_read_pkt.
pub fn (mut c Connection) perform_handshake() ! {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	println('Starting QUIC handshake...')

	// Handshake loop: write initial packet, then read/write until complete
	max_attempts := 50
	for attempt := 0; attempt < max_attempts; attempt++ {
		ts := u64(time.now().unix_milli()) * 1000000 // nanoseconds
		mut pi := Ngtcp2PktInfo{}

		// Write packets (drives the TLS handshake via crypto callbacks)
		nwritten := conn_write_pkt(c.ngtcp2_conn, &c.path, &pi, c.send_buf, ts) or {
			return error('failed to write handshake packet: ${err}')
		}

		if nwritten > 0 {
			// Send raw QUIC packet via UDP (already encrypted by ngtcp2)
			c.udp_socket.write(c.send_buf[..nwritten]) or {
				return error('failed to send handshake packet: ${err}')
			}
		}

		// Check if handshake completed
		if conn_get_handshake_completed(c.ngtcp2_conn) {
			c.handshake_done = true
			println('Handshake complete!')
			return
		}

		// Read response from server
		c.udp_socket.set_read_timeout(2 * time.second)
		n, _ := c.udp_socket.read(mut c.recv_buf) or {
			if err.msg().contains('timed out') || err.msg().contains('timeout') {
				// Timeout: retry writing
				continue
			}
			return error('failed to read packet: ${err}')
		}

		if n == 0 {
			continue
		}

		// Process received packet (ngtcp2 handles decryption via callbacks)
		conn_read_pkt(c.ngtcp2_conn, &c.path, &pi, c.recv_buf[..n], ts) or {
			err_str := err.msg()
			// Ignore non-fatal errors during handshake
			if err_str.contains('DISCARD_PKT') || err_str.contains('discard') {
				$if trace_quic ? {
					eprintln('[QUIC] discarded packet during client handshake: ${err_str}')
				}
				continue
			}
			return error('handshake read error: ${err}')
		}

		// Check again after processing
		if conn_get_handshake_completed(c.ngtcp2_conn) {
			c.handshake_done = true
			println('Handshake complete!')
			return
		}
	}

	return error('handshake timeout after ${max_attempts} attempts')
}

// perform_handshake_server performs server-side QUIC handshake
pub fn (mut c Connection) perform_handshake_server(cert_file string, key_file string) ! {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	println('Starting server QUIC handshake...')

	// Server handshake loop
	max_attempts := 50
	for attempt := 0; attempt < max_attempts; attempt++ {
		ts := u64(time.now().unix_milli()) * 1000000
		mut pi := Ngtcp2PktInfo{}

		// Read packet from client
		c.udp_socket.set_read_timeout(2 * time.second)
		n, _ := c.udp_socket.read(mut c.recv_buf) or {
			if err.msg().contains('timed out') || err.msg().contains('timeout') {
				continue
			}
			return error('failed to read packet: ${err}')
		}

		if n > 0 {
			// Process packet
			conn_read_pkt(c.ngtcp2_conn, &c.path, &pi, c.recv_buf[..n], ts) or {
				err_str := err.msg()
				if err_str.contains('DISCARD_PKT') || err_str.contains('discard') {
					$if trace_quic ? {
						eprintln('[QUIC] discarded packet during server handshake: ${err_str}')
					}
					continue
				}
				return error('server handshake read error: ${err}')
			}
		}

		// Write response packets
		nwritten := conn_write_pkt(c.ngtcp2_conn, &c.path, &pi, c.send_buf, ts) or { continue }

		if nwritten > 0 {
			c.udp_socket.write(c.send_buf[..nwritten]) or {
				return error('failed to send handshake packet: ${err}')
			}
		}

		// Check if handshake completed
		if conn_get_handshake_completed(c.ngtcp2_conn) {
			c.handshake_done = true
			println('Server handshake complete!')
			return
		}
	}

	return error('server handshake timeout')
}

// send_with_crypto sends data with encryption via ngtcp2 callbacks
pub fn (mut c Connection) send_with_crypto(stream_id u64, data []u8, crypto_ctx &CryptoContext) ! {
	if c.closed {
		return error('connection closed')
	}

	if !c.handshake_done {
		return error('handshake not completed')
	}

	// Write stream data (ngtcp2 handles encryption via callbacks)
	ts := u64(time.now().unix_milli()) * 1000000
	mut pi := Ngtcp2PktInfo{}

	nwritten, _ := conn_writev_stream(c.ngtcp2_conn, &c.path, &pi, c.send_buf, i64(stream_id),
		data, ts) or { return error('failed to write stream data: ${err}') }

	if nwritten > 0 {
		c.udp_socket.write(c.send_buf[..nwritten]) or {
			return error('failed to send packet: ${err}')
		}
	}

	// Update stream data
	mut stream := c.streams[stream_id] or { return error('stream not found') }
	stream.data << data
}

// recv_with_crypto receives data with decryption via ngtcp2 callbacks
pub fn (mut c Connection) recv_with_crypto(stream_id u64, crypto_ctx &CryptoContext) ![]u8 {
	if c.closed {
		return error('connection closed')
	}

	if !c.handshake_done {
		return error('handshake not completed')
	}

	// Read packet from UDP
	n, _ := c.udp_socket.read(mut c.recv_buf) or { return error('failed to read packet: ${err}') }

	if n == 0 {
		return []u8{}
	}

	// Process packet with ngtcp2 (handles decryption via callbacks)
	ts := u64(time.now().unix_milli()) * 1000000
	mut pi := Ngtcp2PktInfo{}

	conn_read_pkt(c.ngtcp2_conn, &c.path, &pi, c.recv_buf[..n], ts) or {
		if !err_is_fatal(ngtcp2_err_invalid_argument) {
			return []u8{}
		}
		return error('failed to read packet: ${err}')
	}

	// Return stream data
	stream := c.streams[stream_id] or { return error('stream not found') }
	return stream.data.clone()
}
