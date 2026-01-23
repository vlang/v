// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

import time

// QUIC Handshake Implementation
// Implements the QUIC handshake process using TLS 1.3

// HandshakeState represents the state of the QUIC handshake
pub enum HandshakeState {
	initial
	handshake_in_progress
	handshake_complete
	failed
}

// perform_handshake performs the complete QUIC handshake
pub fn (mut c Connection) perform_handshake() ! {
	if c.closed {
		return error('connection closed')
	}

	if c.ngtcp2_conn == unsafe { nil } {
		return error('ngtcp2 connection not initialized')
	}

	println('Starting QUIC handshake...')

	// Create crypto context
	mut crypto_ctx := new_crypto_context_client(['h3']) or {
		return error('failed to create crypto context: ${err}')
	}
	defer {
		crypto_ctx.free()
	}

	// Derive initial secrets
	tx_secret, rx_secret := derive_initial_secrets(c.conn_id, false) or {
		return error('failed to derive initial secrets: ${err}')
	}

	crypto_ctx.tx_secret = tx_secret
	crypto_ctx.rx_secret = rx_secret

	println('Initial secrets derived')

	// Send Initial packet
	ts := u64(time.now().unix_milli())
	mut path := Ngtcp2PathStruct{}
	mut pi := Ngtcp2PktInfo{}

	// Write initial packet
	nwritten := conn_write_pkt(c.ngtcp2_conn, &path, &pi, c.send_buf, ts) or {
		return error('failed to write initial packet: ${err}')
	}

	if nwritten > 0 {
		println('Sending Initial packet (${nwritten} bytes)')

		// Encrypt packet
		encrypted := crypto_ctx.encrypt_packet(c.send_buf[..nwritten], []u8{}) or {
			return error('failed to encrypt initial packet: ${err}')
		}

		// Send via UDP
		c.udp_socket.write(encrypted) or { return error('failed to send initial packet: ${err}') }
	}

	// Handshake loop
	max_attempts := 20
	for attempt := 0; attempt < max_attempts; attempt++ {
		println('Handshake attempt ${attempt + 1}/${max_attempts}')

		// Read response with timeout
		c.udp_socket.set_read_timeout(500 * time.millisecond)
		n, _ := c.udp_socket.read(mut c.recv_buf) or {
			if err.msg().contains('timeout') {
				println('  Timeout waiting for response')
				time.sleep(100 * time.millisecond)
				continue
			}
			return error('failed to read packet: ${err}')
		}

		if n == 0 {
			println('  No data received')
			time.sleep(100 * time.millisecond)
			continue
		}

		println('  Received packet (${n} bytes)')

		// Decrypt packet
		decrypted := crypto_ctx.decrypt_packet(c.recv_buf[..n], []u8{}) or {
			println('  Failed to decrypt: ${err}')
			continue
		}

		// Process packet with ngtcp2
		conn_read_pkt(c.ngtcp2_conn, &path, &pi, decrypted, ts) or {
			if err_is_fatal(ngtcp2_err_invalid_argument) {
				return error('handshake failed: ${err}')
			}
			println('  Packet processing error (non-fatal): ${err}')
		}

		// Provide crypto data to TLS
		crypto_ctx.provide_data(.initial, decrypted) or {
			println('  Failed to provide crypto data: ${err}')
		}

		// Perform TLS handshake step
		handshake_done := crypto_ctx.do_handshake() or {
			println('  TLS handshake error: ${err}')
			continue
		}

		// Check if handshake is complete
		if handshake_done || conn_get_handshake_completed(c.ngtcp2_conn) {
			c.handshake_done = true
			println('✓ Handshake complete!')
			return
		}

		// Generate response packet
		nwritten2 := conn_write_pkt(c.ngtcp2_conn, &path, &pi, c.send_buf, ts) or {
			println('  Failed to write response: ${err}')
			continue
		}

		if nwritten2 > 0 {
			println('  Sending response packet (${nwritten2} bytes)')

			// Encrypt and send
			encrypted2 := crypto_ctx.encrypt_packet(c.send_buf[..nwritten2], []u8{}) or {
				println('  Failed to encrypt response: ${err}')
				continue
			}

			c.udp_socket.write(encrypted2) or {
				println('  Failed to send response: ${err}')
				continue
			}
		}

		time.sleep(100 * time.millisecond)
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

	// Create crypto context for server
	mut crypto_ctx := new_crypto_context_server(cert_file, key_file, ['h3']) or {
		return error('failed to create crypto context: ${err}')
	}
	defer {
		crypto_ctx.free()
	}

	// Derive initial secrets (server side)
	tx_secret, rx_secret := derive_initial_secrets(c.conn_id, true) or {
		return error('failed to derive initial secrets: ${err}')
	}

	crypto_ctx.tx_secret = tx_secret
	crypto_ctx.rx_secret = rx_secret

	println('Server initial secrets derived')

	// Wait for Initial packet from client
	ts := u64(time.now().unix_milli())
	mut path := Ngtcp2PathStruct{}
	mut pi := Ngtcp2PktInfo{}

	// Read initial packet
	n, _ := c.udp_socket.read(mut c.recv_buf) or {
		return error('failed to read initial packet: ${err}')
	}

	if n == 0 {
		return error('no initial packet received')
	}

	println('Received Initial packet from client (${n} bytes)')

	// Decrypt packet
	decrypted := crypto_ctx.decrypt_packet(c.recv_buf[..n], []u8{}) or {
		return error('failed to decrypt initial packet: ${err}')
	}

	// Process packet
	conn_read_pkt(c.ngtcp2_conn, &path, &pi, decrypted, ts) or {
		return error('failed to process initial packet: ${err}')
	}

	// Provide crypto data to TLS
	crypto_ctx.provide_data(.initial, decrypted) or {
		return error('failed to provide crypto data: ${err}')
	}

	// Perform TLS handshake
	crypto_ctx.do_handshake() or { return error('TLS handshake failed: ${err}') }

	// Send Handshake packet
	nwritten := conn_write_pkt(c.ngtcp2_conn, &path, &pi, c.send_buf, ts) or {
		return error('failed to write handshake packet: ${err}')
	}

	if nwritten > 0 {
		println('Sending Handshake packet (${nwritten} bytes)')

		// Encrypt and send
		encrypted := crypto_ctx.encrypt_packet(c.send_buf[..nwritten], []u8{}) or {
			return error('failed to encrypt handshake packet: ${err}')
		}

		c.udp_socket.write(encrypted) or { return error('failed to send handshake packet: ${err}') }
	}

	// Wait for handshake completion
	max_attempts := 10
	for attempt := 0; attempt < max_attempts; attempt++ {
		// Check if handshake is complete
		if crypto_ctx.is_handshake_complete() && conn_get_handshake_completed(c.ngtcp2_conn) {
			c.handshake_done = true
			println('✓ Server handshake complete!')
			return
		}

		// Read more packets
		n2, _ := c.udp_socket.read(mut c.recv_buf) or {
			time.sleep(100 * time.millisecond)
			continue
		}

		if n2 > 0 {
			// Decrypt and process
			decrypted2 := crypto_ctx.decrypt_packet(c.recv_buf[..n2], []u8{}) or { continue }

			conn_read_pkt(c.ngtcp2_conn, &path, &pi, decrypted2, ts) or { continue }

			crypto_ctx.provide_data(.handshake, decrypted2) or { continue }

			crypto_ctx.do_handshake() or { continue }

			// Send response if needed
			nwritten2 := conn_write_pkt(c.ngtcp2_conn, &path, &pi, c.send_buf, ts) or { continue }

			if nwritten2 > 0 {
				encrypted2 := crypto_ctx.encrypt_packet(c.send_buf[..nwritten2], []u8{}) or {
					continue
				}

				c.udp_socket.write(encrypted2) or { continue }
			}
		}

		time.sleep(100 * time.millisecond)
	}

	return error('server handshake timeout')
}

// send_with_crypto sends data with encryption
pub fn (mut c Connection) send_with_crypto(stream_id u64, data []u8, crypto_ctx &CryptoContext) ! {
	if c.closed {
		return error('connection closed')
	}

	if !c.handshake_done {
		return error('handshake not completed')
	}

	// Write stream data
	ts := u64(time.now().unix_milli())
	mut path := Ngtcp2PathStruct{}
	mut pi := Ngtcp2PktInfo{}

	nwritten, datalen := conn_writev_stream(c.ngtcp2_conn, &path, &pi, c.send_buf, i64(stream_id),
		data, ts) or { return error('failed to write stream data: ${err}') }

	if nwritten > 0 {
		// Encrypt packet
		mut crypto_ctx_mut := unsafe { &CryptoContext(crypto_ctx) }
		encrypted := crypto_ctx_mut.encrypt_packet(c.send_buf[..nwritten], []u8{}) or {
			return error('failed to encrypt packet: ${err}')
		}

		// Send via UDP
		c.udp_socket.write(encrypted) or { return error('failed to send packet: ${err}') }
	}

	// Update stream data
	mut stream := c.streams[stream_id] or { return error('stream not found') }
	stream.data << data
}

// recv_with_crypto receives data with decryption
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

	// Decrypt packet
	mut crypto_ctx_mut := unsafe { &CryptoContext(crypto_ctx) }
	decrypted := crypto_ctx_mut.decrypt_packet(c.recv_buf[..n], []u8{}) or {
		return error('failed to decrypt packet: ${err}')
	}

	// Process packet with ngtcp2
	ts := u64(time.now().unix_milli())
	mut path := Ngtcp2PathStruct{}
	mut pi := Ngtcp2PktInfo{}

	conn_read_pkt(c.ngtcp2_conn, &path, &pi, decrypted, ts) or {
		if !err_is_fatal(ngtcp2_err_invalid_argument) {
			return []u8{}
		}
		return error('failed to read packet: ${err}')
	}

	// Return stream data
	stream := c.streams[stream_id] or { return error('stream not found') }
	return stream.data.clone()
}
