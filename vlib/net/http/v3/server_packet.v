module v3

// Server-side QUIC packet decryption and frame dispatch.

fn (mut s Server) handle_packet(mut conn ServerConnection, packet []u8) {
	decrypted := s.decrypt_incoming_packet(mut conn, packet) or {
		eprintln('Failed to decrypt packet: ${err}')
		return
	}

	mut idx := 0
	mut current_stream_id := u64(0)

	for idx < decrypted.len {
		frame_type_val, bytes_read := decode_varint(decrypted[idx..]) or {
			eprintln('Failed to decode frame type: ${err}')
			return
		}
		idx += bytes_read

		frame_length, bytes_read2 := decode_varint(decrypted[idx..]) or {
			eprintln('Failed to decode frame length: ${err}')
			return
		}
		idx += bytes_read2

		if idx + int(frame_length) > decrypted.len {
			eprintln('Incomplete frame')
			return
		}

		payload := decrypted[idx..idx + int(frame_length)]
		idx += int(frame_length)

		frame_type := frame_type_from_u64(frame_type_val) or { continue }

		if frame_type == .headers {
			conn.mu.lock()
			current_stream_id = conn.next_client_stream_id
			conn.next_client_stream_id += 4
			conn.mu.unlock()
		}

		s.dispatch_server_frame(mut conn, frame_type, current_stream_id, payload)
	}
}

fn (mut s Server) decrypt_incoming_packet(mut conn ServerConnection, packet []u8) ![]u8 {
	base_iv := if conn.crypto_ctx.rx_iv.len == 12 {
		conn.crypto_ctx.rx_iv
	} else {
		[]u8{len: 12}
	}

	conn.mu.lock()
	pkt_num := if conn.crypto_ctx.rx_hp_key.len > 0 {
		extracted_pn, _, _ := conn.crypto_ctx.extract_and_unprotect_pn(packet, conn.quic_conn.conn_id.len) or {
			pn := conn.rx_packet_number
			conn.rx_packet_number++
			conn.mu.unlock()
			return conn.crypto_ctx.decrypt_packet(packet, []u8{}, base_iv, pn)
		}
		conn.rx_packet_number = extracted_pn + 1
		conn.mu.unlock()
		extracted_pn
	} else {
		pn := conn.rx_packet_number
		conn.rx_packet_number++
		conn.mu.unlock()
		pn
	}

	return conn.crypto_ctx.decrypt_packet(packet, []u8{}, base_iv, pkt_num)
}

fn (mut s Server) dispatch_server_frame(mut conn ServerConnection, frame_type FrameType, stream_id u64, payload []u8) {
	match frame_type {
		.headers {
			s.handle_headers_frame(mut conn, stream_id, payload) or {
				close_on_h3_error(mut conn, err)
			}
		}
		.data {
			s.handle_data_frame(mut conn, stream_id, payload) or {
				close_on_h3_error(mut conn, err)
			}
		}
		.settings {
			s.handle_settings_frame(mut conn, payload) or { close_on_h3_error(mut conn, err) }
		}
		.goaway {
			if payload.len > 0 {
				goaway_id, _ := decode_varint(payload) or {
					eprintln('Failed to decode GOAWAY stream ID: ${err}')
					return
				}
				conn.last_peer_goaway_stream_id = goaway_id
				$if debug {
					eprintln('Received GOAWAY with stream ID ${goaway_id}')
				}
			}
		}
		else {}
	}
}

// close_on_h3_error maps an error message to an H3ErrorCode and closes the
// QUIC connection with the appropriate application error code (RFC 9114 §8).
fn close_on_h3_error(mut conn ServerConnection, err IError) {
	error_code := map_h3_error(err.msg())
	conn.quic_conn.close_with_error(u64(error_code), err.msg()) or { conn.quic_conn.close() }
}

// map_h3_error extracts an H3ErrorCode from an error message string.
fn map_h3_error(msg string) H3ErrorCode {
	if msg.contains('H3_SETTINGS_ERROR') {
		return .h3_settings_error
	}
	if msg.contains('H3_FRAME_UNEXPECTED') {
		return .h3_frame_unexpected
	}
	if msg.contains('H3_ID_ERROR') {
		return .h3_id_error
	}
	if msg.contains('H3_MESSAGE_ERROR') {
		return .h3_message_error
	}
	if msg.contains('H3_MISSING_SETTINGS') {
		return .h3_missing_settings
	}
	return .h3_general_protocol_error
}
