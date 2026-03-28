module v3

// Server-side QUIC packet decryption and frame dispatch.

fn (mut s Server) handle_packet(mut conn ServerConnection, packet []u8) {
	// Serialize all packet processing for this connection. handle_packet is
	// spawned per UDP packet (see listen_and_serve), so concurrent packets
	// for the same connection would race on ngtcp2_conn state, stream_events,
	// and the streams map without this lock. The inner conn.mu remains for
	// fine-grained field access within frame handlers.
	conn.packet_mu.lock()
	dispatched_ids := s.process_packet_frames(mut conn, packet)
	// Collect FIN event IDs accumulated during process_incoming_packet's
	// drain_stream_events, then clear. Union with dispatched IDs covers
	// the case where FIN arrived in a previous packet but the stream
	// wasn't yet completable (e.g. HEADERS hadn't been processed).
	mut check_ids := conn.quic_conn.pending_fin_streams.clone()
	conn.quic_conn.pending_fin_streams.clear()
	for id in dispatched_ids {
		if id !in check_ids {
			check_ids << id
		}
	}
	if check_ids.len > 0 {
		s.check_fin_completions(mut conn, check_ids)
	}
	conn.packet_mu.unlock()
}

// process_packet_frames orchestrates QUIC state feeding and HTTP/3 frame dispatch.
// MUST be called under conn.packet_mu to ensure serialized access.
// Returns the set of stream IDs that had frames dispatched in this packet.
fn (mut s Server) process_packet_frames(mut conn ServerConnection, packet []u8) []u64 {
	s.ingest_quic_packet(mut conn, packet)
	return s.decode_and_dispatch_frames(mut conn, packet)
}

// ingest_quic_packet feeds the raw packet to ngtcp2 for QUIC state tracking —
// connection-level ACKs, flow control, and FIN/close event detection via C
// callbacks. This MUST run before frame parsing so that stream FIN flags are
// up-to-date when check_fin_completions sweeps after all frames are processed.
fn (mut s Server) ingest_quic_packet(mut conn ServerConnection, packet []u8) {
	conn.quic_conn.process_incoming_packet(packet) or {
		eprintln('QUIC packet processing failed: ${err}')
	}
}

// decode_and_dispatch_frames decrypts the packet, decodes HTTP/3 frames, and
// dispatches each frame to the appropriate handler. Returns stream IDs that
// received frames, used by handle_packet for targeted FIN completion checks.
fn (mut s Server) decode_and_dispatch_frames(mut conn ServerConnection, packet []u8) []u64 {
	decrypted := s.decrypt_incoming_packet(mut conn, packet) or {
		eprintln('Failed to decrypt packet: ${err}')
		return []u64{}
	}

	mut dispatched_ids := []u64{}
	mut idx := 0
	mut current_stream_id := u64(0)

	for idx < decrypted.len {
		frame_type_val, bytes_read := decode_varint(decrypted[idx..]) or {
			eprintln('Failed to decode frame type: ${err}')
			return dispatched_ids
		}
		idx += bytes_read

		frame_length, bytes_read2 := decode_varint(decrypted[idx..]) or {
			eprintln('Failed to decode frame length: ${err}')
			return dispatched_ids
		}
		idx += bytes_read2

		if idx + int(frame_length) > decrypted.len {
			eprintln('Incomplete frame')
			return dispatched_ids
		}

		payload := decrypted[idx..idx + int(frame_length)]
		idx += int(frame_length)

		frame_type := frame_type_from_u64(frame_type_val) or { continue }

		if frame_type == .headers {
			conn.mu.lock()
			current_stream_id = conn.next_client_stream_id
			conn.next_client_stream_id += 4
			conn.mu.unlock()
			if current_stream_id !in dispatched_ids {
				dispatched_ids << current_stream_id
			}
		}

		s.dispatch_server_frame(mut conn, frame_type, current_stream_id, payload)
	}

	return dispatched_ids
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
