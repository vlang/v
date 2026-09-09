module quic

// This file drives QPACK's own two peer-initiated unidirectional streams
// (RFC 9204 §4.2) -- distinct from the HTTP/3 control stream and from any
// request stream, and from THIS endpoint's own outgoing encoder/decoder
// streams (opened once, in h3_conn.v's open_own_streams_if_ready, with
// nothing further ever written on the decoder stream except what this file
// relays). Previously entirely unwired: qpack_decoder.v/qpack_encoder.v's
// own doc comments explicitly deferred "writing encoder-stream bytes onto
// a real QUIC unidirectional stream under real flow control" and this
// exact glue loop to Phase 12.

// drive_peer_qpack_encoder_stream reads any new bytes on the peer's QPACK
// encoder stream, applies as many complete instructions as are available
// (RFC 9204 §4.3) to this connection's own QpackDecoder, relays each
// instruction's resulting decoder-stream bytes (an Insert Count Increment,
// per QpackDecoder's own immediate-acknowledgment policy) onto this
// endpoint's own outgoing decoder stream, and -- after EVERY successful
// application, since any one of them can be what finally satisfies a
// blocked section's Required Insert Count -- retries every currently
// blocked field section.
fn (mut h H3Conn) drive_peer_qpack_encoder_stream(stream_id u64, mut result H3PollResult) ! {
	new_bytes := h.qc.read_stream(stream_id)!
	if new_bytes.len > 0 {
		h.peer_qpack_encoder_buf << new_bytes
	}
	if u64(h.peer_qpack_encoder_buf.len) > max_h3_control_plane_buffered_bytes {
		return error_with_code('h3: QPACK encoder stream ${stream_id} exceeded the ${max_h3_control_plane_buffered_bytes}-byte control-plane buffering limit without completing an instruction', int(H3ErrorCode.excessive_load))
	}
	for {
		// apply_encoder_instruction's own errors already carry the correct
		// QPACK_ENCODER_STREAM_ERROR code (RFC 9204 §2.2.3) for every
		// failure path -- propagate as-is rather than overriding it.
		applied := h.qpack_decoder.apply_encoder_instruction(h.peer_qpack_encoder_buf)!
		if !applied.applied {
			break
		}
		h.peer_qpack_encoder_buf.delete_many(0, applied.consumed)
		if applied.decoder_instructions.len > 0 {
			if dec_id := h.own_qpack_decoder_stream_id {
				h.qc.write_stream(dec_id, applied.decoder_instructions, false)!
			}
		}
		h.retry_blocked_sections(mut result)!
	}
}

// drive_peer_qpack_decoder_stream reads any new bytes on the peer's QPACK
// decoder stream and applies as many complete instructions as are
// available (RFC 9204 §4.4) to this connection's own QpackEncoder --
// releasing dynamic-table references and advancing Known Received Count as
// the peer acknowledges (or cancels) field sections this endpoint sent.
fn (mut h H3Conn) drive_peer_qpack_decoder_stream(stream_id u64) ! {
	new_bytes := h.qc.read_stream(stream_id)!
	if new_bytes.len > 0 {
		h.peer_qpack_decoder_buf << new_bytes
	}
	if u64(h.peer_qpack_decoder_buf.len) > max_h3_control_plane_buffered_bytes {
		return error_with_code('h3: QPACK decoder stream ${stream_id} exceeded the ${max_h3_control_plane_buffered_bytes}-byte control-plane buffering limit without completing an instruction', int(H3ErrorCode.excessive_load))
	}
	for {
		// Unlike apply_encoder_instruction, decode_qpack_decoder_instruction
		// does NOT attach a QpackErrorCode to its own malformed-data errors
		// (its doc comment leaves that to the caller) -- a genuine parse
		// failure here means the bytes on the PEER'S decoder stream are
		// malformed, which is QPACK_DECODER_STREAM_ERROR (RFC 9204 §2.2.3:
		// this endpoint is acting as the ENCODER, receiving decoder-stream
		// instructions), the mirror-image of apply_encoder_instruction's own
		// encoder_stream_error wrapping one function above.
		decoded := decode_qpack_decoder_instruction(h.peer_qpack_decoder_buf) or {
			return error_with_code(err.msg(), int(QpackErrorCode.decoder_stream_error))
		}
		if !decoded.has_instruction {
			break
		}
		h.peer_qpack_decoder_buf.delete_many(0, decoded.consumed)
		match decoded.instr {
			QpackSectionAck {
				h.qpack_encoder.note_section_acknowledged(decoded.instr.stream_id)!
			}
			QpackStreamCancellation {
				h.qpack_encoder.note_stream_cancelled(decoded.instr.stream_id)
			}
			QpackInsertCountIncrement {
				h.qpack_encoder.note_insert_count_increment(decoded.instr.increment)!
			}
		}
	}
}
