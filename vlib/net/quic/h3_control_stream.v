module quic

// This file drives the peer's incoming HTTP/3 control stream (RFC 9114
// §6.2.1): decoding frames via H3FrameDecoder (Phase 10), validating their
// ordering via H3ControlStreamState (Phase 10), and acting on the ones a
// v1 client role must: applying recognized SETTINGS (RFC 9204 §5's QPACK
// pair; anything else is recognized-but-inert per h3_frame.v's own SETTINGS
// doc comment), validating and surfacing GOAWAY, and rejecting CANCEL_PUSH/
// PUSH_PROMISE/MAX_PUSH_ID outright per this connection's push-disabled
// scope decision (h3_conn.v's module doc comment). Driving THIS endpoint's
// own outgoing control stream is just the one-shot header+SETTINGS write in
// h3_conn.v's open_own_streams_if_ready -- nothing else is ever sent on it.

// drive_peer_control_stream reads any new bytes on the peer's control
// stream, decodes as many complete frames as are available, and acts on
// each in order.
fn (mut h H3Conn) drive_peer_control_stream(stream_id u64, mut result H3PollResult) ! {
	new_bytes := h.qc.read_stream(stream_id)!
	total_pending := u64(h.peer_control_decoder.pending_len()) + u64(new_bytes.len)
	if total_pending > max_h3_control_plane_buffered_bytes {
		return error_with_code('h3: control stream ${stream_id} exceeded the ${max_h3_control_plane_buffered_bytes}-byte control-plane buffering limit without completing a frame', int(H3ErrorCode.excessive_load))
	}
	if new_bytes.len > 0 {
		h.peer_control_decoder.push(new_bytes)
	}
	for {
		decoded := h.peer_control_decoder.next()!
		if !decoded.has_frame {
			break
		}
		h.require_valid_frame_for_role(decoded.frame, .control)!
		h.peer_control_state.note_frame(decoded.frame)!
		h.apply_control_frame(decoded.frame, mut result)!
	}
}

// apply_control_frame acts on one already-order-validated frame from the
// peer's control stream.
fn (mut h H3Conn) apply_control_frame(frame H3Frame, mut result H3PollResult) ! {
	match frame {
		SettingsFrame {
			h.apply_peer_settings(frame.settings)!
			result.events << H3Event{
				kind: .settings_received
			}
		}
		GoawayFrame {
			if !goaway_id_is_valid_client_initiated_bidi_stream_id(frame.id) {
				return error_with_code('h3: GOAWAY id ${frame.id} is not a valid client-initiated bidirectional stream id', int(H3ErrorCode.id_error))
			}
			result.events << H3Event{
				kind: .goaway
				goaway_id: frame.id
			}
		}
		CancelPushFrame, PushPromiseFrame, MaxPushIdFrame {
			// Push is never authorized in v1 (no MAX_PUSH_ID is ever sent by
			// this client) -- any of these three is therefore always a
			// protocol violation. MaxPushIdFrame specifically: RFC 9114
			// §7.2.7 "A client MUST treat the receipt of a MAX_PUSH_ID frame
			// as a connection error of type H3_FRAME_UNEXPECTED" (a SERVER
			// is the only endpoint that should ever see one FROM a client).
			return error_with_code('h3: received a push-related frame on the control stream, but this client never authorizes push', int(H3ErrorCode.id_error))
		}
		DataFrame, HeadersFrame {
			// drive_peer_control_stream calls require_valid_frame_for_role
			// before this function runs, which already rejects these on a
			// .control-role stream (RFC 9114 §7 Table 1) -- unreachable here
			// in practice, but matched explicitly (V's sum-type match must be
			// exhaustive over every H3Frame variant) rather than folded into
			// a catch-all that could silently swallow a real future case.
			return error_with_code('h3: DATA/HEADERS is not valid on the control stream', int(H3ErrorCode.frame_unexpected))
		}
		H3RawFrame {
			// Grease/genuinely-unknown -- RFC 9114 §9: MUST be ignored.
		}
	}
}

// apply_peer_settings applies the 2 QPACK settings this implementation
// recognizes (RFC 9204 §5) to the local QPACK encoder; every other
// identifier -- including grease and genuinely unrecognized ones -- is
// syntactically valid and simply not acted on, matching h3_frame.v's own
// SettingsFrame doc comment ("deciding what to actually apply is a Phase
// 12 concern").
fn (mut h H3Conn) apply_peer_settings(settings []H3Setting) ! {
	peer_max_table_capacity := qpack_max_table_capacity_from_settings(settings)
	if peer_max_table_capacity > 0 {
		// The SETTINGS value is a CEILING (RFC 9204 §3.2.3), not a mandate --
		// this encoder is free to choose any capacity up to it, and chooses
		// the largest it can actually support. Passing the raw peer value as
		// BOTH set_capacity args would wrongly turn a legal, merely-large
		// peer-advertised ceiling (nothing in the RFC bounds the SETTINGS
		// value itself) into a fatal connection error the moment it exceeds
		// this implementation's own 64 MiB limit, instead of just using
		// less than what was offered.
		mut chosen_capacity := peer_max_table_capacity
		if chosen_capacity > u64(qpack_max_dynamic_table_capacity_bytes) {
			chosen_capacity = u64(qpack_max_dynamic_table_capacity_bytes)
		}
		instructions := h.qpack_encoder.set_capacity(chosen_capacity, peer_max_table_capacity)!
		if instructions.len > 0 {
			if enc_id := h.own_qpack_encoder_stream_id {
				h.qc.write_stream(enc_id, instructions, false)!
			}
		}
	}
	// SETTINGS_QPACK_BLOCKED_STREAMS: this connection's QpackEncoder never
	// risks blocking a stream at all (its own doc comment, qpack_encoder.v)
	// -- the peer's advertised budget is therefore always trivially
	// satisfied and needs no local bookkeeping to enforce.
}
