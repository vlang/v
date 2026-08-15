module quic

// Per-stream frame-legality rules from RFC 9114 §7 (Table 1) and the
// control-stream discipline in §6.2.1/§7.2.4. These are genuinely part of
// "the HTTP framing layer" (Table 1 lives inside §7's own text, not §4's
// message-semantics chapter) and need only a stream's ROLE, not full
// request/response context -- unlike the rest of §4.1's message framing
// (HEADERS -> zero-or-more DATA -> optional trailing HEADERS ordering
// WITHIN one request/response, single-request-per-stream enforcement,
// CONNECT's different framing), which needs request/response semantic
// state this phase deliberately does not model. That remainder is Phase
// 12's responsibility once real request/response objects exist to hold
// it -- left as an explicit, documented gap here rather than silently
// unimplemented, per this module's own established practice (see e.g.
// conn.v's RFC 9002 §6.4 bytes-in-flight note in
// quic_conformance_matrix.md).

// H3StreamRole is which of HTTP/3's three stream purposes (Table 1's three
// columns) a stream is currently playing. Determining a stream's role in
// the first place (reading its unidirectional Stream Type header via
// h3_stream_type.v, or recognizing it as a client-initiated bidirectional
// QUIC stream) is a Phase 12 wiring concern; this file only needs to be
// TOLD the role once known.
pub enum H3StreamRole {
	control
	request
	push
}

// is_h3_frame_valid_on_stream reports whether `frame`'s type is permitted
// on a stream playing `role`, per RFC 9114 §7 Table 1. H3RawFrame (both
// grease, §7.2.8, and genuinely unrecognized extension types, §9) is valid
// everywhere: Table 1's own "Reserved" row is Yes/Yes/Yes, and §9 states
// implementations "MUST ignore unknown or unsupported values in all
// extensible protocol elements" -- read directly from §9's text, not
// assumed by analogy with the grease case alone.
//
// This function does NOT enforce the SETTINGS-must-be-first-and-only-once
// rule (that needs per-stream sequencing state across multiple frames, not
// just this one frame's type) -- see H3ControlStreamState for that.
pub fn is_h3_frame_valid_on_stream(frame H3Frame, role H3StreamRole) bool {
	return match frame {
		DataFrame, HeadersFrame {
			role == .request || role == .push
		}
		CancelPushFrame, SettingsFrame, GoawayFrame, MaxPushIdFrame {
			role == .control
		}
		PushPromiseFrame {
			role == .request
		}
		H3RawFrame {
			true
		}
	}
}

// H3ControlStreamState tracks the one piece of control-stream discipline
// that depends on frame ORDER rather than just frame type: SETTINGS "MUST
// be sent as the first frame of each control stream... and MUST NOT be
// sent subsequently" (§6.2.1/§7.2.4). One instance is owned per control
// stream a connection has (its own outgoing one, and the peer's incoming
// one); frames arriving on a DIFFERENT stream never touch this type at
// all (is_h3_frame_valid_on_stream already rejects a SETTINGS frame
// received on a non-control stream independently of this state).
pub struct H3ControlStreamState {
mut:
	seen_first_frame bool
}

// new_h3_control_stream_state returns a fresh tracker for a control stream
// that has not yet received (or sent) any frames.
pub fn new_h3_control_stream_state() H3ControlStreamState {
	return H3ControlStreamState{}
}

// note_frame must be called, in stream order, for every frame decoded on
// this control stream -- BEFORE the caller acts on the frame's contents.
// Returns an error, carrying an H3ErrorCode via `error_with_code`, when:
//   - this is the first frame on the stream and it is not SETTINGS
//     (§6.2.1: "If the first frame of the control stream is any other
//     frame type, this MUST be treated as a connection error of type
//     H3_MISSING_SETTINGS"). A grease/unknown H3RawFrame as the first
//     frame also triggers this -- §9: "where a known frame type is
//     required to be in a specific location... an unknown frame type does
//     not satisfy that requirement and SHOULD be treated as an error";
//   - this is a SECOND (or later) SETTINGS frame on the same stream
//     (§7.2.4: "it MUST NOT be sent subsequently... the endpoint MUST
//     respond with a connection error of type H3_FRAME_UNEXPECTED").
pub fn (mut s H3ControlStreamState) note_frame(frame H3Frame) ! {
	is_settings := match frame {
		SettingsFrame { true }
		else { false }
	}
	if !s.seen_first_frame {
		s.seen_first_frame = true
		if !is_settings {
			return error_with_code("h3: control stream's first frame was not SETTINGS",
				int(H3ErrorCode.missing_settings))
		}
		return
	}
	if is_settings {
		return error_with_code('h3: received a second SETTINGS frame on a control stream',
			int(H3ErrorCode.frame_unexpected))
	}
}
