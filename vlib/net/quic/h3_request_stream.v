module quic

// This file covers the ONE piece of RFC 9114 §4.1's message-framing rules
// h3_message_state.v explicitly deferred to Phase 12: HEADERS -> zero-or-
// more DATA -> optional trailing HEADERS ordering WITHIN one request/
// response, and single-request-per-stream enforcement (implicit here: this
// state machine has no transition back to awaiting_response_headers once
// left, so a stream can never carry a second request). Distinct from
// H3ControlStreamState (h3_message_state.v), which only enforces "SETTINGS
// first" on a control stream -- this is a request/response-shaped state
// machine instead, and one instance is owned per client-opened bidi
// request stream, not per connection.

// H3RequestStreamPhase is where a request stream currently is in RFC
// 9114 §4.1's message sequence, from THIS (client) endpoint's receive
// side -- it tracks the SERVER's response, since a v1 client only ever
// sends its own request once, synchronously, when opening the stream
// (h3_conn.v's open_request_stream/send_request_headers/send_request_data),
// never incrementally re-entering an earlier phase of its own send side.
pub enum H3RequestStreamPhase {
	awaiting_response_headers
	in_body
	trailers_received
	done
}

// H3RequestStreamState is one request stream's phase tracker.
@[heap]
pub struct H3RequestStreamState {
mut:
	phase H3RequestStreamPhase
}

// new_h3_request_stream_state returns a tracker for a request stream that
// has not yet received any response frames.
pub fn new_h3_request_stream_state() &H3RequestStreamState {
	return &H3RequestStreamState{
		phase: .awaiting_response_headers
	}
}

// phase reports this stream's current position in the message sequence.
pub fn (s &H3RequestStreamState) phase() H3RequestStreamPhase {
	return s.phase
}

// note_frame_kind must be called, in stream order, for every frame decoded
// on this request stream's receive side, BEFORE the caller acts on the
// frame's contents (mirrors H3ControlStreamState.note_frame's calling
// convention). `frame_is_headers`/`frame_is_data` classify the frame that
// was just decoded (any OTHER Table-1-legal frame on a `.request`-role
// stream is illegal on a client's request stream in v1 -- PUSH_PROMISE is
// the only other type Table 1 permits there, and v1 never authorizes push,
// see h3_control_stream.v's MAX_PUSH_ID scope decision -- so both false is
// always rejected here, not silently accepted).
//
// Errors, carrying an H3ErrorCode via `error_with_code`, when:
//   - the first frame is not HEADERS (§4.1: "A HTTP message ... consists of
//     ... a HEADERS frame ... containing the message headers");
//   - a non-DATA, non-HEADERS frame arrives while `.in_body` (§4.1's frame
//     sequence permits only DATA between the two HEADERS blocks);
//   - ANY frame arrives once `.trailers_received` or `.done` (§4.1: "an
//     HTTP message consists of ... one HEADERS frame ... containing the
//     message headers ... one or more frames containing the message
//     content ... optionally, one HEADERS frame containing the trailer
//     section, if present" -- nothing legally follows trailers).
pub fn (mut s H3RequestStreamState) note_frame_kind(frame_is_headers bool, frame_is_data bool) ! {
	match s.phase {
		.awaiting_response_headers {
			if !frame_is_headers {
				return error_with_code('h3: first frame on a request stream was not HEADERS', int(H3ErrorCode.frame_unexpected))
			}
			// Phase is deliberately NOT advanced here: whether this HEADERS
			// block is the final response or an RFC 9110 §15.2 1xx interim
			// response (100 Continue, 103 Early Hints, ...) can only be
			// known after QPACK-decoding its :status field, which this
			// envelope-only state machine never sees. h3_conn.v's
			// deliver_decoded_headers calls note_final_response_headers()
			// once that is determined -- possibly on a LATER poll() call,
			// if this section was blocked. Leaving phase unchanged here
			// means a second (or third, ...) HEADERS block arriving while
			// still awaiting the final response is processed by this exact
			// same branch again, which is what allows any number of 1xx
			// responses to precede the final one.
		}
		.in_body {
			if frame_is_headers {
				s.phase = .trailers_received
				return
			}
			if !frame_is_data {
				return error_with_code('h3: unexpected frame type on a request stream mid-body', int(H3ErrorCode.frame_unexpected))
			}
		}
		.trailers_received {
			return error_with_code('h3: received a frame on a request stream after its trailing HEADERS', int(H3ErrorCode.frame_unexpected))
		}
		.done {
			return error_with_code('h3: received a frame on a request stream after it ended', int(H3ErrorCode.frame_unexpected))
		}
	}
}

// note_final_response_headers transitions the stream past the response-
// headers position once the caller (h3_conn.v) has determined -- by
// inspecting a decoded HEADERS block's :status field, which this framing-
// only state machine never sees -- that it is the final (non-1xx) response,
// as opposed to an RFC 9110 §15.2 interim response that leaves the phase
// unchanged. A no-op once already past .awaiting_response_headers: safe to
// call at most once per stream in practice (note_frame_kind only reaches
// the .awaiting_response_headers branch before this has ever been called),
// but idempotent rather than assuming that ordering.
pub fn (mut s H3RequestStreamState) note_final_response_headers() {
	if s.phase == .awaiting_response_headers {
		s.phase = .in_body
	}
}

// note_fin marks this stream done -- called once the caller has both (a)
// observed this stream's receive side reach a FIN terminal state (12a's
// QuicConn.stream_recv_status) and (b) drained every already-buffered
// frame from it (a FIN only means no MORE bytes are coming; frames already
// received but not yet decoded must still be processed first). Idempotent:
// safe to call again once already `.done`.
pub fn (mut s H3RequestStreamState) note_fin() {
	s.phase = .done
}
