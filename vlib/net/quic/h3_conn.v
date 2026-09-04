module quic

// H3Conn wires HTTP/3 framing (h3_frame.v et al, Phase 10) and QPACK
// (qpack_*.v, Phase 11) onto a real `QuicConn` (Phase 9): opening this
// endpoint's own control and QPACK streams, classifying and driving the
// peer's, running the per-request-stream message-framing state machine
// (h3_request_stream.v), and retrying field sections QPACK reports as
// blocked once more encoder-stream data arrives. This is the file every
// "deferred to Phase 12" doc comment across h3_*.v/qpack_*.v pointed at.
//
// Scope decision, documented here and in PROGRESS.md/the conformance
// matrix: this v1 client role NEVER sends MAX_PUSH_ID, so RFC 9114
// §7.2.7's push-authorization precondition is never satisfied -- any
// PUSH_PROMISE/CANCEL_PUSH this connection receives is therefore always a
// protocol violation (H3_ID_ERROR), and no push-ID/max-push-id tracking
// state exists anywhere in this file. CONNECT is similarly out of scope --
// nothing in H3ClientRequest (net.http, Phase 12d) ever constructs one.

// max_h3_control_plane_buffered_bytes bounds how many not-yet-decoded
// bytes this connection will buffer for a CONTROL-PLANE stream (the HTTP/3
// control stream, or either QPACK stream) before treating it as a
// connection error. Mirrors crypto_stream.v's max_crypto_stream_buffered_
// bytes reasoning exactly: control-plane messages (SETTINGS, GOAWAY,
// QPACK instructions) never legitimately need anywhere near this much.
// Deliberately NOT applied to a REQUEST stream's H3FrameDecoder -- DATA
// frames legitimately carry arbitrarily large bodies (h3_frame.v's own
// module doc comment), so capping there would incorrectly reject a
// spec-legal large transfer; H3FrameDecoder.pending_len() exists
// specifically so a request stream's buffering can instead be reasoned
// about against QUIC's own per-stream flow-control window (already
// enforced by QuicConn itself), not an independent byte cap here.
pub const max_h3_control_plane_buffered_bytes = u64(65536)

// max_h3_uni_stream_header_buffered_bytes bounds how many bytes this
// connection will buffer for a peer-initiated unidirectional stream whose
// TYPE has not yet been determined (the Stream Type varint, plus a Push
// ID varint for a push stream) -- both are single small varints (RFC 9114
// §6.2 Figure 1 / RFC 9204 §4.2), so any peer sending more than this
// before completing its own header is malformed, not merely slow.
const max_h3_uni_stream_header_buffered_bytes = 32

// H3EventKind is one kind of thing an H3Conn.poll()/process_timeouts()
// call can report.
pub enum H3EventKind {
	settings_received
	response_headers
	response_data
	response_trailers
	response_ended
	request_error
	goaway
	connection_error
}

// H3Event reports one thing that happened. Which fields are meaningful
// depends on `kind`: `stream_id` is set for every per-request-stream kind
// (response_headers/response_data/response_trailers/response_ended/
// request_error); `headers` for response_headers/response_trailers;
// `data` for response_data; `error_code`/`reason` for request_error/
// connection_error; `goaway_id` for goaway.
pub struct H3Event {
pub:
	kind       H3EventKind
	stream_id  ?u64
	headers    []QpackFieldLine
	data       []u8
	error_code ?u64
	reason     string
	goaway_id  ?u64
}

// H3PollResult reports everything one H3Conn.poll()/process_timeouts()
// call produced -- mirrors QuicConn.PollResult's shape exactly, with
// `outgoing`/`next_timeout` passed straight through from the wrapped
// QuicConn (this layer adds no timers or datagrams of its own beyond what
// writing to QUIC streams already queues).
pub struct H3PollResult {
pub mut:
	events       []H3Event
	outgoing     []QuicDatagram
	next_timeout ?u64
}

// H3ConnParams configures a new H3Conn.
pub struct H3ConnParams {
pub:
	// settings is this client's own outgoing SETTINGS frame contents (RFC
	// 9114 §7.2.4). Must include SETTINGS_QPACK_MAX_TABLE_CAPACITY /
	// SETTINGS_QPACK_BLOCKED_STREAMS (RFC 9204 §5) for QPACK to have any
	// dynamic-table capacity at all -- an H3Conn does not invent these
	// values itself, matching qpack_settings.v's own "pure extraction, no
	// opinion on what to send" scope.
	settings []H3Setting
	// own_qpack_max_table_capacity configures this endpoint's OWN QPACK
	// decoder (RFC 9204 §3.2.3) -- separate from `settings` above (which
	// only carries the WIRE bytes) since new_qpack_decoder needs the raw
	// value, not a re-parse of what this connection just encoded.
	own_qpack_max_table_capacity u64
}

// UniStreamKind classifies a peer-initiated unidirectional stream once
// enough of its header has arrived to tell -- the union of h3_stream_
// type.v's and qpack_stream_type.v's two separate codepoint spaces, which
// neither file's own classifier alone can resolve (QPACK's 0x02/0x03 are
// not part of RFC 9114's own registry, so classify_h3_unidirectional_
// stream_type would wrongly bucket them as .unknown if consulted first).
enum UniStreamGroup {
	h3
	qpack
}

struct UniStreamKind {
	group   UniStreamGroup
	h3_kind H3UnidirectionalStreamKind
	q_kind  QpackStreamKind
}

// classify_peer_uni_stream_header attempts to decode a peer unidirectional
// stream's header from the START of `buf` -- QPACK's codepoint space
// (0x02/0x03) is checked FIRST, since h3_stream_type.v's own classifier
// has no knowledge of it and would otherwise misclassify it as `.unknown`.
// Returns none (not an error) if `buf` does not yet hold a complete
// header, matching every other resumable parser in this module.
fn classify_peer_uni_stream_header(buf []u8) ?(UniStreamKind, int) {
	raw_type, type_len := decode_varint(buf) or { return none }
	if q_kind := classify_qpack_stream_type(raw_type) {
		return UniStreamKind{
			group: .qpack
			q_kind: q_kind
		}, type_len
	}
	h3_kind := classify_h3_unidirectional_stream_type(raw_type)
	if h3_kind == .push {
		_, id_len := decode_varint(buf[type_len..]) or { return none }
		return UniStreamKind{
			group: .h3
			h3_kind: .push
		}, type_len + id_len
	}
	return UniStreamKind{
		group: .h3
		h3_kind: h3_kind
	}, type_len
}

// BlockedFieldSection is one HEADERS frame this connection could not yet
// decode because QpackDecoder.decode_field_section reported it blocked
// (RFC 9204 §2.1.2/§2.2.1 -- the section's Required Insert Count has not
// been satisfied by this decoder's own Insert Count yet). `is_trailers` is
// captured at QUEUE time (from the request stream's phase immediately
// BEFORE this frame's own note_frame_kind call advanced it) since, by the
// time a retry finally succeeds, the state machine has already moved on
// and can no longer answer "which HEADERS block was this."
struct BlockedFieldSection {
	stream_id   u64
	buf         []u8
	is_trailers bool
}

// H3Conn is one HTTP/3 connection: a wrapped QuicConn plus every piece of
// session state h3_message_state.v/h3_frame.v/h3_stream_type.v/qpack_*.v
// left for "a real connection" to own.
@[heap]
pub struct H3Conn {
mut:
	qc &QuicConn

	own_settings                 []H3Setting
	own_qpack_max_table_capacity u64

	own_control_stream_id       ?u64
	own_qpack_encoder_stream_id ?u64
	own_qpack_decoder_stream_id ?u64

	// Peer-initiated uni streams whose type is still unknown: raw
	// accumulated header bytes, keyed by stream_id. Removed once
	// classified (bounded by max_h3_uni_stream_header_buffered_bytes per
	// entry, and by MAX_STREAMS -- the same locally-controlled cap that
	// bounds conn.v's announced_peer_streams -- for entry count).
	pending_peer_uni_headers map[u64][]u8

	peer_control_stream_id       ?u64
	peer_control_state           H3ControlStreamState
	peer_control_decoder         &H3FrameDecoder = unsafe { nil }
	peer_qpack_encoder_stream_id ?u64
	peer_qpack_decoder_stream_id ?u64
	peer_qpack_encoder_buf       []u8
	peer_qpack_decoder_buf       []u8
	qpack_stream_registry        QpackStreamRegistry
	// Every OTHER classified-but-uninteresting peer uni stream (push,
	// reserved/grease, or genuinely unknown -- RFC 9114 §6.2/§6.2.3/§9 all
	// require these to be silently tolerated, never treated as an error).
	// Bytes arriving on one of these are never read again (the "abort or
	// discard" §6.2 MAY is satisfied by simply not reading further, since
	// this module has no STOP_SENDING API yet -- documented follow-up, see
	// PROGRESS.md).
	ignored_peer_streams map[u64]bool

	qpack_encoder QpackEncoder
	qpack_decoder QpackDecoder

	request_streams      map[u64]&H3RequestStreamState
	request_decoders     map[u64]&H3FrameDecoder
	dead_request_streams map[u64]bool

	blocked_sections []BlockedFieldSection

	closed bool
}

// new_h3_conn wraps `qc` (already dial()'d, not yet necessarily
// established) with HTTP/3 + QPACK session state. Nothing is sent yet --
// this endpoint's own control/QPACK streams open lazily, the first time
// poll()/process_timeouts() observes handshake_confirmed.
pub fn new_h3_conn(mut qc QuicConn, params H3ConnParams) &H3Conn {
	return &H3Conn{
		qc: qc
		own_settings: params.settings
		own_qpack_max_table_capacity: params.own_qpack_max_table_capacity
		peer_control_decoder: new_h3_frame_decoder()
		qpack_decoder: new_qpack_decoder(params.own_qpack_max_table_capacity)
		qpack_encoder: new_qpack_encoder()
		request_streams: map[u64]&H3RequestStreamState{}
		request_decoders: map[u64]&H3FrameDecoder{}
	}
}

// free releases native resources owned by the wrapped QUIC connection.
// It is idempotent and must be called once the H3Conn will no longer be polled.
pub fn (mut h H3Conn) free() {
	h.qc.handshake.free()
}

// established reports whether the wrapped QuicConn has reached RFC 9000's
// `.established` state -- the earliest point at which this connection's
// own control/QPACK streams are queued to open, and the signal net.http
// (Phase 12c) needs before it may start draining queued requests.
pub fn (h &H3Conn) established() bool {
	return h.qc.state() == .established
}

// poll feeds one datagram (or none) through the wrapped QuicConn, then
// drives every consequence: opening our own control/QPACK streams once
// established, classifying/dispatching every newly peer_stream_opened
// stream by role, decoding and applying whatever new bytes arrived on any
// already-known stream, retrying blocked field sections, and translating
// QuicEvents into H3Events.
pub fn (mut h H3Conn) poll(incoming ?[]u8, now u64) !H3PollResult {
	qc_result := h.qc.poll(incoming, now)!
	return h.drive(qc_result)!
}

// process_timeouts mirrors poll(), for when a previously-returned
// next_timeout deadline elapses with no new datagram.
pub fn (mut h H3Conn) process_timeouts(now u64) !H3PollResult {
	qc_result := h.qc.process_timeouts(now)!
	return h.drive(qc_result)!
}

// drive is poll()/process_timeouts()' shared body.
fn (mut h H3Conn) drive(qc_result PollResult) !H3PollResult {
	mut result := H3PollResult{
		outgoing: qc_result.outgoing
		next_timeout: qc_result.next_timeout
	}
	h.handle_quic_events(qc_result.events, mut result)!
	if !h.closed {
		h.open_own_streams_if_ready()!
		h.drain_known_peer_streams(mut result)!
	}
	h.prune_terminal_dead_streams()
	return result
}

// handle_quic_events translates each QuicEvent into this layer's own
// consequences. peer_stream_opened stream IDs are queued into
// pending_peer_uni_headers so drain_known_peer_streams' next pass reads
// and classifies them -- classification needs actual header BYTES, which
// may not have arrived yet even though the stream is now known to exist.
fn (mut h H3Conn) handle_quic_events(events []QuicEvent, mut result H3PollResult) ! {
	for ev in events {
		match ev.kind {
			.connection_closed {
				h.closed = true
				result.events << H3Event{
					kind: .connection_error
					error_code: ev.error_code
					reason: ev.reason
				}
			}
			.peer_stream_opened {
				stream_id := ev.stream_id or { continue }
				id := StreamId{
					value: stream_id
				}
				if id.direction() == .unidirectional {
					h.pending_peer_uni_headers[stream_id] = []u8{}
				}
				// A peer-initiated BIDI stream has no defined use in HTTP/3's
				// client role (request streams are always client-initiated,
				// RFC 9114 §6.1) -- ignored rather than acted on, same
				// tolerate-and-do-nothing posture as an unknown uni stream
				// type, since Table 1 grants it no legal frame types either.
			}
			.handshake_confirmed {}
		}
	}
}

// open_own_streams_if_ready queues this endpoint's own control stream
// (header + SETTINGS) and both QPACK streams (header only -- nothing to
// say yet) once this connection is established. Queue-now-drain-later,
// matching QuicConn.write_stream's own contract: the actual bytes reach
// the wire on the NEXT poll()/process_timeouts() call, not this one.
//
// Each of the three streams is opened independently, gated on its own ID
// still being none -- NOT on one shared "already attempted" flag. QUIC's
// own peer-imposed unidirectional stream limit (RFC 9000 §4.6) could in
// principle be small enough that the second or third open_stream() call
// here fails (STREAM_LIMIT) even though the first succeeded; a single
// shared flag set before any of the three attempts would then permanently
// strand the connection with fewer than 3 streams opened and this
// function never trying again on a later poll(), since open_stream()
// mints a fresh ID on every call and is not itself safe to retry
// blindly. Gating per-stream on its own optional ID makes a partial
// failure naturally resumable: already-opened streams are never reopened,
// and only the ones still missing are retried next time.
fn (mut h H3Conn) open_own_streams_if_ready() ! {
	if !h.established() {
		return
	}
	if h.own_control_stream_id == none {
		control_id := h.qc.open_stream(false)!
		h.own_control_stream_id = control_id
		mut control_bytes := encode_h3_control_stream_header()!
		control_bytes << encode_settings_frame(h.own_settings)!
		h.qc.write_stream(control_id, control_bytes, false)!
	}
	if h.own_qpack_encoder_stream_id == none {
		enc_id := h.qc.open_stream(false)!
		h.own_qpack_encoder_stream_id = enc_id
		h.qc.write_stream(enc_id, encode_qpack_encoder_stream_header()!, false)!
	}
	if h.own_qpack_decoder_stream_id == none {
		dec_id := h.qc.open_stream(false)!
		h.own_qpack_decoder_stream_id = dec_id
		h.qc.write_stream(dec_id, encode_qpack_decoder_stream_header()!, false)!
	}
}

// drain_known_peer_streams reads and processes any new bytes on every
// peer stream this connection currently knows about: unclassified uni
// streams (attempt classification), the peer's control/QPACK streams
// (dispatch to their dedicated handlers, h3_control_stream.v/h3_qpack_
// streams.v), and every open request stream (h3_request_stream.v's state
// machine via dispatch_request_stream_frames).
fn (mut h H3Conn) drain_known_peer_streams(mut result H3PollResult) ! {
	// Snapshot the key list before iterating: pump_pending_uni_header can
	// delete from pending_peer_uni_headers as a stream gets classified,
	// which V does not guarantee is safe to do while ranging over the same
	// map directly.
	for stream_id in h.pending_peer_uni_headers.keys() {
		h.pump_pending_uni_header(stream_id)!
	}

	if control_id := h.peer_control_stream_id {
		h.drive_peer_control_stream(control_id, mut result)!
	}
	if enc_id := h.peer_qpack_encoder_stream_id {
		h.drive_peer_qpack_encoder_stream(enc_id, mut result)!
	}
	if dec_id := h.peer_qpack_decoder_stream_id {
		h.drive_peer_qpack_decoder_stream(dec_id)!
	}
	// ignored_peer_streams (push/reserved/unknown uni streams) are
	// deliberately never read again -- see the field's own doc comment; no
	// loop over it belongs here at all.
	for stream_id in h.request_streams.keys() {
		if stream_id in h.dead_request_streams {
			continue
		}
		h.dispatch_request_stream_frames(stream_id, mut result)!
	}
	h.retry_blocked_sections(mut result)!
}

// pump_pending_uni_header reads any new bytes on a not-yet-classified
// peer uni stream and attempts to complete its header. Once classified,
// routes the stream into the right typed slot (erroring, per RFC 9114
// §6.2.1/RFC 9204's own uniqueness rule, if it is a SECOND control/QPACK-
// encoder/QPACK-decoder stream) and forwards any bytes that arrived past
// the header to that stream's real handler immediately, rather than
// waiting for the next poll() cycle to notice them.
fn (mut h H3Conn) pump_pending_uni_header(stream_id u64) ! {
	mut buf := h.pending_peer_uni_headers[stream_id] or { return }
	new_bytes := h.qc.read_stream(stream_id)!
	if new_bytes.len > 0 {
		buf << new_bytes
	}
	if u64(buf.len) > max_h3_uni_stream_header_buffered_bytes {
		return error_with_code('h3: peer unidirectional stream ${stream_id} sent more than ${max_h3_uni_stream_header_buffered_bytes} bytes without completing its type header', int(H3ErrorCode.general_protocol_error))
	}
	kind, consumed := classify_peer_uni_stream_header(buf) or {
		h.pending_peer_uni_headers[stream_id] = buf
		return
	}
	rest := buf[consumed..].clone()
	h.pending_peer_uni_headers.delete(stream_id)

	match kind.group {
		.qpack {
			h.qpack_stream_registry.note_stream_opened(kind.q_kind) or {
				return error_with_code('h3: ${err.msg()}', int(H3ErrorCode.stream_creation_error))
			}
			match kind.q_kind {
				.encoder {
					h.peer_qpack_encoder_stream_id = stream_id
					h.peer_qpack_encoder_buf << rest
				}
				.decoder {
					h.peer_qpack_decoder_stream_id = stream_id
					h.peer_qpack_decoder_buf << rest
				}
			}
		}
		.h3 {
			match kind.h3_kind {
				.control {
					if h.peer_control_stream_id != none {
						return error_with_code('h3: peer opened a second control stream', int(H3ErrorCode.stream_creation_error))
					}
					h.peer_control_stream_id = stream_id
					h.peer_control_decoder.push(rest)
				}
				.push, .reserved, .unknown {
					// RFC 9114 §6.2/§6.2.3/§9: never an error. A v1 client never
					// authorizes push (see this file's module doc comment), so a
					// push stream is exactly as inert to us as a genuinely
					// unrecognized type.
					h.ignored_peer_streams[stream_id] = true
				}
			}
		}
	}
}

// require_valid_frame_for_role rejects any frame RFC 9114 §7 Table 1 does
// not permit on a stream playing `role` (is_h3_frame_valid_on_stream,
// Phase 10) as a CONNECTION error -- mirrors how a malformed frame
// envelope from H3FrameDecoder.next() is always connection-scoped (its own
// doc comment): a wrong-frame-type-for-this-role violation reflects a
// fundamental confusion about what the peer thinks this stream is, not a
// fault isolated to one request/response exchange.
fn (mut h H3Conn) require_valid_frame_for_role(frame H3Frame, role H3StreamRole) ! {
	if !is_h3_frame_valid_on_stream(frame, role) {
		return error_with_code('h3: frame type is not valid for this stream role', int(H3ErrorCode.frame_unexpected))
	}
}

// -----------------------------------------------------------------------
// Request-stream API (queue-now-drain-later, mirroring QuicConn's own
// open_stream/write_stream/read_stream shape). Field lines only -- no
// HTTP-semantic awareness; pseudo-headers are just ordinary QpackFieldLine
// entries, matching QPACK's own lack of a separate pseudo-header concept.
// net.http (Phase 12d) owns turning a Request/Response into/out of these.
// -----------------------------------------------------------------------

// open_request_stream opens a new client-initiated bidirectional request
// stream and begins tracking its response message-framing state.
pub fn (mut h H3Conn) open_request_stream() !u64 {
	stream_id := h.qc.open_stream(true)!
	h.request_streams[stream_id] = new_h3_request_stream_state()
	h.request_decoders[stream_id] = new_h3_frame_decoder()
	return stream_id
}

// send_request_headers QPACK-encodes `lines` and queues the resulting
// HEADERS frame onto `stream_id` (opened via open_request_stream). Errors
// if this connection's own QPACK encoder stream has not been opened yet
// (open_own_streams_if_ready runs once this connection reaches
// .established) -- calling this any earlier would silently drop any new
// dynamic-table-insert instructions QPACK needs to send alongside the
// field section, corrupting the peer's mirrored table without it ever
// knowing an insertion happened.
pub fn (mut h H3Conn) send_request_headers(stream_id u64, lines []QpackFieldLine, fin bool) ! {
	enc_id := h.own_qpack_encoder_stream_id or {
		return error('h3: cannot send request headers before this connection has opened its own QPACK encoder stream (wait for established())')
	}

	encoded := h.qpack_encoder.encode_field_section(stream_id, lines)!
	if encoded.encoder_instructions.len > 0 {
		h.qc.write_stream(enc_id, encoded.encoder_instructions, false)!
	}
	frame_bytes := encode_headers_frame(encoded.field_section)!
	h.qc.write_stream(stream_id, frame_bytes, fin)!
}

// send_request_data queues a DATA frame onto `stream_id`.
pub fn (mut h H3Conn) send_request_data(stream_id u64, data []u8, fin bool) ! {
	frame_bytes := encode_data_frame(data)!
	h.qc.write_stream(stream_id, frame_bytes, fin)!
}

// -----------------------------------------------------------------------
// Request-stream response dispatch
// -----------------------------------------------------------------------

// dispatch_request_stream_frames reads any new bytes on `stream_id`,
// decodes as many complete frames as are available, and drives both
// h3_request_stream.v's message-framing state machine and QPACK field-
// section decoding for each. A message-framing or QPACK decompression
// failure on THIS stream is request-scoped (RFC 9204 §2.2.3 explicitly
// calls a field-section decode failure "a stream error of type
// QPACK_DECOMPRESSION_FAILED" -- QPACK's dynamic table is mutated only by
// separate encoder-stream instructions, already applied independently, so
// a decode failure on one section does not desync it the way an HPACK
// failure would) -- reported as a request_error event and the stream
// marked dead, never propagated as a connection-level error.
fn (mut h H3Conn) dispatch_request_stream_frames(stream_id u64, mut result H3PollResult) ! {
	mut decoder := h.request_decoders[stream_id] or { return }
	new_bytes := h.qc.read_stream(stream_id)!
	if new_bytes.len > 0 {
		decoder.push(new_bytes)
	}
	for {
		decoded := decoder.next()!
		if !decoded.has_frame {
			break
		}
		h.require_valid_frame_for_role(decoded.frame, .request)!
		match decoded.frame {
			PushPromiseFrame {
				// Table-1-legal on a request stream, but v1 never authorizes
				// push -- request-scoped here (tied to this one exchange),
				// unlike the identical rejection on the control stream, which
				// is connection-scoped (MAX_PUSH_ID there implies a peer that
				// may be confused about push in general, not just this
				// request).
				h.fail_request_stream(stream_id, H3ErrorCode.id_error.code(), 'push is never authorized by this client', mut result)
				return
			}
			DataFrame {
				mut state := h.request_streams[stream_id] or { return }
				state.note_frame_kind(false, true) or {
					h.fail_request_stream(stream_id, u64(err.code()), err.msg(), mut result)
					return
				}
				result.events << H3Event{
					kind: .response_data
					stream_id: stream_id
					data: decoded.frame.data
				}
			}
			HeadersFrame {
				mut state := h.request_streams[stream_id] or { return }
				is_trailers := state.phase() == .in_body
				state.note_frame_kind(true, false) or {
					h.fail_request_stream(stream_id, u64(err.code()), err.msg(), mut result)
					return
				}
				h.decode_or_queue_headers(stream_id, decoded.frame.encoded_field_section, is_trailers, mut result)!
			}
			else {
				// require_valid_frame_for_role already restricted a
				// .request-role frame to DataFrame/HeadersFrame/
				// PushPromiseFrame -- every other H3Frame variant is
				// unreachable here.
			}
		}
	}
	h.finalize_request_stream_if_done(stream_id, mut result)!
}

// decode_or_queue_headers attempts to QPACK-decode one HEADERS frame's
// field section; queues it (BlockedFieldSection) instead of delivering it
// if QPACK reports the section blocked (RFC 9204 §2.1.2/§2.2.1).
fn (mut h H3Conn) decode_or_queue_headers(stream_id u64, buf []u8, is_trailers bool, mut result H3PollResult) ! {
	decoded := h.qpack_decoder.decode_field_section(stream_id, buf) or {
		h.fail_request_stream(stream_id, u64(err.code()), err.msg(), mut result)
		return
	}
	if decoded.blocked {
		h.blocked_sections << BlockedFieldSection{
			stream_id: stream_id
			buf: buf
			is_trailers: is_trailers
		}
		return
	}
	h.deliver_decoded_headers(stream_id, decoded, is_trailers, mut result)!
}

// deliver_decoded_headers relays a successfully decoded field section's
// resulting decoder-stream bytes (a Section Acknowledgment, if any -- see
// QpackDecoder.decode_field_section's own doc comment for when one is
// produced) and emits the matching H3Event.
//
// A `!is_trailers` block (a response-headers CANDIDATE -- the first HEADERS
// this stream has seen, or another one arriving while still awaiting the
// final response) is inspected for RFC 9110 §15.2's 1xx informational
// range: a well-formed 1xx :status is discarded here entirely (this v1
// client has no Expect:100-continue/early-hints support to hand it to) and
// the stream's phase is left unchanged, so the NEXT HEADERS block -- final
// or another interim one -- is still treated as a response-headers
// candidate rather than trailers. Only once a non-1xx (or malformed/
// missing, left for net.http's own :status validation to reject) status is
// seen does this advance the phase and emit `.response_headers`. Mirrors
// h2_mux_conn.v's identical "discard 1xx, keep waiting for the real
// response" handling for HTTP/2.
fn (mut h H3Conn) deliver_decoded_headers(stream_id u64, decoded QpackDecodeFieldSectionResult, is_trailers bool, mut result H3PollResult) ! {
	if decoded.decoder_instructions.len > 0 {
		if dec_id := h.own_qpack_decoder_stream_id {
			h.qc.write_stream(dec_id, decoded.decoder_instructions, false)!
		}
	}
	if !is_trailers && h3_status_is_informational(decoded.lines) {
		return
	}
	if !is_trailers {
		mut state := h.request_streams[stream_id] or { return }
		state.note_final_response_headers()
	}
	result.events << H3Event{
		kind: if is_trailers {
			H3EventKind.response_trailers
		} else {
			H3EventKind.response_headers
		}
		stream_id: stream_id
		headers: decoded.lines
	}
}

// h3_status_is_informational reports whether `lines` carries a well-formed
// RFC 9110 §15.2 1xx informational :status (100-199). A missing or
// malformed :status is deliberately NOT treated as informational -- it is
// left for net.http's own :status validation (h3_mux_conn.v's wait_
// response) to reject, matching this layer's existing division of labor
// (it never itself rejects a malformed :status).
fn h3_status_is_informational(lines []QpackFieldLine) bool {
	for f in lines {
		if f.name == ':status' {
			return f.value.len == 3 && f.value[0] == `1` && h3_value_all_digits(f.value)
		}
	}
	return false
}

fn h3_value_all_digits(s string) bool {
	if s.len == 0 {
		return false
	}
	for c in s {
		if c < `0` || c > `9` {
			return false
		}
	}
	return true
}

// retry_blocked_sections re-attempts every currently blocked field
// section -- called after every successful encoder-stream instruction
// application (h3_qpack_streams.v), since any one of them can be what
// finally satisfies a blocked section's Required Insert Count.
fn (mut h H3Conn) retry_blocked_sections(mut result H3PollResult) ! {
	if h.blocked_sections.len == 0 {
		return
	}
	mut still_blocked := []BlockedFieldSection{}
	mut resolved_streams := []u64{}
	for section in h.blocked_sections {
		if section.stream_id in h.dead_request_streams {
			continue
		}
		decoded := h.qpack_decoder.decode_field_section(section.stream_id, section.buf) or {
			h.fail_request_stream(section.stream_id, u64(err.code()), err.msg(), mut result)
			continue
		}
		if decoded.blocked {
			still_blocked << section
			continue
		}
		h.deliver_decoded_headers(section.stream_id, decoded, section.is_trailers, mut result)!
		resolved_streams << section.stream_id
	}
	h.blocked_sections = still_blocked
	for stream_id in resolved_streams {
		h.finalize_request_stream_if_done(stream_id, mut result)!
	}
}

// finalize_request_stream_if_done checks whether `stream_id`'s receive
// side has reached a terminal condition (12a's QuicConn.stream_recv_
// status) AND every consequence of that has actually been drained --
// every already-arrived frame decoded (H3FrameDecoder.pending_len() == 0)
// and no blocked field section still outstanding for it -- before
// reporting it done. A reset is always immediately terminal (nothing left
// to wait for); a FIN is only terminal once nothing is left pending.
fn (mut h H3Conn) finalize_request_stream_if_done(stream_id u64, mut result H3PollResult) ! {
	if stream_id in h.dead_request_streams {
		return
	}
	status := h.qc.stream_recv_status(stream_id) or { return }
	match status.state {
		.reset_received {
			h.fail_request_stream(stream_id, status.reset_error or {
				H3ErrorCode.request_cancelled.code()
			}, 'stream reset by peer', mut result)
		}
		.fin_received {
			decoder := h.request_decoders[stream_id] or { return }
			if decoder.pending_len() > 0 {
				return
			}
			if h.has_blocked_section_for(stream_id) {
				return
			}
			mut state := h.request_streams[stream_id] or { return }
			if state.phase() == .done {
				return
			}
			state.note_fin()
			result.events << H3Event{
				kind: .response_ended
				stream_id: stream_id
			}
			// This stream will never be dispatched to again (note_fin is
			// terminal, and finalize_request_stream_if_done's own guards
			// above already require the receive side to be fully drained):
			// request_streams/request_decoders would otherwise grow with
			// the TOTAL number of requests ever opened on a long-lived
			// pooled connection, not the number currently in flight (see
			// fail_request_stream's identical pruning, and its own doc
			// comment, for the failure-path counterpart of this).
			h.request_streams.delete(stream_id)
			h.request_decoders.delete(stream_id)
		}
		.open {}
	}
}

// fail_request_stream marks `stream_id` dead (no further frames on it are
// ever processed again -- this module has no per-stream RST_STREAM/
// STOP_SENDING send API yet, see PROGRESS.md's documented follow-up),
// discards any blocked section still queued for it, and reports the
// failure as a request-scoped event.
fn (mut h H3Conn) fail_request_stream(stream_id u64, error_code u64, reason string, mut result H3PollResult) {
	h.dead_request_streams[stream_id] = true
	mut kept := []BlockedFieldSection{}
	for section in h.blocked_sections {
		if section.stream_id != stream_id {
			kept << section
		}
	}
	h.blocked_sections = kept
	result.events << H3Event{
		kind: .request_error
		stream_id: stream_id
		error_code: error_code
		reason: reason
	}
	// request_streams/request_decoders would otherwise grow with the total
	// number of requests ever opened on a long-lived pooled connection, not
	// the number currently in flight -- see the identical pruning (and this
	// same reasoning) in finalize_request_stream_if_done's success path.
	// dead_request_streams itself is NOT deleted here (unlike these two):
	// the peer may still legally send more STREAM frames for `stream_id`
	// after this call (this module has no per-stream RST_STREAM/
	// STOP_SENDING send API yet, see this function's own doc comment), and
	// drain_known_peer_streams/retry_blocked_sections/finalize_request_
	// stream_if_done all rely on membership here to keep ignoring those --
	// deleting it now would let a same-poll-cycle re-entry into
	// finalize_request_stream_if_done (from dispatch_request_stream_frames
	// falling through after this call, e.g. via decode_or_queue_headers)
	// double-fail this same stream. prune_terminal_dead_streams() removes
	// the entry once the QUIC layer confirms no further frames can ever
	// arrive for it, bounding this map instead of leaving it permanent.
	h.request_streams.delete(stream_id)
	h.request_decoders.delete(stream_id)
}

// prune_terminal_dead_streams drops any dead_request_streams entry whose
// underlying QUIC receive side has itself reached a terminal state
// (reset_recvd/reset_read or size_known/data_recvd/data_read): once that
// has happened, the transport guarantees no further STREAM frames for that
// ID can ever arrive (RFC 9000 -- a stream ID is never reused within a
// connection), so this layer no longer needs to remember it was locally
// failed. Runs once per drive() cycle, strictly after drain_known_peer_
// streams/retry_blocked_sections have both finished processing this
// cycle's frames -- never mid-dispatch, so it cannot retire an ID out from
// under fail_request_stream's own same-poll-cycle reentrancy guard in
// finalize_request_stream_if_done. Bounds dead_request_streams to the
// number of failed streams the peer hasn't finished draining yet, instead
// of the total ever failed over the connection's lifetime (see fail_
// request_stream's own doc comment for why the entry can't just be
// deleted immediately on failure). If the QUIC layer has no record of the
// ID at all (stream_recv_status returns none), the entry is left alone
// rather than guessed at -- a rare, harmless conservative miss, not a
// correctness risk.
fn (mut h H3Conn) prune_terminal_dead_streams() {
	mut terminal := []u64{}
	for stream_id, _ in h.dead_request_streams {
		status := h.qc.stream_recv_status(stream_id) or { continue }
		if status.state == .reset_received || status.state == .fin_received {
			terminal << stream_id
		}
	}
	for stream_id in terminal {
		h.dead_request_streams.delete(stream_id)
	}
}

// has_blocked_section_for reports whether any queued blocked field
// section still belongs to `stream_id`.
fn (h &H3Conn) has_blocked_section_for(stream_id u64) bool {
	for section in h.blocked_sections {
		if section.stream_id == stream_id {
			return true
		}
	}
	return false
}
