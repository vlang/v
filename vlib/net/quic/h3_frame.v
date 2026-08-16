module quic

// HTTP/3 framing layer (RFC 9114 §7). Unlike QUIC's own frames (frame.v),
// which are parsed once from an already-fully-buffered, already-decrypted
// packet payload, HTTP/3 frames ride inside QUIC STREAM frame byte streams
// and "can span multiple [QUIC] packets" (§7, explicitly called out). This
// file is split into three layers to match that:
//   1. the wire-format frame Type/Length header (§7.1) plus per-frame-type
//      payload structs and their encode/decode (§7.2.1-7.2.8);
//   2. `H3FrameDecoder`, a small stateful incremental reader that buffers
//      partial bytes across calls and only ever hands back a frame once its
//      FULL declared Length is available -- callers (Phase 12) feed it
//      arbitrarily-sized chunks as they arrive from `StreamReassembler`'s
//      already-in-order byte stream (no offset-based reordering is needed
//      here, unlike stream_reassembly.v/crypto_stream.v, since QUIC's own
//      stream layer already guarantees in-order delivery by the time bytes
//      reach this layer).
//
// Scope note (mirrors this module's existing role-scoping, e.g. ecn.v/
// stateless_reset.v): only DATA, HEADERS, CANCEL_PUSH, SETTINGS, GOAWAY,
// and MAX_PUSH_ID get an ENCODER, since those are the only frame types a
// v1 client role ever originates (§7.2.5: "A client MUST NOT send a
// PUSH_PROMISE frame" -- omitted here by not providing an encoder for it,
// rather than a runtime check with no reachable caller to exercise it).
// Every frame type still gets a DECODER, since a client must be able to
// recognize (and, at the Phase 12 wiring layer, correctly reject) any
// frame type table 1 permits on a given stream, regardless of who
// legitimately originates it.

// --- Frame type registry (RFC 9114 §11.2.1, Table 2) ---

pub const h3_frame_data = u64(0x00)
pub const h3_frame_headers = u64(0x01)
pub const h3_frame_cancel_push = u64(0x03)
pub const h3_frame_settings = u64(0x04)
pub const h3_frame_push_promise = u64(0x05)
pub const h3_frame_goaway = u64(0x07)
pub const h3_frame_max_push_id = u64(0x0d)

// h3_reserved_h2_carryover_frame_types are frame type codepoints HTTP/2
// used with no HTTP/3 equivalent (§7.2.8/§11.2.1 Table 2: PRIORITY=0x02,
// PING=0x06, WINDOW_UPDATE=0x08, CONTINUATION=0x09). These are NOT part of
// the shared 0x1f*N+0x21 grease sequence -- they MUST NOT be sent, and
// receipt MUST be treated as a connection error of type H3_FRAME_UNEXPECTED
// (distinct from an ordinary unrecognized/grease type, which must be
// silently ignored).
const h3_reserved_h2_carryover_frame_types = [u64(0x02), u64(0x06), u64(0x08), u64(0x09)]

fn is_h3_reserved_h2_carryover_frame_type(frame_type u64) bool {
	return frame_type in h3_reserved_h2_carryover_frame_types
}

// --- Per-frame-type payload structs (RFC 9114 §7.2.1-7.2.8) ---

// DataFrame carries content bytes (§7.2.1). Opaque to this layer -- no
// interpretation of the bytes happens here.
pub struct DataFrame {
pub:
	data []u8
}

// HeadersFrame carries a QPACK-encoded field section (§7.2.2). QPACK
// decoding is Phase 11's responsibility; this layer only extracts the
// still-encoded bytes.
pub struct HeadersFrame {
pub:
	encoded_field_section []u8
}

// CancelPushFrame identifies a server push to cancel by push ID (§7.2.3).
pub struct CancelPushFrame {
pub:
	push_id u64
}

// H3Setting is one (identifier, value) pair from a SETTINGS frame's
// payload (§7.2.4, Figure 7's inner "Setting" struct).
pub struct H3Setting {
pub:
	identifier u64
	value      u64
}

// SettingsFrame carries zero or more configuration parameters (§7.2.4).
// `settings` includes every syntactically valid pair the payload
// contained, INCLUDING grease and other unrecognized identifiers --
// deciding what to actually apply is a Phase 12 concern once real
// connection state exists to apply settings to (see decode_h3_frame_payload
// doc comment for what IS rejected outright at parse time).
pub struct SettingsFrame {
pub:
	settings []H3Setting
}

// PushPromiseFrame carries a promised request's push ID and QPACK-encoded
// header section (§7.2.5). A v1 client role only ever DECODES this (see
// this file's module-level scope note); no encoder is provided.
pub struct PushPromiseFrame {
pub:
	push_id               u64
	encoded_field_section []u8
}

// GoawayFrame carries a single generic id whose meaning depends on
// direction: a client-initiated bidirectional QUIC stream ID when sent
// server-to-client, or a push ID when sent client-to-server (§7.2.6).
// Distinguishing the two, and validating a received stream-ID-flavored
// GOAWAY, needs connection role context this layer doesn't have -- see
// `goaway_id_is_valid_client_initiated_bidi_stream_id` for the one piece
// of that validation that IS pure/role-independent.
pub struct GoawayFrame {
pub:
	id u64
}

// goaway_id_is_valid_client_initiated_bidi_stream_id reports whether id is
// legal as the stream-ID-flavored form of a GOAWAY frame (server-to-client
// direction): RFC 9000 §2.1 encodes a client-initiated bidirectional
// stream ID as id%4==0. RFC 9114 §7.2.6: "A client MUST treat receipt of a
// GOAWAY frame containing a stream ID of any other type as a connection
// error of type H3_ID_ERROR." This is a pure, role-independent predicate;
// the actual connection-error action belongs to Phase 12's wiring, once a
// real connection can be role-aware about which GOAWAY variant it expects.
pub fn goaway_id_is_valid_client_initiated_bidi_stream_id(id u64) bool {
	return id % 4 == 0
}

// MaxPushIdFrame sets the maximum push ID a server may use (§7.2.7). A v1
// client role only ever ENCODES this; decoding exists only so a client can
// recognize (and, at Phase 12, reject per §7.2.7 "A client MUST treat the
// receipt of a MAX_PUSH_ID frame as a connection error of type
// H3_FRAME_UNEXPECTED") a server that incorrectly sends one.
pub struct MaxPushIdFrame {
pub:
	push_id u64
}

// H3RawFrame preserves any frame this layer does not assign a dedicated
// struct to: BOTH the reserved-for-grease codepoints (§7.2.8, MUST be
// ignored) and any genuinely unrecognized extension frame type (§9, MUST
// also be ignored, per "Implementations MUST ignore unknown or unsupported
// values in all extensible protocol elements" -- read directly from §9,
// not assumed by analogy with HTTP/2). H2-carryover reserved types
// (`h3_reserved_h2_carryover_frame_types`) are NOT represented this way --
// decoding one of those returns an error instead, since §7.2.8 requires a
// connection error on receipt, not silent tolerance.
pub struct H3RawFrame {
pub:
	frame_type u64
	payload    []u8
}

// H3Frame is any decoded HTTP/3 frame.
pub type H3Frame = CancelPushFrame
	| DataFrame
	| GoawayFrame
	| H3RawFrame
	| HeadersFrame
	| MaxPushIdFrame
	| PushPromiseFrame
	| SettingsFrame

// --- Per-type payload decode (RFC 9114 §7.2.1-7.2.8) ---

// decode_h3_frame_payload decodes `payload` (already known to be exactly
// `frame_type`'s declared Length, per §7.1/§10.8 -- callers such as
// H3FrameDecoder are responsible for that slicing) into a concrete H3Frame.
// Returns an error, carrying an H3ErrorCode via `error_with_code`, for:
//   - an H2-carryover reserved frame type (§7.2.8) -- H3_FRAME_UNEXPECTED;
//   - a SETTINGS frame whose payload is not an exact sequence of
//     (identifier, value) varint pairs (§7.1's generic "terminates before
//     the end of the identified fields" rule) -- H3_FRAME_ERROR;
//   - a SETTINGS frame containing a duplicate identifier -- H3_SETTINGS_ERROR.
//     RFC 9114 §7.2.4 states this as a MAY ("A receiver MAY treat the
//     presence of duplicate setting identifiers as a connection error"),
//     not a MUST; this implementation chooses to enforce it, matching the
//     RFC's own stated rationale elsewhere in the document for preferring
//     strict validation over permissiveness (§4.1.2: "deliberately strict
//     because being permissive can expose implementations to [...]
//     vulnerabilities"). Documented here as a deliberate implementation
//     choice, not presented as a literal MUST;
//   - a SETTINGS frame containing one of the 5 reserved HTTP/2-carryover
//     setting identifiers (§7.2.4.1/§11.2.2 Table 3: 0x00, 0x02, 0x03,
//     0x04, 0x05) -- H3_SETTINGS_ERROR.
// Every other frame type's payload is a fixed small number of varints
// (CANCEL_PUSH/GOAWAY/MAX_PUSH_ID: one; PUSH_PROMISE: one varint + the
// remaining bytes) -- trailing or missing bytes there are likewise
// H3_FRAME_ERROR per §7.1's generic rule.
pub fn decode_h3_frame_payload(frame_type u64, payload []u8) !H3Frame {
	if is_h3_reserved_h2_carryover_frame_type(frame_type) {
		return error_with_code('h3: frame type 0x${frame_type.hex()} is reserved (HTTP/2 carryover) and MUST NOT be sent',
			int(H3ErrorCode.frame_unexpected))
	}
	match frame_type {
		h3_frame_data {
			return DataFrame{
				data: payload.clone()
			}
		}
		h3_frame_headers {
			return HeadersFrame{
				encoded_field_section: payload.clone()
			}
		}
		h3_frame_cancel_push {
			push_id, n := decode_varint(payload) or {
				return error_with_code('h3: CANCEL_PUSH frame payload does not contain a valid push ID: ${err.msg()}',
					int(H3ErrorCode.frame_error))
			}
			if n != payload.len {
				return error_with_code('h3: CANCEL_PUSH frame payload has ${payload.len - n} trailing byte(s) after its push ID',
					int(H3ErrorCode.frame_error))
			}
			return CancelPushFrame{
				push_id: push_id
			}
		}
		h3_frame_settings {
			return decode_h3_settings_payload(payload)!
		}
		h3_frame_push_promise {
			push_id, n := decode_varint(payload) or {
				return error_with_code('h3: PUSH_PROMISE frame payload does not contain a valid push ID: ${err.msg()}',
					int(H3ErrorCode.frame_error))
			}
			return PushPromiseFrame{
				push_id:               push_id
				encoded_field_section: payload[n..].clone()
			}
		}
		h3_frame_goaway {
			id, n := decode_varint(payload) or {
				return error_with_code('h3: GOAWAY frame payload does not contain a valid stream/push ID: ${err.msg()}',
					int(H3ErrorCode.frame_error))
			}
			if n != payload.len {
				return error_with_code('h3: GOAWAY frame payload has ${payload.len - n} trailing byte(s) after its ID',
					int(H3ErrorCode.frame_error))
			}
			return GoawayFrame{
				id: id
			}
		}
		h3_frame_max_push_id {
			push_id, n := decode_varint(payload) or {
				return error_with_code('h3: MAX_PUSH_ID frame payload does not contain a valid push ID: ${err.msg()}',
					int(H3ErrorCode.frame_error))
			}
			if n != payload.len {
				return error_with_code('h3: MAX_PUSH_ID frame payload has ${payload.len - n} trailing byte(s) after its push ID',
					int(H3ErrorCode.frame_error))
			}
			return MaxPushIdFrame{
				push_id: push_id
			}
		}
		else {
			return H3RawFrame{
				frame_type: frame_type
				payload:    payload.clone()
			}
		}
	}
}

// h3_settings_reserved_h2_carryover_ids are the 5 setting identifiers
// HTTP/2 defined with no HTTP/3 equivalent (§7.2.4.1/§11.2.2 Table 3).
// NOTE: 0x01 and 0x07 are QPACK's SETTINGS_QPACK_MAX_TABLE_CAPACITY /
// SETTINGS_QPACK_BLOCKED_STREAMS (RFC 9204, not RFC 9114 itself) -- they
// are deliberately absent from both this reserved list AND from any
// "known" list in this Phase 10 file; Phase 11 will add them as
// recognized identifiers without needing to touch this reserved set.
const h3_settings_reserved_h2_carryover_ids = [u64(0x00), u64(0x02), u64(0x03), u64(0x04), u64(0x05)]

fn is_h3_settings_reserved_h2_carryover_id(id u64) bool {
	return id in h3_settings_reserved_h2_carryover_ids
}

// decode_h3_settings_payload decodes a SETTINGS frame's payload (§7.2.4)
// into its (identifier, value) pairs, enforcing the checks documented on
// decode_h3_frame_payload.
fn decode_h3_settings_payload(payload []u8) !SettingsFrame {
	mut settings := []H3Setting{}
	mut seen_ids := map[u64]bool{}
	mut offset := 0
	for offset < payload.len {
		identifier, id_len := decode_varint(payload[offset..]) or {
			return error_with_code('h3: SETTINGS frame payload has a truncated setting identifier at offset ${offset}: ${err.msg()}',
				int(H3ErrorCode.frame_error))
		}
		value, value_len := decode_varint(payload[offset + id_len..]) or {
			return error_with_code('h3: SETTINGS frame payload has a truncated setting value at offset ${
				offset + id_len}: ${err.msg()}', int(H3ErrorCode.frame_error))
		}
		if is_h3_settings_reserved_h2_carryover_id(identifier) {
			return error_with_code('h3: SETTINGS frame contains reserved (HTTP/2 carryover) identifier 0x${identifier.hex()}',
				int(H3ErrorCode.settings_error))
		}
		if seen_ids[identifier] {
			return error_with_code('h3: SETTINGS frame contains duplicate identifier 0x${identifier.hex()}',
				int(H3ErrorCode.settings_error))
		}
		seen_ids[identifier] = true
		settings << H3Setting{
			identifier: identifier
			value:      value
		}
		offset += id_len + value_len
	}
	return SettingsFrame{
		settings: settings
	}
}

// --- Encode (RFC 9114 §7.1 envelope + §7.2.1-7.2.7 payloads) ---
//
// See this file's module-level scope note for why PUSH_PROMISE has no
// encoder.

// encode_h3_frame_envelope wraps `payload` with its Type/Length header
// (§7.1, Figure 3). `frame_type` MUST NOT be one of
// `h3_reserved_h2_carryover_frame_types` -- callers constructing a frame
// this layer's own encoders produce never pass one of those, so this is
// checked defensively rather than reachable in normal use.
fn encode_h3_frame_envelope(frame_type u64, payload []u8) ![]u8 {
	if is_h3_reserved_h2_carryover_frame_type(frame_type) {
		return error('h3: internal error: refusing to encode reserved frame type 0x${frame_type.hex()}')
	}
	mut out := encode_varint(frame_type)!
	out << encode_varint(u64(payload.len))!
	out << payload
	return out
}

// encode_data_frame encodes a DATA frame (§7.2.1).
pub fn encode_data_frame(data []u8) ![]u8 {
	return encode_h3_frame_envelope(h3_frame_data, data)
}

// encode_headers_frame encodes a HEADERS frame (§7.2.2) around an
// already-QPACK-encoded field section (QPACK encoding is Phase 11's
// responsibility, not this function's).
pub fn encode_headers_frame(encoded_field_section []u8) ![]u8 {
	return encode_h3_frame_envelope(h3_frame_headers, encoded_field_section)
}

// encode_cancel_push_frame encodes a CANCEL_PUSH frame (§7.2.3).
pub fn encode_cancel_push_frame(push_id u64) ![]u8 {
	payload := encode_varint(push_id)!
	return encode_h3_frame_envelope(h3_frame_cancel_push, payload)
}

// encode_settings_frame encodes a SETTINGS frame (§7.2.4) from an ordered
// list of settings. Does not itself enforce the duplicate-identifier or
// reserved-identifier rules decode_h3_settings_payload checks on receipt --
// a sender controls its own output and is trusted not to construct an
// invalid frame; validating here too would only be useful for catching an
// internal bug, not a wire-format concern.
pub fn encode_settings_frame(settings []H3Setting) ![]u8 {
	mut payload := []u8{}
	for s in settings {
		payload << encode_varint(s.identifier)!
		payload << encode_varint(s.value)!
	}
	return encode_h3_frame_envelope(h3_frame_settings, payload)
}

// encode_goaway_frame encodes a GOAWAY frame (§7.2.6). See GoawayFrame's
// doc comment for the direction-dependent meaning of `id`.
pub fn encode_goaway_frame(id u64) ![]u8 {
	payload := encode_varint(id)!
	return encode_h3_frame_envelope(h3_frame_goaway, payload)
}

// encode_max_push_id_frame encodes a MAX_PUSH_ID frame (§7.2.7).
pub fn encode_max_push_id_frame(push_id u64) ![]u8 {
	payload := encode_varint(push_id)!
	return encode_h3_frame_envelope(h3_frame_max_push_id, payload)
}

// --- Incremental/resumable decoding (module-level doc comment) ---

// H3FrameDecodeResult is the result of one H3FrameDecoder.next() call.
// `has_frame` is false whenever `buf`'s buffered bytes do not yet contain a
// complete frame -- NOT an error condition, since RFC 9114 explicitly
// allows a frame's Type/Length/Payload to be split arbitrarily across QUIC
// STREAM frames (§7, "unlike QUIC frames, HTTP/3 frames can span multiple
// packets"). `frame`/`consumed` are meaningless when `has_frame` is false
// (defaults to a zero-value H3RawFrame / 0).
pub struct H3FrameDecodeResult {
pub:
	has_frame bool
	frame     H3Frame = H3RawFrame{}
	consumed  int
}

// H3FrameDecoder incrementally decodes a sequence of HTTP/3 frames from an
// already-in-order byte stream (e.g. the output of a QUIC
// `StreamReassembler.data()`/read cursor). Bytes are pushed as they
// arrive; complete frames are popped one at a time via `next()`.
@[heap]
pub struct H3FrameDecoder {
mut:
	pending []u8
}

// new_h3_frame_decoder allocates an empty incremental frame decoder.
pub fn new_h3_frame_decoder() &H3FrameDecoder {
	return &H3FrameDecoder{}
}

// push appends newly-received stream bytes to the decoder's internal
// buffer. Bytes already fully consumed by a prior `next()` call are never
// re-examined; `push` never itself parses anything.
pub fn (mut d H3FrameDecoder) push(data []u8) {
	d.pending << data
}

// next attempts to decode one complete frame from the front of the
// currently-buffered bytes. Returns `has_frame: false` (not an error) if
// the buffered bytes do not yet contain a full frame; the caller should
// call `push` again once more data has arrived and retry. On success, the
// decoded frame's bytes are removed from the internal buffer, so a
// subsequent `next()` call continues from wherever this one left off.
// Returns an error for a genuine protocol violation (see
// decode_h3_frame_payload's doc comment) -- the offending bytes are left
// in the buffer (not consumed) when this happens, so calling `next()`
// again is harmless and simply reproduces the same error (decoding is a
// pure function of the buffered bytes). That said, per RFC 9114 §8, any
// such error is a CONNECTION error: the caller's real obligation is to
// close the whole HTTP/3 connection, not to keep feeding this decoder.
pub fn (mut d H3FrameDecoder) next() !H3FrameDecodeResult {
	frame_type, type_len := decode_varint(d.pending) or { return H3FrameDecodeResult{} }
	length, length_len := decode_varint(d.pending[type_len..]) or { return H3FrameDecodeResult{} }
	header_len := type_len + length_len
	// `length` is attacker-controlled and can legally be up to 2^62-1
	// (RFC 9000 §16's varint range) -- e.g. a genuinely huge DATA frame
	// declared before its bytes have actually arrived. Do the "do we have
	// enough bytes YET" comparison entirely in u64 space; only convert to
	// `int` once total is proven <= d.pending.len, which is itself a real,
	// physically-bounded int already. Casting `length` to `int` BEFORE
	// this comparison would silently truncate/wrap on any value above
	// ~2^31, making total wrong (possibly negative) and either falsely
	// reporting "frame ready" on too few bytes or panicking on a negative
	// slice bound.
	total_u64 := u64(header_len) + length
	if total_u64 > u64(d.pending.len) {
		return H3FrameDecodeResult{}
	}
	total := int(total_u64)
	payload := d.pending[header_len..total]
	frame := decode_h3_frame_payload(frame_type, payload)!
	d.pending.delete_many(0, total)
	return H3FrameDecodeResult{
		has_frame: true
		frame:     frame
		consumed:  total
	}
}

// pending_len returns how many not-yet-decoded bytes are currently
// buffered -- useful for a caller wanting to bound how much unconsumed
// data it will let a peer accumulate before a complete frame arrives
// (mirrors crypto_stream.v's max_crypto_stream_buffered_bytes rationale;
// no such cap is enforced BY this file itself -- see the "Known design
// notes" row this phase's matrix section adds on why DATA frames in
// particular must stay uncapped here).
pub fn (d &H3FrameDecoder) pending_len() int {
	return d.pending.len
}
