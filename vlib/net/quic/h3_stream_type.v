module quic

// RFC 9114 §6.2 — Unidirectional Streams. The purpose of a unidirectional
// QUIC stream is indicated by a Stream Type varint sent as the first bytes
// on that stream ("Unidirectional Stream Header", Figure 1); the format of
// everything after it depends on that type. This file only decodes the
// header itself -- the two stream types HTTP/3 defines here (control,
// §6.2.1; push, §6.2.2) carry HTTP/3 frames or push-stream data
// respectively after their header, which is a Phase 12 wiring concern once
// a real QuicConn stream registry exists to dispatch into.

// h3_control_stream_type is the Stream Type value that marks a control
// stream (RFC 9114 §6.2.1).
pub const h3_control_stream_type = u64(0x00)

// h3_push_stream_type is the Stream Type value that marks a push stream
// (RFC 9114 §6.2.2); it is immediately followed by the push ID it fulfills.
pub const h3_push_stream_type = u64(0x01)

// H3UnidirectionalStreamKind classifies a decoded Stream Type value.
// `reserved` and `unknown` are deliberately distinct even though RFC 9114
// treats both as "ignore, do not error" (§6.2/§6.2.3): `reserved` values
// exist SPECIFICALLY to be grease, while `unknown` covers a value this
// implementation genuinely does not recognize (e.g. a future extension's
// QPACK-encoder/-decoder stream types, defined by RFC 9204 rather than
// RFC 9114 itself, or any other extension) -- keeping them apart lets a
// caller log/count them differently without treating either as an error.
pub enum H3UnidirectionalStreamKind {
	control
	push
	reserved
	unknown
}

// H3UnidirectionalStreamHeader is the decoded result of the first bytes of
// a unidirectional QUIC stream. `push_id` is only ever set when
// `kind == .push` (RFC 9114 §6.2.2, Figure 2). `consumed` is the number of
// bytes of the input buffer the header occupied, so the caller can advance
// past it to whatever HTTP/3 frames or push-stream data follows.
pub struct H3UnidirectionalStreamHeader {
pub:
	kind     H3UnidirectionalStreamKind
	raw_type u64
	push_id  ?u64
	consumed int
}

// classify_h3_unidirectional_stream_type maps a raw Stream Type varint
// value to its H3UnidirectionalStreamKind per RFC 9114 §6.2/§6.2.1/§6.2.2/
// §6.2.3. Exported separately from the header parser so callers (and
// tests) can classify a value without needing a full byte buffer.
pub fn classify_h3_unidirectional_stream_type(raw_type u64) H3UnidirectionalStreamKind {
	if raw_type == h3_control_stream_type {
		return .control
	}
	if raw_type == h3_push_stream_type {
		return .push
	}
	if is_h3_reserved_codepoint(raw_type) {
		return .reserved
	}
	return .unknown
}

// parse_h3_unidirectional_stream_header attempts to decode the Stream Type
// (and, for a push stream, the Push ID that immediately follows it -- RFC
// 9114 §6.2.2 Figure 2) from the start of `buf`. Returns `none`, not an
// error, when `buf` does not yet hold enough bytes to complete the header:
// RFC 9114 places no requirement on how these header bytes are split across
// QUIC STREAM frames, so a caller reading a fresh unidirectional stream
// incrementally must be able to retry once more data has arrived rather
// than treat a short read as a protocol violation (mirrors this file's
// h3_frame.v incremental frame decoder). On success, the returned header's
// `consumed` field is the number of bytes it occupied at the START of `buf`.
pub fn parse_h3_unidirectional_stream_header(buf []u8) ?H3UnidirectionalStreamHeader {
	raw_type, type_len := decode_varint(buf) or { return none }
	kind := classify_h3_unidirectional_stream_type(raw_type)
	if kind == .push {
		push_id, id_len := decode_varint(buf[type_len..]) or { return none }
		return H3UnidirectionalStreamHeader{
			kind:     .push
			raw_type: raw_type
			push_id:  push_id
			consumed: type_len + id_len
		}
	}
	return H3UnidirectionalStreamHeader{
		kind:     kind
		raw_type: raw_type
		push_id:  none
		consumed: type_len
	}
}

// encode_h3_control_stream_header returns the single-varint header a
// control stream must send as its first bytes (RFC 9114 §6.2.1).
pub fn encode_h3_control_stream_header() ![]u8 {
	return encode_varint(h3_control_stream_type)
}

// encode_h3_push_stream_header returns the Stream Type + Push ID header a
// push stream must send as its first bytes (RFC 9114 §6.2.2, Figure 2).
// `push_id` must already satisfy RFC 9000 §16's varint range (2^62-1);
// encode_varint enforces that and returns an error otherwise.
pub fn encode_h3_push_stream_header(push_id u64) ![]u8 {
	mut out := encode_varint(h3_push_stream_type)!
	out << encode_varint(push_id)!
	return out
}
