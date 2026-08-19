module quic

// qpack_encoder_stream_type is the Stream Type value that marks a QPACK
// encoder stream (RFC 9204 §4.2, §8.2). It shares HTTP/3's unidirectional
// Stream Type varint space (the same one `h3_stream_type.v` classifies
// 0x00/0x01 in) but is defined by RFC 9204, not RFC 9114 itself -- kept in
// its own file rather than added to h3_stream_type.v's classifier, mirroring
// how RFC 9204 §5's settings are recognized as "not reserved" by
// h3_frame.v without h3_frame.v itself interpreting them.
pub const qpack_encoder_stream_type = u64(0x02)

// qpack_decoder_stream_type is the Stream Type value that marks a QPACK
// decoder stream (RFC 9204 §4.2, §8.2).
pub const qpack_decoder_stream_type = u64(0x03)

// QpackStreamKind distinguishes the two QPACK stream types.
pub enum QpackStreamKind {
	encoder
	decoder
}

// classify_qpack_stream_type maps a raw unidirectional Stream Type varint
// to a QpackStreamKind, if it is one of QPACK's own two types. Returns
// `none` for anything else (RFC 9114's own types, grease, or a genuinely
// unrecognized extension) -- a caller should try this classifier before
// falling back to `classify_h3_unidirectional_stream_type`, since 0x02/0x03
// would otherwise be reported `.unknown` there (correct as far as RFC 9114
// itself is concerned, but incomplete once QPACK is implemented).
pub fn classify_qpack_stream_type(raw_type u64) ?QpackStreamKind {
	if raw_type == qpack_encoder_stream_type {
		return .encoder
	}
	if raw_type == qpack_decoder_stream_type {
		return .decoder
	}
	return none
}

// encode_qpack_encoder_stream_header returns the single-varint header this
// endpoint's own QPACK encoder stream must send as its first bytes (RFC
// 9204 §4.2) -- mirrors h3_stream_type.v's encode_h3_control_stream_header,
// which this file never got its own copy of until Phase 12 needed one.
pub fn encode_qpack_encoder_stream_header() ![]u8 {
	return encode_varint(qpack_encoder_stream_type)
}

// encode_qpack_decoder_stream_header returns the single-varint header this
// endpoint's own QPACK decoder stream must send as its first bytes (RFC
// 9204 §4.2).
pub fn encode_qpack_decoder_stream_header() ![]u8 {
	return encode_varint(qpack_decoder_stream_type)
}

// QpackStreamRegistry tracks whether a peer has already opened its (at
// most one) QPACK encoder stream and (at most one) QPACK decoder stream.
// Mirrors `h3_message_state.v`'s `H3ControlStreamState` shape exactly (a
// from-scratch sibling comparison against that struct was done before
// writing this one): both are "has a single required-unique stream
// appeared yet" trackers fed by whatever eventually owns the real QUIC
// streams, needing no other connection state to be correct.
@[heap]
pub struct QpackStreamRegistry {
mut:
	seen_encoder bool
	seen_decoder bool
}

// new_qpack_stream_registry returns an empty registry, as at the start of
// a connection before either QPACK stream has been seen.
pub fn new_qpack_stream_registry() QpackStreamRegistry {
	return QpackStreamRegistry{}
}

// note_stream_opened records that the peer opened a stream of QPACK kind
// `kind`. Errors, carrying H3ErrorCode.stream_creation_error, on a second
// stream of the same kind (RFC 9204 §4.2: "Each endpoint MUST initiate, at
// most, one encoder stream and, at most, one decoder stream. Receipt of a
// second instance of either stream type MUST be treated as a connection
// error of type H3_STREAM_CREATION_ERROR" -- this is RFC 9114's own error
// code, not one of QPACK's three from `qpack_error.v`, since the violation
// is of HTTP/3's stream-creation model, not a QPACK decompression or
// instruction-stream failure). The actual connection-close action this
// error should trigger is Phase 12's job, once a real stream registry
// exists to call this from -- same "role-legality only" deferral as every
// analogous row in the HTTP/3 framing conformance-matrix section.
pub fn (mut r QpackStreamRegistry) note_stream_opened(kind QpackStreamKind) ! {
	match kind {
		.encoder {
			if r.seen_encoder {
				return error_with_code('qpack: received a second encoder stream',
					int(H3ErrorCode.stream_creation_error))
			}
			r.seen_encoder = true
		}
		.decoder {
			if r.seen_decoder {
				return error_with_code('qpack: received a second decoder stream',
					int(H3ErrorCode.stream_creation_error))
			}
			r.seen_decoder = true
		}
	}
}
