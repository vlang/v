module quic

// This file is the pure wire codec for the 3 instructions sent on a QPACK
// decoder stream (RFC 9204 §4.4). Applying one (updating an encoder's
// Known Received Count / releasing references) is `qpack_encoder.v`'s job,
// since an encoder is what receives and acts on decoder instructions --
// mirrors `qpack_encoder_instructions.v`'s codec/apply split.

// QpackSectionAck is the Section Acknowledgment instruction (RFC 9204
// §4.4.1): the decoder has finished processing the earliest unacknowledged
// encoded field section with dynamic-table references on `stream_id`.
pub struct QpackSectionAck {
pub:
	stream_id u64
}

// QpackStreamCancellation is the Stream Cancellation instruction (RFC 9204
// §4.4.2): `stream_id` was reset or abandoned before all its encoded field
// sections were processed.
pub struct QpackStreamCancellation {
pub:
	stream_id u64
}

// QpackInsertCountIncrement is the Insert Count Increment instruction (RFC
// 9204 §4.4.3): increase Known Received Count by `increment`.
pub struct QpackInsertCountIncrement {
pub:
	increment u64
}

// QpackDecoderInstruction is any one of the 3 decoder-stream instructions.
pub type QpackDecoderInstruction = QpackInsertCountIncrement
	| QpackSectionAck
	| QpackStreamCancellation

// decode_qpack_decoder_instruction decodes ONE instruction starting at
// `buf[0]` (RFC 9204 Figures 9-11: `1...`=Section-Ack, `01..`=Stream-
// Cancellation, `00..`=Insert-Count-Increment). Returns `none`, not an
// error, when `buf` does not yet hold a complete instruction -- same
// resumable-parsing contract as `decode_qpack_encoder_instruction`. On
// success, also returns the number of bytes consumed.
pub fn decode_qpack_decoder_instruction(buf []u8) ?(QpackDecoderInstruction, int) {
	if buf.len == 0 {
		return none
	}
	first := buf[0]
	if first & 0x80 != 0 {
		stream_id, len := decode_prefixed_int(buf, 7) or { return none }
		return QpackDecoderInstruction(QpackSectionAck{
			stream_id: stream_id
		}), len
	}
	if first & 0x40 != 0 {
		stream_id, len := decode_prefixed_int(buf, 6) or { return none }
		return QpackDecoderInstruction(QpackStreamCancellation{
			stream_id: stream_id
		}), len
	}
	increment, len := decode_prefixed_int(buf, 6) or { return none }
	return QpackDecoderInstruction(QpackInsertCountIncrement{
		increment: increment
	}), len
}

// encode_qpack_section_ack encodes a Section Acknowledgment instruction
// (RFC 9204 §4.4.1, Figure 9: `1` + 7-bit prefix stream ID).
pub fn encode_qpack_section_ack(stream_id u64) []u8 {
	mut out := []u8{}
	encode_prefixed_int(mut out, stream_id, 7, 0x80)
	return out
}

// encode_qpack_stream_cancellation encodes a Stream Cancellation
// instruction (RFC 9204 §4.4.2, Figure 10: `01` + 6-bit prefix stream ID).
pub fn encode_qpack_stream_cancellation(stream_id u64) []u8 {
	mut out := []u8{}
	encode_prefixed_int(mut out, stream_id, 6, 0x40)
	return out
}

// encode_qpack_insert_count_increment encodes an Insert Count Increment
// instruction (RFC 9204 §4.4.3, Figure 11: `00` + 6-bit prefix increment).
pub fn encode_qpack_insert_count_increment(increment u64) []u8 {
	mut out := []u8{}
	encode_prefixed_int(mut out, increment, 6, 0x00)
	return out
}
