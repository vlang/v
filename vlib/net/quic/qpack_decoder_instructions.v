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

// QpackDecodedDecoderInstruction is the result of attempting to decode one
// decoder-stream instruction -- mirrors `QpackDecodedEncoderInstruction`'s
// shape and reasoning exactly (qpack_encoder_instructions.v).
pub struct QpackDecodedDecoderInstruction {
pub:
	has_instruction bool
	instr           QpackDecoderInstruction = QpackSectionAck{}
	consumed        int
}

// decode_qpack_decoder_instruction decodes ONE instruction starting at
// `buf[0]` (RFC 9204 Figures 9-11: `1...`=Section-Ack, `01..`=Stream-
// Cancellation, `00..`=Insert-Count-Increment). Returns
// `has_instruction: false` (not an error) when `buf` does not yet hold a
// complete instruction -- same resumable-parsing contract as
// `decode_qpack_encoder_instruction`, including the same obligation to
// propagate (not swallow) a genuine malformed-data error rather than
// treating it as "not yet complete": this decoder-stream sibling had the
// identical swallow-everything bug (a `?` return type with no error
// channel at all), fixed here for the same reason even though no
// production caller yet exists for it -- the same class of bug, not just
// the one instance a reviewer named.
pub fn decode_qpack_decoder_instruction(buf []u8) !QpackDecodedDecoderInstruction {
	if buf.len == 0 {
		return QpackDecodedDecoderInstruction{}
	}
	first := buf[0]
	if first & 0x80 != 0 {
		stream_id, len := decode_prefixed_int(buf, 7) or {
			if err.code() == qpack_err_need_more_data {
				return QpackDecodedDecoderInstruction{}
			}
			return err
		}
		return QpackDecodedDecoderInstruction{
			has_instruction: true
			instr:           QpackDecoderInstruction(QpackSectionAck{
				stream_id: stream_id
			})
			consumed:        len
		}
	}
	if first & 0x40 != 0 {
		stream_id, len := decode_prefixed_int(buf, 6) or {
			if err.code() == qpack_err_need_more_data {
				return QpackDecodedDecoderInstruction{}
			}
			return err
		}
		return QpackDecodedDecoderInstruction{
			has_instruction: true
			instr:           QpackDecoderInstruction(QpackStreamCancellation{
				stream_id: stream_id
			})
			consumed:        len
		}
	}
	increment, len := decode_prefixed_int(buf, 6) or {
		if err.code() == qpack_err_need_more_data {
			return QpackDecodedDecoderInstruction{}
		}
		return err
	}
	return QpackDecodedDecoderInstruction{
		has_instruction: true
		instr:           QpackDecoderInstruction(QpackInsertCountIncrement{
			increment: increment
		})
		consumed:        len
	}
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
