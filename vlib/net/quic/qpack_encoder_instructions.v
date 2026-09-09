module quic

// This file is the pure wire codec for the 4 instructions sent on a QPACK
// encoder stream (RFC 9204 §4.3). It has no dynamic-table state of its own
// -- decoding one of these bytes into a struct is a separate step from
// APPLYING it (mutating a `QpackDynamicTable`), which is `qpack_decoder.v`'s
// job, since a decoder is what receives and acts on encoder instructions.

// QpackSetDynamicTableCapacity is the Set Dynamic Table Capacity
// instruction (RFC 9204 §4.3.1).
pub struct QpackSetDynamicTableCapacity {
pub:
	capacity u64
}

// QpackInsertWithNameRef is the Insert With Name Reference instruction
// (RFC 9204 §4.3.2). `name_index` is a static table index when `is_static`,
// otherwise a relative index in the ENCODER-INSTRUCTION context (RFC 9204
// §3.2.5, Figure 2 -- relative to the most recently inserted entry, not to
// a field section's Base).
pub struct QpackInsertWithNameRef {
pub:
	is_static  bool
	name_index u64
	value      string
}

// QpackInsertWithLiteralName is the Insert With Literal Name instruction
// (RFC 9204 §4.3.3).
pub struct QpackInsertWithLiteralName {
pub:
	name  string
	value string
}

// QpackDuplicate is the Duplicate instruction (RFC 9204 §4.3.4).
// `rel_index` is a relative index in the same encoder-instruction context
// as `QpackInsertWithNameRef.name_index`.
pub struct QpackDuplicate {
pub:
	rel_index u64
}

// QpackEncoderInstruction is any one of the 4 encoder-stream instructions.
pub type QpackEncoderInstruction = QpackDuplicate
	| QpackInsertWithLiteralName
	| QpackInsertWithNameRef
	| QpackSetDynamicTableCapacity

// QpackDecodedEncoderInstruction is the result of attempting to decode one
// encoder-stream instruction: `has_instruction` is false when `buf` does
// not yet hold a complete instruction (genuinely need more bytes, not an
// error -- see `decode_qpack_encoder_instruction`'s doc comment). `instr`'s
// default value is never meaningful when `has_instruction` is false; V sum
// types require SOME default to satisfy the struct literal.
pub struct QpackDecodedEncoderInstruction {
pub:
	has_instruction bool
	instr           QpackEncoderInstruction = QpackDuplicate{}
	consumed        int
}

// decode_qpack_encoder_instruction decodes ONE instruction starting at
// `buf[0]`, distinguishing the 4 types by their leading bit pattern (RFC
// 9204 Figures 5-8, checked in the order that lets each subsequent branch
// assume the higher bits already ruled out: `1...`=Insert-with-name-ref,
// `01..`=Insert-with-literal-name, `001.`=Set-capacity, `000.`=Duplicate).
// Returns `has_instruction: false` (not an error) when `buf` does not yet
// hold a complete instruction: encoder-stream data can arrive fragmented
// across QUIC STREAM frames the same as HTTP/3 frames do, so this mirrors
// `H3FrameDecoder`'s "need more data" contract rather than treating a
// short read as invalid. Returns a real error -- which the caller MUST
// propagate, not treat as "not yet complete" -- for genuinely malformed
// data (an oversized integer, invalid Huffman coding): conflating the two
// previously let a malformed encoder-stream instruction be silently
// retried forever instead of raising QPACK_ENCODER_STREAM_ERROR and
// closing the connection (RFC 9204 §2.2.3, §3.2.2). Self-found via GPT-5.6
// Luna review, Phase-R reproduced before this fix. Uses the shared
// `qpack_err_need_more_data` sentinel (qpack_primitives.v) to tell the two
// cases apart at every `decode_prefixed_int`/`decode_prefixed_string`
// call site below.
pub fn decode_qpack_encoder_instruction(buf []u8) !QpackDecodedEncoderInstruction {
	if buf.len == 0 {
		return QpackDecodedEncoderInstruction{}
	}
	first := buf[0]
	if first & 0x80 != 0 {
		is_static := first & 0x40 != 0
		name_index, idx_len := decode_prefixed_int(buf, 6) or {
			if err.code() == qpack_err_need_more_data {
				return QpackDecodedEncoderInstruction{}
			}
			return err
		}
		value, _, val_len := decode_prefixed_string(buf[idx_len..], 7) or {
			if err.code() == qpack_err_need_more_data {
				return QpackDecodedEncoderInstruction{}
			}
			return err
		}
		return QpackDecodedEncoderInstruction{
			has_instruction: true
			instr: QpackEncoderInstruction(QpackInsertWithNameRef{
				is_static: is_static
				name_index: name_index
				value: value
			})
			consumed: idx_len + val_len
		}
	}
	if first & 0x40 != 0 {
		name, _, name_len := decode_prefixed_string(buf, 5) or {
			if err.code() == qpack_err_need_more_data {
				return QpackDecodedEncoderInstruction{}
			}
			return err
		}
		value, _, val_len := decode_prefixed_string(buf[name_len..], 7) or {
			if err.code() == qpack_err_need_more_data {
				return QpackDecodedEncoderInstruction{}
			}
			return err
		}
		return QpackDecodedEncoderInstruction{
			has_instruction: true
			instr: QpackEncoderInstruction(QpackInsertWithLiteralName{
				name: name
				value: value
			})
			consumed: name_len + val_len
		}
	}
	if first & 0x20 != 0 {
		capacity, len := decode_prefixed_int(buf, 5) or {
			if err.code() == qpack_err_need_more_data {
				return QpackDecodedEncoderInstruction{}
			}
			return err
		}
		return QpackDecodedEncoderInstruction{
			has_instruction: true
			instr: QpackEncoderInstruction(QpackSetDynamicTableCapacity{
				capacity: capacity
			})
			consumed: len
		}
	}
	rel_index, len := decode_prefixed_int(buf, 5) or {
		if err.code() == qpack_err_need_more_data {
			return QpackDecodedEncoderInstruction{}
		}
		return err
	}
	return QpackDecodedEncoderInstruction{
		has_instruction: true
		instr: QpackEncoderInstruction(QpackDuplicate{
			rel_index: rel_index
		})
		consumed: len
	}
}

// encode_qpack_set_dynamic_table_capacity encodes a Set Dynamic Table
// Capacity instruction (RFC 9204 §4.3.1, Figure 5: `001` + 5-bit prefix).
pub fn encode_qpack_set_dynamic_table_capacity(capacity u64) []u8 {
	mut out := []u8{}
	encode_prefixed_int(mut out, capacity, 5, 0x20)
	return out
}

// encode_qpack_insert_with_name_ref encodes an Insert With Name Reference
// instruction (RFC 9204 §4.3.2, Figure 6: `1` + T + 6-bit prefix name
// index + value string literal). `name_index` must already be resolved to
// the correct context (static index, or encoder-instruction-relative
// dynamic index) by the caller.
pub fn encode_qpack_insert_with_name_ref(is_static bool, name_index u64, value string) []u8 {
	mut out := []u8{}
	high := if is_static { u8(0xC0) } else { u8(0x80) }
	encode_prefixed_int(mut out, name_index, 6, high)
	encode_prefixed_string(mut out, value, 7, 0)
	return out
}

// encode_qpack_insert_with_literal_name encodes an Insert With Literal
// Name instruction (RFC 9204 §4.3.3, Figure 7: `01` + name as a 6-bit
// prefix string literal + value as an 8-bit prefix string literal).
pub fn encode_qpack_insert_with_literal_name(name string, value string) []u8 {
	mut out := []u8{}
	encode_prefixed_string(mut out, name, 5, 0x40)
	encode_prefixed_string(mut out, value, 7, 0)
	return out
}

// encode_qpack_duplicate encodes a Duplicate instruction (RFC 9204 §4.3.4,
// Figure 8: `000` + 5-bit prefix relative index).
pub fn encode_qpack_duplicate(rel_index u64) []u8 {
	mut out := []u8{}
	encode_prefixed_int(mut out, rel_index, 5, 0x00)
	return out
}
