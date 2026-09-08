module quic

// QpackApplyInstructionResult is the result of processing one encoder-
// stream instruction (RFC 9204 §4.3): whether a complete instruction was
// available, how many bytes it consumed, and any decoder-stream bytes this
// decoder wants to send in response (an Insert Count Increment, per this
// decoder's policy of acknowledging every insertion immediately -- see
// `apply_encoder_instruction`'s doc comment for why).
pub struct QpackApplyInstructionResult {
pub:
	applied              bool
	consumed             int
	decoder_instructions []u8
}

// QpackDecodeFieldSectionResult is the result of decoding one encoded
// field section (RFC 9204 §2.2): either it was blocked (Required Insert
// Count not yet satisfied, RFC 9204 §2.1.2/§2.2.1 -- not an error, the
// caller should retry once more encoder-stream data has arrived) or it
// produced field lines plus any decoder-stream bytes to send (a Section
// Acknowledgment, RFC 9204 §4.4.1).
pub struct QpackDecodeFieldSectionResult {
pub:
	blocked              bool
	lines                []QpackFieldLine
	decoder_instructions []u8
}

// QpackDecoder is a QPACK decoder's state (RFC 9204 §2.2): a dynamic table
// mirroring the encoder's, decoded via the encoder instructions this
// decoder receives, plus this endpoint's own configured maximum dynamic
// table capacity (its own outgoing SETTINGS_QPACK_MAX_TABLE_CAPACITY,
// needed for the Required Insert Count wraparound math, RFC 9204
// §4.5.1.1).
@[heap]
pub struct QpackDecoder {
mut:
	dynamic_table      QpackDynamicTable
	max_table_capacity u64
}

// new_qpack_decoder returns a decoder configured with this endpoint's own
// SETTINGS_QPACK_MAX_TABLE_CAPACITY value (RFC 9204 §5) -- the value THIS
// side sends to its peer, bounding what the peer's encoder may ever set
// the mirrored dynamic table's capacity to (§3.2.3).
pub fn new_qpack_decoder(max_table_capacity u64) QpackDecoder {
	return QpackDecoder{
		max_table_capacity: max_table_capacity
	}
}

// apply_encoder_instruction decodes and applies ONE instruction from
// encoder-stream bytes starting at `buf[0]` (RFC 9204 §4.3), mutating this
// decoder's mirrored dynamic table. Returns `applied: false` (not an
// error) when `buf` does not yet hold a complete instruction, matching
// this module's established resumable-parsing contract. Every error --
// including a malformed instruction `decode_qpack_encoder_instruction`
// itself rejects, not only ones raised while applying it -- carries
// `QpackErrorCode.encoder_stream_error` (RFC 9204 §2.2.3, §3.2.2: every
// failure mode an encoder instruction can trigger on the decoder side
// maps to this one code). Malformed input previously fell through
// `decode_qpack_encoder_instruction`'s old none-only contract and came
// back here as `applied: false`, indistinguishable from "just needs more
// bytes" -- silently hanging instead of raising the required connection
// error (self-found via GPT-5.6 Luna review, Phase-R reproduced before
// this fix). After every insertion or duplication, this decoder
// immediately emits an Insert Count Increment of 1 (RFC 9204 §2.2.2.3
// leaves the exact timing to decoder policy; "emit after adding each new
// entry" is explicitly named as the timeliest, if not the most
// bandwidth-efficient, choice -- picked here for simplicity and testable
// correctness over coalescing).
pub fn (mut d QpackDecoder) apply_encoder_instruction(buf []u8) !QpackApplyInstructionResult {
	decoded := decode_qpack_encoder_instruction(buf) or {
		return error_with_code('qpack: ${err.msg()}', int(QpackErrorCode.encoder_stream_error))
	}
	if !decoded.has_instruction {
		return QpackApplyInstructionResult{}
	}
	instr := decoded.instr
	consumed := decoded.consumed
	mut inserted := false
	match instr {
		QpackSetDynamicTableCapacity {
			if u64(instr.capacity) > u64(qpack_max_dynamic_table_capacity_bytes) {
				return error_with_code('qpack: capacity ${instr.capacity} exceeds implementation limit', int(QpackErrorCode.encoder_stream_error))
			}
			if u64(instr.capacity) > d.max_table_capacity {
				return error_with_code("qpack: capacity ${instr.capacity} exceeds this endpoint's configured maximum ${d.max_table_capacity}", int(QpackErrorCode.encoder_stream_error))
			}
			d.dynamic_table.set_capacity(int(instr.capacity))
		}
		QpackInsertWithNameRef {
			name := if instr.is_static {
				e := qpack_static_lookup(int(instr.name_index)) or {
					return error_with_code('qpack: ${err.msg()}', int(QpackErrorCode.encoder_stream_error))
				}
				e.name
			} else {
				abs := d.dynamic_table.resolve_relative_from_insert_count(instr.name_index) or {
					return error_with_code('qpack: ${err.msg()}', int(QpackErrorCode.encoder_stream_error))
				}
				e := d.dynamic_table.get(abs) or {
					return error_with_code('qpack: ${err.msg()}', int(QpackErrorCode.encoder_stream_error))
				}
				e.name
			}
			d.dynamic_table.insert(name, instr.value) or {
				return error_with_code('qpack: ${err.msg()}', int(QpackErrorCode.encoder_stream_error))
			}
			inserted = true
		}
		QpackInsertWithLiteralName {
			d.dynamic_table.insert(instr.name, instr.value) or {
				return error_with_code('qpack: ${err.msg()}', int(QpackErrorCode.encoder_stream_error))
			}
			inserted = true
		}
		QpackDuplicate {
			d.dynamic_table.duplicate(instr.rel_index) or {
				return error_with_code('qpack: ${err.msg()}', int(QpackErrorCode.encoder_stream_error))
			}
			inserted = true
		}
	}
	mut decoder_instructions := []u8{}
	if inserted {
		decoder_instructions = encode_qpack_insert_count_increment(1)
	}
	return QpackApplyInstructionResult{
		applied: true
		consumed: consumed
		decoder_instructions: decoder_instructions
	}
}

// decode_field_section decodes one already-complete encoded field section
// (e.g. a HEADERS frame's payload, RFC 9114 §7.2.2 -- already fully
// buffered by the time `H3FrameDecoder` hands it over, so this needs no
// resumable-parsing contract of its own; see `decode_field_section_prefix`
// and `decode_qpack_field_line`'s doc comments for that reasoning).
// Returns `blocked: true` (not an error) if this decoder's Insert Count
// has not yet reached the section's declared Required Insert Count (RFC
// 9204 §2.1.2/§2.2.1) -- the caller should retry once more encoder-stream
// data has been applied. Verifies every reference stays within the
// declared Required Insert Count (§2.2.3) and that the declared count
// matches what was actually referenced (§2.2.1 -- this project enforces
// the "larger than expected" MAY-error case as a hard error, the same
// established preference for a stricter reading of an offered MAY used
// elsewhere in this codebase's QUIC/HTTP-3 work).
pub fn (mut d QpackDecoder) decode_field_section(stream_id u64, buf []u8) !QpackDecodeFieldSectionResult {
	prefix, prefix_len := decode_field_section_prefix(buf, u64(d.dynamic_table.insert_count()), d.max_table_capacity)!
	if prefix.required_insert_count > u64(d.dynamic_table.insert_count()) {
		return QpackDecodeFieldSectionResult{
			blocked: true
		}
	}
	mut lines := []QpackFieldLine{}
	mut max_ref := -1
	mut pos := prefix_len
	for pos < buf.len {
		decoded := decode_qpack_field_line(buf[pos..], prefix.base, &d.dynamic_table)!
		lines << decoded.line
		if idx := decoded.referenced_index {
			if idx >= prefix.required_insert_count {
				return error_with_code('qpack: field line references an entry beyond the declared Required Insert Count', int(QpackErrorCode.decompression_failed))
			}
			if int(idx) > max_ref {
				max_ref = int(idx)
			}
		}
		pos += decoded.consumed
	}
	if u64(max_ref + 1) != prefix.required_insert_count {
		return error_with_code("qpack: declared Required Insert Count does not match the field section's actual references", int(QpackErrorCode.decompression_failed))
	}
	mut decoder_instructions := []u8{}
	if prefix.required_insert_count != 0 {
		decoder_instructions = encode_qpack_section_ack(stream_id)
	}
	return QpackDecodeFieldSectionResult{
		lines: lines
		decoder_instructions: decoder_instructions
	}
}
