// vtest build: present_openssl?
module quic

fn test_encode_decode_section_ack_roundtrip() {
	for id in [u64(0), 1, 4, 127, 128, 1_000_000] {
		buf := encode_qpack_section_ack(id)
		decoded := decode_qpack_decoder_instruction(buf) or { panic('id=${id}: ${err}') }
		assert decoded.has_instruction
		assert decoded.consumed == buf.len
		match decoded.instr {
			QpackSectionAck {
				assert decoded.instr.stream_id == id
			}
			else {
				assert false, 'expected QpackSectionAck'
			}
		}
	}
}

fn test_encode_decode_stream_cancellation_roundtrip() {
	buf := encode_qpack_stream_cancellation(8)
	decoded := decode_qpack_decoder_instruction(buf) or { panic('${err}') }
	assert decoded.has_instruction
	assert decoded.consumed == buf.len
	match decoded.instr {
		QpackStreamCancellation {
			assert decoded.instr.stream_id == 8
		}
		else {
			assert false, 'expected QpackStreamCancellation'
		}
	}
}

fn test_encode_decode_insert_count_increment_roundtrip() {
	buf := encode_qpack_insert_count_increment(1)
	decoded := decode_qpack_decoder_instruction(buf) or { panic('${err}') }
	assert decoded.has_instruction
	assert decoded.consumed == buf.len
	match decoded.instr {
		QpackInsertCountIncrement {
			assert decoded.instr.increment == 1
		}
		else {
			assert false, 'expected QpackInsertCountIncrement'
		}
	}
}

fn test_decode_qpack_decoder_instruction_returns_not_complete_on_empty_buffer() {
	decoded := decode_qpack_decoder_instruction([]u8{}) or { panic('${err}') }
	assert !decoded.has_instruction
}

fn test_three_decoder_instruction_bit_patterns_are_mutually_exclusive() {
	d1 := decode_qpack_decoder_instruction(encode_qpack_section_ack(1)) or { panic('${err}') }
	d2 := decode_qpack_decoder_instruction(encode_qpack_stream_cancellation(1)) or {
		panic('${err}')
	}
	d3 := decode_qpack_decoder_instruction(encode_qpack_insert_count_increment(1)) or {
		panic('${err}')
	}
	assert d1.instr is QpackSectionAck
	assert d2.instr is QpackStreamCancellation
	assert d3.instr is QpackInsertCountIncrement
}

fn test_phase_r_decoder_instruction_malformed_oversized_integer_propagates_error() {
	// Sibling of the same fix in qpack_encoder_instructions_test.v -- Insert
	// Count Increment (pattern '00', 6-bit prefix) forced into continuation
	// with enough bytes to exceed the implementation limit.
	mut buf := [u8(0x3f)] // 6-bit prefix, max_prefix=63, forces continuation
	for _ in 0 .. 10 {
		buf << 0xff
	}
	buf << 0x7f
	if _ := decode_qpack_decoder_instruction(buf) {
		assert false, 'expected a propagated error for an over-limit integer'
	} else {
		assert err.msg().contains('exceeds implementation limit')
	}
}
