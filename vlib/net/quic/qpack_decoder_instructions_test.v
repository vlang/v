// vtest build: present_openssl?
module quic

fn test_encode_decode_section_ack_roundtrip() {
	for id in [u64(0), 1, 4, 127, 128, 1_000_000] {
		buf := encode_qpack_section_ack(id)
		instr, consumed := decode_qpack_decoder_instruction(buf) or { panic('id=${id}: ${err}') }
		assert consumed == buf.len
		match instr {
			QpackSectionAck {
				assert instr.stream_id == id
			}
			else {
				assert false, 'expected QpackSectionAck'
			}
		}
	}
}

fn test_encode_decode_stream_cancellation_roundtrip() {
	buf := encode_qpack_stream_cancellation(8)
	instr, consumed := decode_qpack_decoder_instruction(buf) or { panic('${err}') }
	assert consumed == buf.len
	match instr {
		QpackStreamCancellation {
			assert instr.stream_id == 8
		}
		else {
			assert false, 'expected QpackStreamCancellation'
		}
	}
}

fn test_encode_decode_insert_count_increment_roundtrip() {
	buf := encode_qpack_insert_count_increment(1)
	instr, consumed := decode_qpack_decoder_instruction(buf) or { panic('${err}') }
	assert consumed == buf.len
	match instr {
		QpackInsertCountIncrement {
			assert instr.increment == 1
		}
		else {
			assert false, 'expected QpackInsertCountIncrement'
		}
	}
}

fn test_decode_qpack_decoder_instruction_returns_none_on_empty_buffer() {
	if _, _ := decode_qpack_decoder_instruction([]u8{}) {
		assert false, 'expected none'
	}
}

fn test_three_decoder_instruction_bit_patterns_are_mutually_exclusive() {
	i1, _ := decode_qpack_decoder_instruction(encode_qpack_section_ack(1)) or { panic('${err}') }
	i2, _ := decode_qpack_decoder_instruction(encode_qpack_stream_cancellation(1)) or {
		panic('${err}')
	}
	i3, _ := decode_qpack_decoder_instruction(encode_qpack_insert_count_increment(1)) or {
		panic('${err}')
	}
	assert i1 is QpackSectionAck
	assert i2 is QpackStreamCancellation
	assert i3 is QpackInsertCountIncrement
}
