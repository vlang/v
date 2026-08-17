// vtest build: present_openssl?
module quic

fn test_encode_decode_set_dynamic_table_capacity_roundtrip() {
	for cap in [u64(0), 1, 63, 64, 220, 1_000_000] {
		buf := encode_qpack_set_dynamic_table_capacity(cap)
		decoded := decode_qpack_encoder_instruction(buf) or { panic('cap=${cap}: ${err}') }
		assert decoded.has_instruction
		assert decoded.consumed == buf.len
		match decoded.instr {
			QpackSetDynamicTableCapacity {
				assert decoded.instr.capacity == cap
			}
			else {
				assert false, 'expected QpackSetDynamicTableCapacity'
			}
		}
	}
}

fn test_encode_decode_insert_with_name_ref_static_roundtrip() {
	buf := encode_qpack_insert_with_name_ref(true, 42, 'value here')
	decoded := decode_qpack_encoder_instruction(buf) or { panic('${err}') }
	assert decoded.has_instruction
	assert decoded.consumed == buf.len
	match decoded.instr {
		QpackInsertWithNameRef {
			assert decoded.instr.is_static
			assert decoded.instr.name_index == 42
			assert decoded.instr.value == 'value here'
		}
		else {
			assert false, 'expected QpackInsertWithNameRef'
		}
	}
}

fn test_encode_decode_insert_with_name_ref_dynamic_roundtrip() {
	buf := encode_qpack_insert_with_name_ref(false, 7, 'v')
	decoded := decode_qpack_encoder_instruction(buf) or { panic('${err}') }
	match decoded.instr {
		QpackInsertWithNameRef {
			assert !decoded.instr.is_static
			assert decoded.instr.name_index == 7
		}
		else {
			assert false, 'expected QpackInsertWithNameRef'
		}
	}
}

fn test_encode_decode_insert_with_literal_name_roundtrip() {
	buf := encode_qpack_insert_with_literal_name('x-custom-header', 'a value')
	decoded := decode_qpack_encoder_instruction(buf) or { panic('${err}') }
	assert decoded.has_instruction
	assert decoded.consumed == buf.len
	match decoded.instr {
		QpackInsertWithLiteralName {
			assert decoded.instr.name == 'x-custom-header'
			assert decoded.instr.value == 'a value'
		}
		else {
			assert false, 'expected QpackInsertWithLiteralName'
		}
	}
}

fn test_encode_decode_duplicate_roundtrip() {
	buf := encode_qpack_duplicate(9)
	decoded := decode_qpack_encoder_instruction(buf) or { panic('${err}') }
	assert decoded.has_instruction
	assert decoded.consumed == buf.len
	match decoded.instr {
		QpackDuplicate {
			assert decoded.instr.rel_index == 9
		}
		else {
			assert false, 'expected QpackDuplicate'
		}
	}
}

fn test_decode_qpack_encoder_instruction_returns_not_complete_on_empty_buffer() {
	decoded := decode_qpack_encoder_instruction([]u8{}) or { panic('${err}') }
	assert !decoded.has_instruction
}

fn test_decode_qpack_encoder_instruction_returns_not_complete_on_partial_instruction() {
	full := encode_qpack_insert_with_literal_name('name', 'value')
	for n in 1 .. full.len {
		decoded := decode_qpack_encoder_instruction(full[..n]) or {
			panic('unexpected error with only ${n}/${full.len} bytes buffered: ${err}')
		}
		assert !decoded.has_instruction, 'expected not-yet-complete with only ${n}/${full.len} bytes buffered'
	}
	decoded := decode_qpack_encoder_instruction(full) or {
		panic('expected success with full buffer')
	}
	assert decoded.has_instruction
	assert decoded.consumed == full.len
}

fn test_four_instruction_bit_patterns_are_mutually_exclusive() {
	// Sanity check the leading-bit-pattern dispatch doesn't misclassify.
	set_cap := encode_qpack_set_dynamic_table_capacity(5)
	insert_ref := encode_qpack_insert_with_name_ref(true, 5, 'v')
	insert_lit := encode_qpack_insert_with_literal_name('n', 'v')
	dup := encode_qpack_duplicate(5)

	d1 := decode_qpack_encoder_instruction(set_cap) or { panic('${err}') }
	d2 := decode_qpack_encoder_instruction(insert_ref) or { panic('${err}') }
	d3 := decode_qpack_encoder_instruction(insert_lit) or { panic('${err}') }
	d4 := decode_qpack_encoder_instruction(dup) or { panic('${err}') }

	assert d1.instr is QpackSetDynamicTableCapacity
	assert d2.instr is QpackInsertWithNameRef
	assert d3.instr is QpackInsertWithLiteralName
	assert d4.instr is QpackDuplicate
}

fn test_phase_r_malformed_huffman_value_is_not_silently_treated_as_incomplete() {
	// Insert With Literal Name: pattern '01' + H=0 + NameLen(5+)=4, name="test",
	// then H=1 + Len(7+)=4 + 4 bytes of Huffman data invalid for ANY valid
	// symbol boundary within 30 bits.
	buf := [u8(0x44), `t`, `e`, `s`, `t`, 0x84, 0xFF, 0xFF, 0xFF, 0xFF]

	// First confirm the underlying primitive genuinely treats this as
	// malformed (a real decode failure), not merely short -- i.e. this is
	// not a false-alarm truncation case.
	mut primitive_errored := false
	mut primitive_msg := ''
	if _, _, _ := decode_prefixed_string(buf[5..], 7) {
	} else {
		primitive_errored = true
		primitive_msg = err.msg()
	}
	assert primitive_errored, 'expected decode_prefixed_string to reject invalid Huffman data'
	assert primitive_msg.contains('huffman'), 'expected a Huffman-specific error, got: ${primitive_msg}'

	// decode_qpack_encoder_instruction must now propagate this as a real
	// error, carrying it up to QPACK_ENCODER_STREAM_ERROR at the
	// apply_encoder_instruction layer -- NOT report "not yet complete".
	if _ := decode_qpack_encoder_instruction(buf) {
		assert false, 'expected a propagated error for malformed Huffman data, not success'
	} else {
		assert err.msg().contains('huffman'), 'expected the Huffman error to propagate through, got: ${err.msg()}'
	}
}

fn test_decode_qpack_encoder_instruction_propagates_oversized_integer_error() {
	// Set Dynamic Table Capacity: pattern '001', 5-bit prefix forcing
	// continuation, with enough continuation bytes to exceed
	// qpack_max_prefixed_int -- genuinely malformed, not truncated.
	mut buf := [u8(0x3f)] // 5-bit prefix, max_prefix=31, forces continuation
	for _ in 0 .. 10 {
		buf << 0xff
	}
	buf << 0x7f
	if _ := decode_qpack_encoder_instruction(buf) {
		assert false, 'expected a propagated error for an over-limit integer'
	} else {
		assert err.msg().contains('exceeds implementation limit')
	}
}
