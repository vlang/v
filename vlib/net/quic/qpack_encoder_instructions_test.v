// vtest build: present_openssl?
module quic

fn test_encode_decode_set_dynamic_table_capacity_roundtrip() {
	for cap in [u64(0), 1, 63, 64, 220, 1_000_000] {
		buf := encode_qpack_set_dynamic_table_capacity(cap)
		instr, consumed := decode_qpack_encoder_instruction(buf) or { panic('cap=${cap}: ${err}') }
		assert consumed == buf.len
		match instr {
			QpackSetDynamicTableCapacity {
				assert instr.capacity == cap
			}
			else {
				assert false, 'expected QpackSetDynamicTableCapacity'
			}
		}
	}
}

fn test_encode_decode_insert_with_name_ref_static_roundtrip() {
	buf := encode_qpack_insert_with_name_ref(true, 42, 'value here')
	instr, consumed := decode_qpack_encoder_instruction(buf) or { panic('${err}') }
	assert consumed == buf.len
	match instr {
		QpackInsertWithNameRef {
			assert instr.is_static
			assert instr.name_index == 42
			assert instr.value == 'value here'
		}
		else {
			assert false, 'expected QpackInsertWithNameRef'
		}
	}
}

fn test_encode_decode_insert_with_name_ref_dynamic_roundtrip() {
	buf := encode_qpack_insert_with_name_ref(false, 7, 'v')
	instr, _ := decode_qpack_encoder_instruction(buf) or { panic('${err}') }
	match instr {
		QpackInsertWithNameRef {
			assert !instr.is_static
			assert instr.name_index == 7
		}
		else {
			assert false, 'expected QpackInsertWithNameRef'
		}
	}
}

fn test_encode_decode_insert_with_literal_name_roundtrip() {
	buf := encode_qpack_insert_with_literal_name('x-custom-header', 'a value')
	instr, consumed := decode_qpack_encoder_instruction(buf) or { panic('${err}') }
	assert consumed == buf.len
	match instr {
		QpackInsertWithLiteralName {
			assert instr.name == 'x-custom-header'
			assert instr.value == 'a value'
		}
		else {
			assert false, 'expected QpackInsertWithLiteralName'
		}
	}
}

fn test_encode_decode_duplicate_roundtrip() {
	buf := encode_qpack_duplicate(9)
	instr, consumed := decode_qpack_encoder_instruction(buf) or { panic('${err}') }
	assert consumed == buf.len
	match instr {
		QpackDuplicate {
			assert instr.rel_index == 9
		}
		else {
			assert false, 'expected QpackDuplicate'
		}
	}
}

fn test_decode_qpack_encoder_instruction_returns_none_on_empty_buffer() {
	if _, _ := decode_qpack_encoder_instruction([]u8{}) {
		assert false, 'expected none'
	}
}

fn test_decode_qpack_encoder_instruction_returns_none_on_partial_instruction() {
	full := encode_qpack_insert_with_literal_name('name', 'value')
	for n in 1 .. full.len {
		if _, _ := decode_qpack_encoder_instruction(full[..n]) {
			assert false, 'expected none with only ${n}/${full.len} bytes buffered'
		}
	}
	_, consumed := decode_qpack_encoder_instruction(full) or {
		panic('expected success with full buffer')
	}
	assert consumed == full.len
}

fn test_four_instruction_bit_patterns_are_mutually_exclusive() {
	// Sanity check the leading-bit-pattern dispatch doesn't misclassify.
	set_cap := encode_qpack_set_dynamic_table_capacity(5)
	insert_ref := encode_qpack_insert_with_name_ref(true, 5, 'v')
	insert_lit := encode_qpack_insert_with_literal_name('n', 'v')
	dup := encode_qpack_duplicate(5)

	i1, _ := decode_qpack_encoder_instruction(set_cap) or { panic('${err}') }
	i2, _ := decode_qpack_encoder_instruction(insert_ref) or { panic('${err}') }
	i3, _ := decode_qpack_encoder_instruction(insert_lit) or { panic('${err}') }
	i4, _ := decode_qpack_encoder_instruction(dup) or { panic('${err}') }

	assert i1 is QpackSetDynamicTableCapacity
	assert i2 is QpackInsertWithNameRef
	assert i3 is QpackInsertWithLiteralName
	assert i4 is QpackDuplicate
}
