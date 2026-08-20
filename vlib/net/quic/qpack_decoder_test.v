// vtest build: present_openssl?
module quic

fn test_decoder_apply_encoder_instruction_returns_not_applied_on_partial_buffer() {
	mut d := new_qpack_decoder(1000)
	full := encode_qpack_insert_with_literal_name('name', 'value')
	r := d.apply_encoder_instruction(full[..2]) or { panic('${err}') }
	assert !r.applied
}

fn test_decoder_apply_encoder_instruction_capacity_exceeds_own_configured_max_is_error() {
	mut d := new_qpack_decoder(100)
	buf := encode_qpack_set_dynamic_table_capacity(200)
	if _ := d.apply_encoder_instruction(buf) {
		assert false, "expected an error: 200 exceeds this decoder's configured max of 100"
	} else {
		assert err.code() == int(QpackErrorCode.encoder_stream_error)
	}
}

fn test_decoder_apply_encoder_instruction_invalid_static_index_is_encoder_stream_error() {
	mut d := new_qpack_decoder(1000)
	buf := encode_qpack_insert_with_name_ref(true, 200, 'v') // static table only has 99 entries
	if _ := d.apply_encoder_instruction(buf) {
		assert false, 'expected an error'
	} else {
		assert err.code() == int(QpackErrorCode.encoder_stream_error)
	}
}

fn test_decoder_apply_encoder_instruction_insert_larger_than_capacity_is_error() {
	mut d := new_qpack_decoder(1000)
	d.apply_encoder_instruction(encode_qpack_set_dynamic_table_capacity(10)) or { panic('${err}') }
	buf := encode_qpack_insert_with_literal_name('a-fairly-long-name', 'and-a-long-value-too')
	if _ := d.apply_encoder_instruction(buf) {
		assert false, 'expected an error: entry larger than the 10-byte capacity'
	} else {
		assert err.code() == int(QpackErrorCode.encoder_stream_error)
	}
}

fn test_decoder_decode_field_section_blocked_when_ric_exceeds_insert_count() {
	mut d := new_qpack_decoder(1000)
	// Encode a field section that claims Required Insert Count = 1 (an
	// indexed dynamic reference), but no entries have been inserted yet.
	prefix := encode_field_section_prefix(1, 1, 1000) or { panic('${err}') }
	repr := encode_indexed_dynamic_relative(0)
	mut buf := []u8{}
	buf << prefix
	buf << repr
	result := d.decode_field_section(0, buf) or { panic('${err}') }
	assert result.blocked
	assert result.lines.len == 0
}

fn test_decoder_decode_field_section_no_ack_when_ric_zero() {
	mut d := new_qpack_decoder(1000)
	buf := [u8(0x00), 0x00] // RIC=0, Base=0, no representations
	result := d.decode_field_section(0, buf) or { panic('${err}') }
	assert !result.blocked
	assert result.lines.len == 0
	assert result.decoder_instructions.len == 0
}

fn test_decoder_decode_field_section_rejects_reference_beyond_declared_ric() {
	mut d := new_qpack_decoder(1000)
	d.apply_encoder_instruction(encode_qpack_set_dynamic_table_capacity(1000)) or {
		panic('${err}')
	}
	d.apply_encoder_instruction(encode_qpack_insert_with_literal_name('a', '1')) or {
		panic('${err}')
	} // abs 0
	d.apply_encoder_instruction(encode_qpack_insert_with_literal_name('b', '1')) or {
		panic('${err}')
	} // abs 1
	// Declare RIC=1 (only entry 0 "promised") but reference entry at abs 1
	// via post-base indexing with base=1: post-base 0 -> abs 1, which is
	// >= the declared RIC of 1.
	prefix := encode_field_section_prefix(1, 1, 1000) or { panic('${err}') }
	repr := encode_indexed_dynamic_post_base(0)
	mut buf := []u8{}
	buf << prefix
	buf << repr
	if _ := d.decode_field_section(0, buf) {
		assert false, 'expected QPACK_DECOMPRESSION_FAILED'
	} else {
		assert err.code() == int(QpackErrorCode.decompression_failed)
	}
}

fn test_decoder_decode_field_section_rejects_reference_to_evicted_entry() {
	mut d := new_qpack_decoder(1000)
	one_entry_capacity := u64(qpack_entry_size('a', '1'))
	d.apply_encoder_instruction(encode_qpack_set_dynamic_table_capacity(one_entry_capacity)) or {
		panic('${err}')
	}
	d.apply_encoder_instruction(encode_qpack_insert_with_literal_name('a', '1')) or {
		panic('${err}')
	} // abs 0
	d.apply_encoder_instruction(encode_qpack_insert_with_literal_name('b', '1')) or {
		panic('${err}')
	} // abs 1, evicts abs 0
	// base=2 (both entries "before" base); relative 1 -> abs 0, already evicted.
	prefix := encode_field_section_prefix(2, 2, one_entry_capacity) or { panic('${err}') }
	repr := encode_indexed_dynamic_relative(1)
	mut buf := []u8{}
	buf << prefix
	buf << repr
	if _ := d.decode_field_section(0, buf) {
		assert false, 'expected an error: entry 0 was evicted'
	}
}

fn test_decoder_decode_field_section_rejects_declared_ric_larger_than_actual_references() {
	mut d := new_qpack_decoder(1000)
	d.apply_encoder_instruction(encode_qpack_set_dynamic_table_capacity(1000)) or {
		panic('${err}')
	}
	d.apply_encoder_instruction(encode_qpack_insert_with_literal_name('a', '1')) or {
		panic('${err}')
	} // abs 0
	d.apply_encoder_instruction(encode_qpack_insert_with_literal_name('b', '1')) or {
		panic('${err}')
	} // abs 1
	// Declares RIC=2 (implying a reference up to abs 1) but the only
	// representation is a static reference -- no dynamic table reference at
	// all. This project enforces the "larger than expected" MAY-error case.
	prefix := encode_field_section_prefix(2, 2, 1000) or { panic('${err}') }
	repr := encode_indexed_static(17) // :method GET, purely static
	mut buf := []u8{}
	buf << prefix
	buf << repr
	if _ := d.decode_field_section(0, buf) {
		assert false, 'expected an error: declared RIC does not match actual references'
	} else {
		assert err.code() == int(QpackErrorCode.decompression_failed)
	}
}

fn test_decoder_decode_field_section_emits_section_ack_only_when_ric_nonzero() {
	mut d := new_qpack_decoder(1000)
	d.apply_encoder_instruction(encode_qpack_set_dynamic_table_capacity(1000)) or {
		panic('${err}')
	}
	d.apply_encoder_instruction(encode_qpack_insert_with_literal_name('a', '1')) or {
		panic('${err}')
	} // abs 0
	prefix := encode_field_section_prefix(1, 1, 1000) or { panic('${err}') }
	repr := encode_indexed_dynamic_relative(0)
	mut buf := []u8{}
	buf << prefix
	buf << repr
	result := d.decode_field_section(7, buf) or { panic('${err}') }
	assert !result.blocked
	assert result.decoder_instructions == encode_qpack_section_ack(7)
}
