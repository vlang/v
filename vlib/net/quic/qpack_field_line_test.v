module quic

fn test_encode_decode_ric_roundtrip() {
	// The modular encoding only disambiguates losslessly for a RIC that is
	// "recent" relative to total_inserts -- which matches real usage, since
	// a Required Insert Count can only ever reference entries the dynamic
	// table hasn't evicted yet, and a table with capacity 220 (max_entries
	// 6, per RFC 9204 §4.5.1.1's own formula) never holds an entry from
	// hundreds of insertions ago. Testing v far below total_inserts (e.g.
	// v=1 when total_inserts=1000) would assert something no conformant
	// encoder would ever actually produce, not a real round-trip case.
	max_table_capacity := u64(220) // matches Appendix B.2's example, max_entries=6
	for total_inserts in [u64(0), 2, 4, 6, 50, 1000] {
		mut candidates := [u64(0), total_inserts]
		if total_inserts >= 1 {
			candidates << total_inserts - 1
		}
		for v in candidates {
			enc := encode_ric(v, max_table_capacity) or { panic('v=${v}: ${err}') }
			dec := decode_ric(enc, total_inserts, max_table_capacity) or {
				panic('v=${v} total_inserts=${total_inserts}: ${err}')
			}
			assert dec == v
		}
	}
}

fn test_decode_ric_zero_stays_zero() {
	assert decode_ric(0, 100, 220)! == 0
}

fn test_decode_ric_rejects_encoded_value_above_full_range() {
	// max_entries = floor(32/32) = 1, full_range = 2. Encoded value 3 is
	// impossible for any conformant encoder to have produced.
	if _ := decode_ric(3, 0, 32) {
		assert false, 'expected an out-of-range error'
	}
}

fn test_decode_ric_rejects_zero_max_table_capacity_with_nonzero_encoded_value() {
	if _ := decode_ric(1, 0, 0) {
		assert false, 'max_table_capacity=0 means dynamic table is unusable; any nonzero RIC is invalid'
	}
}

fn test_phase_r_encode_ric_rejects_capacity_too_small_instead_of_dividing_by_zero() {
	// Luna P2: encode_ric used to divide by zero when qpack_max_entries(max_
	// table_capacity) == 0, i.e. max_table_capacity < 32 (not <64 as
	// originally cited -- qpack_max_entries divides by 32, so full_range=
	// 2*max_entries only reaches 0 below 32). decode_ric's own sibling
	// guard, exercised by
	// test_decode_ric_rejects_zero_max_table_capacity_with_nonzero_encoded_value
	// just above, already covered this exact case for the decode direction;
	// encode_ric never got the equivalent guard, crashing the whole process
	// on `% full_range` instead of returning a graceful error.
	if _ := encode_ric(1, 10) {
		assert false, 'max_table_capacity=10 cannot hold any entry (< 32 bytes); a nonzero RIC must be rejected, not silently miscomputed'
	}
	// req_insert_count=0 is unaffected -- it short-circuits before full_range
	// is ever computed, same as decode_ric's own zero fast path.
	assert encode_ric(0, 10)! == 0
}

fn test_decode_base_sign_zero() {
	assert decode_base(false, 5, 10)! == 15
}

fn test_decode_base_sign_one() {
	// RFC 9204 §4.5.1.2's own worked example: RIC=9, Sign=1, DeltaBase=2 -> Base=6.
	assert decode_base(true, 2, 9)! == 6
}

fn test_decode_base_rejects_negative_result() {
	if _ := decode_base(true, 5, 5) {
		assert false, 'Sign=1 with DeltaBase >= RIC must be rejected (would make Base negative)'
	}
	if _ := decode_base(true, 6, 5) {
		assert false, 'DeltaBase > RIC must also be rejected'
	}
}

fn test_encode_decode_base_roundtrip() {
	cases := [[u64(10), u64(15)], [u64(9), u64(6)], [u64(5), u64(5)], [u64(0), u64(0)]]
	for c in cases {
		req_insert_count, base := c[0], c[1]
		sign, delta := encode_base(base, req_insert_count)
		decoded := decode_base(sign, delta, req_insert_count) or { panic('${err}') }
		assert decoded == base
	}
}

fn test_decode_qpack_field_line_indexed_static() {
	buf := encode_indexed_static(17) // :method GET
	t := QpackDynamicTable{}
	result := decode_qpack_field_line(buf, 0, &t) or { panic('${err}') }
	assert result.line.name == ':method'
	assert result.line.value == 'GET'
	assert result.referenced_index == none
	assert result.consumed == buf.len
}

fn test_decode_qpack_field_line_indexed_dynamic_relative() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('x-custom', 'v1') or { panic('${err}') } // abs 0
	base := u64(1)
	buf := encode_indexed_dynamic_relative(0) // rel 0, base 1 -> abs 0
	result := decode_qpack_field_line(buf, base, &t) or { panic('${err}') }
	assert result.line.name == 'x-custom'
	assert result.line.value == 'v1'
	assert result.referenced_index? == u64(0)
}

fn test_decode_qpack_field_line_indexed_post_base() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('x-custom', 'v1') or { panic('${err}') } // abs 0, inserted AFTER base was fixed
	base := u64(0)
	buf := encode_indexed_dynamic_post_base(0)
	result := decode_qpack_field_line(buf, base, &t) or { panic('${err}') }
	assert result.line.name == 'x-custom'
	assert result.referenced_index? == u64(0)
}

fn test_decode_qpack_field_line_literal_with_static_name_ref() {
	buf := encode_literal_with_name_ref(true, false, 0, 'example.org') // :authority
	t := QpackDynamicTable{}
	result := decode_qpack_field_line(buf, 0, &t) or { panic('${err}') }
	assert result.line.name == ':authority'
	assert result.line.value == 'example.org'
	assert result.referenced_index == none
	assert !result.line.never_index
}

fn test_decode_qpack_field_line_literal_with_dynamic_name_ref_never_indexed() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('cookie', 'stale') or { panic('${err}') } // abs 0
	buf := encode_literal_with_name_ref(false, true, 0, 'fresh') // rel 0, base 1 -> abs 0
	result := decode_qpack_field_line(buf, 1, &t) or { panic('${err}') }
	assert result.line.name == 'cookie'
	assert result.line.value == 'fresh'
	assert result.line.never_index
	assert result.referenced_index? == u64(0)
}

fn test_decode_qpack_field_line_literal_with_post_base_name_ref() {
	mut t := QpackDynamicTable{}
	t.set_capacity(1000)
	t.insert('x-new', 'placeholder') or { panic('${err}') } // abs 0, inserted after base
	buf := encode_literal_with_post_base_name_ref(false, 0, 'actual-value')
	result := decode_qpack_field_line(buf, 0, &t) or { panic('${err}') }
	assert result.line.name == 'x-new'
	assert result.line.value == 'actual-value'
}

fn test_decode_qpack_field_line_literal_with_literal_name() {
	buf := encode_literal_with_literal_name(false, 'x-totally-custom', 'value')
	t := QpackDynamicTable{}
	result := decode_qpack_field_line(buf, 0, &t) or { panic('${err}') }
	assert result.line.name == 'x-totally-custom'
	assert result.line.value == 'value'
	assert result.referenced_index == none
}

fn test_decode_qpack_field_line_literal_with_literal_name_never_indexed_bit() {
	buf := encode_literal_with_literal_name(true, 'authorization', 'secret')
	t := QpackDynamicTable{}
	result := decode_qpack_field_line(buf, 0, &t) or { panic('${err}') }
	assert result.line.never_index
}

fn test_decode_qpack_field_line_invalid_static_index_is_error() {
	buf := encode_indexed_static(200) // static table only has 99 entries
	t := QpackDynamicTable{}
	if _ := decode_qpack_field_line(buf, 0, &t) {
		assert false, 'expected an out-of-range static index error'
	}
}

fn test_decode_qpack_field_line_empty_buffer_is_error() {
	t := QpackDynamicTable{}
	if _ := decode_qpack_field_line([]u8{}, 0, &t) {
		assert false, 'expected a truncated-buffer error'
	}
}

fn test_decode_field_section_prefix_zero_ric_zero_base() {
	buf := [u8(0x00), 0x00]
	prefix, consumed := decode_field_section_prefix(buf, 0, 0) or { panic('${err}') }
	assert prefix.required_insert_count == 0
	assert prefix.base == 0
	assert consumed == 2
}

fn test_encode_decode_field_section_prefix_roundtrip() {
	max_table_capacity := u64(220)
	total_inserts := u64(4)
	req_insert_count := u64(4)
	base := u64(4)
	encoded := encode_field_section_prefix(req_insert_count, base, max_table_capacity) or {
		panic('${err}')
	}
	prefix, consumed := decode_field_section_prefix(encoded, total_inserts, max_table_capacity) or {
		panic('${err}')
	}
	assert prefix.required_insert_count == req_insert_count
	assert prefix.base == base
	assert consumed == encoded.len
}
