module quic

fn test_qpack_is_sensitive_matches_known_fields() {
	assert qpack_is_sensitive('cookie')
	assert qpack_is_sensitive('authorization')
	assert qpack_is_sensitive('proxy-authorization')
	assert !qpack_is_sensitive('user-agent')
	assert !qpack_is_sensitive('accept')
}

fn test_encoder_never_index_line_matching_static_entry_uses_literal_with_name_ref_not_indexed() {
	// Mirrors net.http's h2_hpack.v `encode_field`: `if exact && !sensitive`
	// withholds a FULLY indexed representation (static or dynamic) for a
	// never_index line -- even one that exactly matches a static entry --
	// while a NAME-only static reference stays available. Same policy,
	// kept consistent across both compression schemes in this codebase.
	mut e := new_qpack_encoder()
	e.set_capacity(1000, 1000) or { panic('${err}') }
	result := e.encode_field_section(0, [
		QpackFieldLine{
			name: ':method'
			value: 'GET'
			never_index: true
		},
	]) or { panic('${err}') }
	mut expected := [u8(0x00), 0x00] // RIC=0, Base=0 -- no dynamic table reference at all
	expected << encode_literal_with_name_ref(true, true, 15, 'GET') // static name index 15 = :method
	assert result.field_section == expected
	assert e.dynamic_table.insert_count() == 0

	// Confirm it decodes back correctly too.
	mut dec := new_qpack_decoder(1000)
	decoded := dec.decode_field_section(0, result.field_section) or { panic('${err}') }
	assert decoded.lines[0].name == ':method'
	assert decoded.lines[0].value == 'GET'
	assert decoded.lines[0].never_index
}

fn test_encoder_never_index_line_is_not_inserted_into_dynamic_table() {
	mut e := new_qpack_encoder()
	e.set_capacity(1000, 1000) or { panic('${err}') }
	result := e.encode_field_section(0, [
		QpackFieldLine{
			name: 'authorization'
			value: 'Bearer secret-token'
			never_index: true
		},
	]) or { panic('${err}') }
	assert e.dynamic_table.insert_count() == 0
	assert result.encoder_instructions.len == 0
}

fn test_encoder_sensitive_field_never_inserted_even_without_caller_flag() {
	// Defense-in-depth parity with h2_hpack.v's `encode_field`, which applies
	// `h2_is_sensitive` unconditionally: a caller that forgets to set
	// `never_index: true` on a Cookie/Authorization field must still not have
	// it inserted into the dynamic table.
	mut e := new_qpack_encoder()
	e.set_capacity(1000, 1000) or { panic('${err}') }
	result := e.encode_field_section(0, [
		QpackFieldLine{
			name: 'cookie'
			value: 'session=super-secret-value'
			// never_index deliberately left false here.
		},
	]) or { panic('${err}') }
	assert e.dynamic_table.insert_count() == 0
	assert result.encoder_instructions.len == 0

	mut dec := new_qpack_decoder(1000)
	decoded := dec.decode_field_section(0, result.field_section) or { panic('${err}') }
	assert decoded.lines[0].name == 'cookie'
	assert decoded.lines[0].value == 'session=super-secret-value'
	assert decoded.lines[0].never_index
}

fn test_encoder_exact_static_match_never_touches_dynamic_table() {
	mut e := new_qpack_encoder()
	e.set_capacity(1000, 1000) or { panic('${err}') }
	result := e.encode_field_section(0, [
		QpackFieldLine{
			name: ':method'
			value: 'GET'
		},
	]) or { panic('${err}') }
	assert e.dynamic_table.insert_count() == 0
	assert result.field_section.len > 0
	// Decode it back to confirm it round-trips through a fresh decoder.
	mut dec := new_qpack_decoder(1000)
	decoded := dec.decode_field_section(0, result.field_section) or { panic('${err}') }
	assert decoded.lines[0].name == ':method'
	assert decoded.lines[0].value == 'GET'
}

fn test_encoder_zero_capacity_never_inserts_or_emits_instructions() {
	mut e := new_qpack_encoder()
	// peer_max_table_capacity defaults to 0 (never called set_capacity).
	result := e.encode_field_section(0, [
		QpackFieldLine{
			name: 'x-custom'
			value: 'value'
		},
	]) or { panic('${err}') }
	assert e.dynamic_table.insert_count() == 0
	assert result.encoder_instructions.len == 0
}

fn test_encoder_set_capacity_rejects_exceeding_peer_max() {
	mut e := new_qpack_encoder()
	if _ := e.set_capacity(500, 100) {
		assert false, 'expected an error: requested capacity exceeds peer max'
	}
}

fn test_encoder_set_capacity_rejects_exceeding_implementation_limit() {
	mut e := new_qpack_encoder()
	huge := u64(qpack_max_dynamic_table_capacity_bytes) + 1
	if _ := e.set_capacity(huge, huge) {
		assert false, 'expected an error: exceeds implementation limit'
	}
}

fn test_encoder_full_round_trip_through_decoder_with_insertion() {
	mut e := new_qpack_encoder()
	cap_instr := e.set_capacity(1000, 1000) or { panic('${err}') }
	mut dec := new_qpack_decoder(1000)
	// A real caller must forward the Set Dynamic Table Capacity instruction
	// itself, same as any other encoder-stream bytes -- the decoder's own
	// mirrored table capacity stays 0 until it does (RFC 9204 §3.2.2's
	// "initial capacity is zero" applies independently on each side).
	dec.apply_encoder_instruction(cap_instr) or { panic('${err}') }

	lines := [
		QpackFieldLine{
			name: 'x-custom-header'
			value: 'custom-value'
		},
	]
	result := e.encode_field_section(5, lines) or { panic('${err}') }
	assert result.encoder_instructions.len > 0 // new name+value -> should insert

	applied := dec.apply_encoder_instruction(result.encoder_instructions) or { panic('${err}') }
	assert applied.applied
	assert applied.consumed == result.encoder_instructions.len

	dec_result := dec.decode_field_section(5, result.field_section) or { panic('${err}') }
	assert !dec_result.blocked
	assert dec_result.lines[0].name == 'x-custom-header'
	assert dec_result.lines[0].value == 'custom-value'
}

fn test_encoder_note_section_acknowledged_releases_references_and_advances_known_received_count() {
	mut e := new_qpack_encoder()
	e.set_capacity(1000, 1000) or { panic('${err}') }
	result := e.encode_field_section(1, [
		QpackFieldLine{
			name: 'x-a'
			value: '1'
		},
	]) or { panic('${err}') }
	assert result.encoder_instructions.len > 0
	assert e.unacked.len == 1
	e.note_section_acknowledged(1) or { panic('${err}') }
	assert e.unacked.len == 0
	assert e.known_received_count == 1
}

fn test_encoder_note_section_acknowledged_errors_on_unknown_stream() {
	mut e := new_qpack_encoder()
	if _ := e.note_section_acknowledged(999) {
		assert false, 'expected an error: nothing outstanding on stream 999'
	} else {
		assert err.code() == int(QpackErrorCode.decoder_stream_error)
	}
}

fn test_encoder_note_stream_cancelled_releases_without_advancing_known_received_count() {
	mut e := new_qpack_encoder()
	e.set_capacity(1000, 1000) or { panic('${err}') }
	e.encode_field_section(2, [
		QpackFieldLine{
			name: 'x-b'
			value: '2'
		},
	]) or { panic('${err}') }
	before := e.known_received_count
	e.note_stream_cancelled(2)
	assert e.known_received_count == before
	assert e.unacked.len == 0
}

fn test_encoder_note_insert_count_increment_rejects_zero() {
	mut e := new_qpack_encoder()
	if _ := e.note_insert_count_increment(0) {
		assert false, 'expected an error for a zero increment'
	} else {
		assert err.code() == int(QpackErrorCode.decoder_stream_error)
	}
}

fn test_encoder_note_insert_count_increment_rejects_exceeding_actual_inserts() {
	mut e := new_qpack_encoder()
	if _ := e.note_insert_count_increment(1) {
		assert false, 'expected an error: nothing has been inserted yet'
	} else {
		assert err.code() == int(QpackErrorCode.decoder_stream_error)
	}
}

fn test_phase_r_dynamic_name_reference_without_insertion_tracks_required_insert_count() {
	mut e := new_qpack_encoder()
	entry_a_size := qpack_entry_size('x-custom', 'original')
	cap_instr := e.set_capacity(u64(entry_a_size), u64(entry_a_size)) or { panic('${err}') }
	mut dec := new_qpack_decoder(u64(entry_a_size))
	dec.apply_encoder_instruction(cap_instr) or { panic('${err}') }

	first := e.encode_field_section(1, [
		QpackFieldLine{
			name: 'x-custom'
			value: 'original'
		},
	]) or { panic('${err}') }
	assert first.encoder_instructions.len > 0 // confirms it actually got inserted

	e.note_section_acknowledged(1) or { panic('${err}') }

	// Same NAME, a value long enough that entry_size exceeds the 1-entry
	// capacity even after evicting everything -- can_insert must return
	// false, forcing the "use existing dynamic NAME reference, emit
	// literal" path this bug skips all tracking for.
	long_value := 'a-value-far-too-long-to-ever-fit-in-this-tiny-table'
	assert qpack_entry_size('x-custom', long_value) > entry_a_size

	second := e.encode_field_section(2, [
		QpackFieldLine{
			name: 'x-custom'
			value: long_value
		},
	]) or { panic('${err}') }

	dec.apply_encoder_instruction(first.encoder_instructions) or { panic('${err}') }
	result := dec.decode_field_section(2, second.field_section) or {
		panic('BUG REPRODUCED: decoder rejected a field section the encoder itself produced: ${err}')
	}
	assert !result.blocked
	assert result.lines[0].value == long_value
}

fn test_phase_r_set_capacity_rejects_reducing_below_a_referenced_entry() {
	// Luna P1: QpackEncoder.set_capacity can evict entries with outstanding
	// references. It calls dynamic_table.set_capacity() unconditionally,
	// which evicts oldest-first purely on cur_size > capacity -- unlike
	// insert(), which goes through can_insert() first specifically to
	// simulate eviction safety (RFC 9204 SS2.1.1: an entry with any
	// outstanding reference, or not yet acknowledged, must not be evicted)
	// before committing to it.
	mut e := new_qpack_encoder()
	e.set_capacity(100, 100) or { panic('${err}') }
	e.encode_field_section(1, [
		QpackFieldLine{
			name: 'x'
			value: '1'
		},
	]) or { panic('${err}') }
	assert e.dynamic_table.insert_count() == 1

	// abs 0 now has ref_count 1 (added by encode_field_section) and is
	// unacknowledged (known_received_count is still 0) -- RFC 9204 SS2.1.1
	// forbids evicting it. Capacity 10 can't hold the 34-byte entry, so
	// this reduction must be rejected outright, not silently applied by
	// evicting the still-referenced entry.
	e.set_capacity(10, 100) or {}
	got := e.dynamic_table.get(0) or {
		panic('BUG REPRODUCED: entry evicted by set_capacity while still referenced and unacknowledged: ${err}')
	}
	assert got.name == 'x'
	assert got.value == '1'
	assert e.dynamic_table.capacity() == 100 // the resize itself must not have taken effect
}
