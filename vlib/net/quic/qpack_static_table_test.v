module quic

fn test_qpack_static_table_has_exactly_99_entries() {
	assert qpack_static_table.len == 99
}

fn test_qpack_static_table_indexed_from_zero_not_one() {
	// Explicit RFC 9204 §3.1 callout: unlike HPACK, index 0 is meaningful.
	e := qpack_static_lookup(0) or { panic('${err}') }
	assert e.name == ':authority'
	assert e.value == ''
}

fn test_qpack_static_table_last_entry() {
	e := qpack_static_lookup(98) or { panic('${err}') }
	assert e.name == 'x-frame-options'
	assert e.value == 'sameorigin'
}

fn test_qpack_static_lookup_rejects_out_of_range() {
	if _ := qpack_static_lookup(99) {
		assert false, 'expected an error for index 99'
	}
	if _ := qpack_static_lookup(-1) {
		assert false, 'expected an error for a negative index'
	}
}

fn test_qpack_static_find_exact_match() {
	idx := qpack_static_find(':method', 'GET') or { panic('expected a match') }
	assert idx == 17
}

fn test_qpack_static_find_no_match_for_wrong_value() {
	if _ := qpack_static_find(':method', 'TRACE') {
		assert false, 'TRACE is not in the static table'
	}
}

fn test_qpack_static_find_name_returns_first_occurrence() {
	// ":method" appears at indices 15-21 (CONNECT..PUT); find_name must
	// return the first one, not just any match.
	idx := qpack_static_find_name(':method') or { panic('expected a match') }
	assert idx == 15
}

fn test_qpack_static_find_name_no_match() {
	if _ := qpack_static_find_name('x-not-a-real-header') {
		assert false, 'expected no match'
	}
}
