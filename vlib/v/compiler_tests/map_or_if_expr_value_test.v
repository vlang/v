// An `or` block whose value is a trailing `if` reaches the map index lowering
// unwrapped by an expr_stmt. Lowering it as a plain statement emitted each branch's
// value as a bare expression and dropped it, leaving the result at its zero value.

fn test_map_index_or_if_expr_yields_the_branch_value() {
	m := map[string]string{}
	cur := 'cmd/tools/vtest.v'
	got := m[cur] or {
		if cur.len > 0 { cur } else { 'fallback' }
	}
	assert got == 'cmd/tools/vtest.v'
}

fn test_map_index_or_if_expr_takes_the_else_branch() {
	m := map[string]string{}
	empty := ''
	got := m['missing'] or {
		if empty.len > 0 { empty } else { 'fallback' }
	}
	assert got == 'fallback'
}

fn test_map_index_or_if_expr_is_skipped_for_a_present_key() {
	m := {
		'present': 'value'
	}
	got := m['present'] or {
		if true { 'from-or' } else { 'fallback' }
	}
	assert got == 'value'
}

fn test_map_index_or_if_expr_with_int_values() {
	m := map[string]int{}
	got := m['missing'] or {
		if m.len == 0 { 7 } else { 9 }
	}
	assert got == 7
}

fn test_map_index_or_else_if_chain() {
	m := map[string]string{}
	kind := 'b'
	got := m['missing'] or {
		if kind == 'a' {
			'first'
		} else if kind == 'b' {
			'second'
		} else {
			'third'
		}
	}
	assert got == 'second'
}
