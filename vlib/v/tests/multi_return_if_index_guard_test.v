fn index_guard_pair(value string) (string, string) {
	return value, value
}

fn index_guard_optional(value string, present bool) ?string {
	if present {
		return value
	}
	return none
}

fn index_guard_result(present bool) !string {
	if present {
		return 'result'
	}
	return error('missing')
}

fn test_multi_return_if_map_guard() {
	m := {
		'k':     'v'
		'empty': ''
	}
	a, b := if value := m['k'] { index_guard_pair(value) } else { '', '' }
	assert a == 'v'
	assert b == 'v'
	c, d := if value := m['absent'] { index_guard_pair(value) } else { 'no', 'key' }
	assert c == 'no'
	assert d == 'key'
	e, f := if value := m['empty'] { index_guard_pair(value) } else { 'no', 'key' }
	assert e == ''
	assert f == ''
}

fn test_multi_return_if_guard_assignment() {
	m := {
		'k': 'v'
	}
	fallback := 'outer'
	mut a, mut b := '', ''
	a, b = if value := m['k'] { value, 'found' } else { fallback, 'missing' }
	assert a == 'v'
	assert b == 'found'
	a, b = if value := m['absent'] { value, 'found' } else { fallback, 'missing' }
	assert a == 'outer'
	assert b == 'missing'
	assert fallback == 'outer'
}

fn test_multi_return_if_guard_else_if() {
	m := {
		'k': 'v'
	}
	a, b := if value := m['absent'] {
		index_guard_pair(value)
	} else if value := m['k'] {
		index_guard_pair(value)
	} else {
		'', ''
	}
	assert a == 'v'
	assert b == 'v'
	c, d := if value := m['absent'] {
		value, value
	} else if m.len == 1 {
		'plain', 'branch'
	} else {
		'', ''
	}
	assert c == 'plain'
	assert d == 'branch'
}

fn test_multi_return_if_optional_map_value_guard() {
	mut values := map[string]?string{}
	values['present'] = 'value'
	values['none'] = none
	for key in ['present', 'none', 'absent'] {
		a, b := if value := values[key] { value, value } else { 'missing', key }
		if key == 'present' {
			assert a == 'value'
			assert b == 'value'
		} else {
			assert a == 'missing'
			assert b == key
		}
	}
}

fn test_multi_return_if_array_guard() {
	values := ['first']
	a, b := if value := values[0] { index_guard_pair(value) } else { '', '' }
	assert a == 'first'
	assert b == 'first'
	c, d := if value := values[1] { index_guard_pair(value) } else { 'out', 'of bounds' }
	assert c == 'out'
	assert d == 'of bounds'
}

fn test_multi_return_if_option_and_result_guards() {
	a, b := if value := index_guard_optional('option', true) {
		index_guard_pair(value)
	} else {
		'', ''
	}
	assert a == 'option'
	assert b == 'option'
	c, d := if value := index_guard_optional('option', false) {
		value, value
	} else {
		'none', 'none'
	}
	assert c == 'none'
	assert d == 'none'
	e, f := if value := index_guard_result(false) { value, value } else { err.msg(), 'error' }
	assert e == 'missing'
	assert f == 'error'
}
