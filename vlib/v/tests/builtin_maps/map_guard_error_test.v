fn guard_value(items map[string]int, key string) int {
	if value := items[key] {
		return value
	}
	return -1
}

fn guard_status(items map[string]int, key string) string {
	if _ := items[key] {
		return 'found'
	} else {
		return 'missing'
	}
}

fn test_map_guard_reports_absence() {
	items := {
		'present': 7
	}
	assert guard_value(items, 'present') == 7
	assert guard_value(items, 'missing') == -1
	assert guard_status(items, 'present') == 'found'
	assert guard_status(items, 'missing') == 'missing'
	err := 'outer'
	mut observed := ''
	if _ := items['first'] {
		assert false
	} else if _ := items['second'] {
		assert false
	} else {
		observed = err
	}
	assert observed == 'outer'
}

fn nested_guard_value(items map[string]int, indexes map[string]string) !int {
	if value := items[indexes['key']!] {
		return value
	}
	return error_sentinel
}

fn test_map_guard_preserves_nested_lookup_errors() {
	items := {
		'present': 7
	}
	if _ := nested_guard_value(items, map[string]string{}) {
		assert false
	} else {
		assert err.msg() == 'map key does not exist'
	}
	if value := nested_guard_value(items, {
		'key': 'present'
	}) {
		assert value == 7
	} else {
		assert false
	}
}

fn test_map_guard_preserves_optional_payloads() {
	items := {
		'present': ?int(7)
		'none':    ?int(none)
	}
	for key in ['missing', 'none'] {
		if _ := items[key] {
			assert false
		}
	}
	if value := items['present'] {
		assert value == 7
	} else {
		assert false
	}
}
