fn get_value1(m map[string]int, key string) ?int {
	return m[key] or { none }
}

fn get_value2(m map[string]int, key string) !int {
	return m[key] or { error('not found') }
}

fn test_return_map_index_with_or_block() {
	m := {
		'aaa': 111
	}
	if ret1 := get_value1(m, 'aaa') {
		assert ret1 == 111
	} else {
		assert false
	}

	if ret2 := get_value2(m, 'aaa') {
		assert ret2 == 111
	} else {
		assert false
	}
}
