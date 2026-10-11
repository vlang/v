module driver

fn test_promoted_set_survives_source_release() {
	mut source := map[string]bool{}
	for index in 0 .. 1024 {
		source[index.str()] = index % 2 == 0
	}
	for index in 0 .. 1023 {
		source.delete(index.str())
	}
	promoted := clone_string_bool_map(source)
	unsafe { source.free() }
	assert promoted.len == 1
	assert '1023' in promoted
	assert !promoted['1023']
}

fn test_promoted_nested_sets_have_independent_storage() {
	mut source := {
		'entry': {
			'field': true
		}
	}
	mut promoted := clone_nested_string_bool_map(source)
	promoted['entry']['next'] = true
	source['entry']['field'] = false
	assert promoted['entry']['field']
	assert 'next' !in source['entry']
}
