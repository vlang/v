struct SharedKeyValue {
	n    int
	text string
}

fn test_map_clone_sharing_keys_copies_entries_and_keeps_writes_separate() {
	mut original := map[string]SharedKeyValue{}
	for i in 0 .. 1000 {
		original['key${i}'] = SharedKeyValue{i, 'v${i}'}
	}
	mut copy := map[string]SharedKeyValue{}
	unsafe { map_clone_sharing_keys(&copy, &original) }
	assert copy == original
	assert copy.keys() == original.keys()
	copy['new'] = SharedKeyValue{1, 'x'}
	copy.delete('key3')
	original['other'] = SharedKeyValue{2, 'y'}
	assert copy.len == 1000
	assert original.len == 1001
	assert 'key3' in original
	assert original['key3'].text == 'v3'
	assert 'new' !in original
	assert 'other' !in copy
	assert copy['key999'].text == 'v999'
	// Deleting from the copy must not free a key that the original still uses.
	for i in 0 .. 1000 {
		copy.delete('key${i}')
	}
	for i in 0 .. 1000 {
		assert original['key${i}'].n == i
	}
}

fn test_map_clone_sharing_keys_in_place_and_small_maps() {
	mut values := map[string]int{}
	values['a'] = 1
	values['b'] = 2
	unsafe { map_clone_sharing_keys(&values, &values) }
	values['c'] = 3
	values.delete('a')
	assert values.len == 2
	assert values['b'] == 2
	assert values.keys() == ['b', 'c']
	mut ints := map[int]string{}
	ints[7] = 'seven'
	mut ints_copy := map[int]string{}
	unsafe { map_clone_sharing_keys(&ints_copy, &ints) }
	ints_copy[8] = 'eight'
	assert ints.len == 1
	assert ints_copy[7] == 'seven'
	mut empty := map[string]int{}
	mut empty_copy := map[string]int{}
	unsafe { map_clone_sharing_keys(&empty_copy, &empty) }
	empty_copy['x'] = 1
	assert empty.len == 0
}
