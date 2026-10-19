fn test_empty_map_clones_remain_independently_writable_after_clear_and_delete() {
	for clear_all in [false, true] {
		mut original := map[string]int{}
		for i in 0 .. 2048 {
			original['key${i}'] = i
		}
		if clear_all {
			original.clear()
		} else {
			for i in 0 .. 2048 {
				original.delete('key${i}')
			}
		}
		mut copy := original.clone()
		assert original.len == 0 && copy.len == 0
		copy['copy'] = 17
		original['original'] = 23
		assert copy == {
			'copy': 17
		}
		assert original == {
			'original': 23
		}
		copy.delete('copy')
		assert copy.len == 0
		assert original['original'] == 23
	}
}

fn test_empty_integer_map_clone_retains_key_and_value_operations() {
	mut original := map[u64]string{}
	for i in 0 .. 1024 {
		original[u64(i)] = 'value${i}'
	}
	original.clear()
	mut copy := original.clone()
	copy[u64(1) << 40] = 'large key'
	assert copy[u64(1) << 40] == 'large key'
	assert original.len == 0
	copy.clear()
	copy[7] = 'reused'
	assert copy == {
		u64(7): 'reused'
	}
}
