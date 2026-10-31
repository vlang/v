fn test_sparse_map_clone_keeps_live_entry_order_and_independent_writes() {
	mut original := map[string]int{}
	for i in 0 .. 2048 {
		original['key${i}'] = i
	}
	retained := [31, 777, 1023, 1987]
	for i in 0 .. 2048 {
		if i !in retained {
			original.delete('key${i}')
		}
	}
	mut copy := original.clone()
	assert copy.keys() == retained.map('key${it}')
	assert copy == original
	copy.delete('key777')
	copy['new'] = 9000
	original['key777'] = 42
	assert copy.keys() == ['key31', 'key1023', 'key1987', 'new']
	assert original.keys() == retained.map('key${it}')
	assert 'new' !in original
	assert copy['key1023'] == 1023
	assert original['key777'] == 42
}

fn test_reserved_sparse_map_clone_keeps_wide_integer_keys_and_reuses_storage() {
	mut original := map[u128]string{}
	original.reserve(32768)
	keys := [u128(1), u128(1) << 80, (u128(1) << 100) | 7]
	for i, key in keys {
		original[key] = 'value${i}'
	}
	mut copy := original.clone()
	assert copy == original
	assert copy.keys() == keys
	original.clear()
	for i, key in keys {
		assert copy[key] == 'value${i}'
	}
	copy.delete(keys[1])
	copy[keys[1] + 1] = 'appended'
	assert copy.keys() == [keys[0], keys[2], keys[1] + 1]
	assert original.len == 0
}
