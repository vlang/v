// Regression test for reserve_metas under -prealloc: rehash must rebuild
// metas from fresh zeroed storage instead of over-reading an unknown-size
// block through v_realloc. Inserting enough keys forces expand/rehash.
fn test_map_rehash_rebuilds_metas() {
	mut m := map[string]int{}
	for i in 0 .. 3000 {
		key := 'key_${i}'
		m[key] = i
		assert m[key] == i
	}
	assert m.len == 3000
	for i in 0 .. 3000 {
		assert m['key_${i}'] == i
	}
	m.reserve(6000)
	assert m.len == 3000
	for i in 0 .. 3000 {
		assert m['key_${i}'] == i
	}
	for i in 0 .. 1500 {
		m.delete('key_${i}')
	}
	assert m.len == 1500
	for i in 1500 .. 3000 {
		assert m['key_${i}'] == i
	}
}
