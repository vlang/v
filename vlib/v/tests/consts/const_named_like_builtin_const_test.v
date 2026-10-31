// The consts below have the names of private consts of the builtin module: `degree`,
// `max_len` and `children_bytes` size the B-tree nodes of SortedMap, and `hashbits`,
// `probe_inc` and `max_load_factor` drive the hash map. A main const of the same name
// must not replace the builtin one in builtin code, whatever its value or type.
const degree = 'abc'
const max_len = 100
const children_bytes = [1, 2, 3]
const hashbits = 2
const probe_inc = 'probe'
const max_load_factor = 3

fn test_main_consts_keep_their_own_values() {
	assert degree == 'abc'
	assert max_len == 100
	assert children_bytes == [1, 2, 3]
	assert hashbits == 2
	assert probe_inc == 'probe'
	assert max_load_factor == 3
}

fn test_builtin_maps_keep_their_own_consts() {
	mut m := map[string]int{}
	for i in 0 .. 20000 {
		m[i.str()] = i
	}
	for i in 0 .. 20000 {
		if i % 3 == 0 {
			m.delete(i.str())
		}
	}
	mut sum := i64(0)
	for i in 0 .. 20000 {
		sum += m[i.str()] or { 0 }
	}
	mut expected := i64(0)
	for i in 0 .. 20000 {
		if i % 3 != 0 {
			expected += i
		}
	}
	assert m.len == 13333
	assert sum == expected
	mut ints := map[int]string{}
	for i in 0 .. 1000 {
		ints[i * 7] = i.str()
	}
	assert ints.len == 1000
	assert ints[6993] == '999'
	assert 7000 !in ints
}
