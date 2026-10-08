module builtin

struct EmptyMapValue {}

fn test_empty_map_values_survive_growth_reserve_and_deletion() {
	mut values := map[string]EmptyMapValue{}
	for i in 0 .. 100 {
		values['key${i}'] = EmptyMapValue{}
	}
	values.reserve(512)
	for i in 100 .. 512 {
		values['key${i}'] = EmptyMapValue{}
	}
	assert values.len == 512
	mut copy := values.clone()
	for i in 0 .. 512 {
		assert 'key${i}' in values
		assert 'key${i}' in copy
		if i % 2 == 0 {
			values.delete('key${i}')
		}
	}
	assert values.len == 256
	assert copy.len == 512
	copy.reserve(1024)
	copy['only in clone'] = EmptyMapValue{}
	assert copy.len == 513
	assert 'only in clone' !in values
	for i in 0 .. 512 {
		assert ('key${i}' in values) == (i % 2 != 0)
	}
	values.clear()
	values.reserve(1024)
	values['reused'] = EmptyMapValue{}
	assert values.len == 1
	assert 'reused' in values
}

fn test_dense_array_zero_value_size_with_every_c_compiler() {
	// TinyCC assigns empty structs one byte, so exercise zero-sized storage explicitly too.
	mut dense := new_dense_array(int(sizeof(int)), 0)
	// DenseArray stores raw key bytes; fill and inspect its integer slots directly.
	for i in 0 .. 100 {
		index := dense.expand()
		unsafe {
			*(&int(dense.key(index))) = i
		}
	}
	dense.reserve(512)
	for i in 100 .. 512 {
		index := dense.expand()
		unsafe {
			*(&int(dense.key(index))) = i
		}
	}
	assert dense.len == 512
	for i in 0 .. 512 {
		assert unsafe { *(&int(dense.key(i))) } == i
		assert !isnil(dense.value(i))
	}
}
