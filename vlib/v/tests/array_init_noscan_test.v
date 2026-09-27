type NoscanScalar = f64

fn test_scalar_array_initializers_preserve_noscan() {
	zeros := []f64{len: 3, cap: 7}
	assert zeros == [0.0, 0.0, 0.0]
	assert zeros.cap == 7
	values := []NoscanScalar{len: 2, init: NoscanScalar(1.5)}
	assert values[0] == NoscanScalar(1.5)
	assert values[1] == NoscanScalar(1.5)
	$if gcboehm_opt ? {
		assert zeros.flags.has(.noscan_data)
		assert values.flags.has(.noscan_data)
	}
}

fn test_nested_array_initializers_keep_scalar_rows_noscan() {
	mut matrix := [][]f64{len: 2, init: []f64{len: 3, init: 0.0}}
	assert !matrix.flags.has(.noscan_data)
	matrix[0][0] = 1.25
	assert matrix[0].data != matrix[1].data
	assert matrix[1][0] == 0.0
	$if gcboehm_opt ? {
		assert matrix[0].flags.has(.noscan_data)
		assert matrix[1].flags.has(.noscan_data)
	}
	matrix[0] << 2.5
	assert matrix[0] == [1.25, 0.0, 0.0, 2.5]
	assert matrix[1] == [0.0, 0.0, 0.0]
	$if gcboehm_opt ? {
		assert matrix[0].flags.has(.noscan_data)
	}
}

fn test_empty_scalar_rows_preserve_noscan_on_growth() {
	mut matrix := [][]f64{len: 2}
	assert !matrix.flags.has(.noscan_data)
	$if gcboehm_opt ? {
		assert matrix[0].flags.has(.noscan_data)
		assert matrix[1].flags.has(.noscan_data)
	}
	matrix[0] << 3.5
	assert matrix[0] == [3.5]
	assert matrix[1].len == 0
	$if gcboehm_opt ? {
		assert matrix[0].flags.has(.noscan_data)
	}
}

fn test_pointer_containing_rows_remain_scanned() {
	mut words := [][]string{len: 2, init: []string{len: 3, init: 'row'}}
	assert !words.flags.has(.noscan_data)
	assert !words[0].flags.has(.noscan_data)
	assert !words[1].flags.has(.noscan_data)
	words[0][0] = 'changed'
	assert words[1][0] == 'row'
	value := 42
	pointers := []&int{len: 2, init: &value}
	assert !pointers.flags.has(.noscan_data)
	assert *pointers[0] == 42
}
