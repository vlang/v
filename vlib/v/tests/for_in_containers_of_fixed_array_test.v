fn test_for_in_array_of_fixed_array() {
	mut rets := []string{}
	arr := [][2]int{len: 3}

	for pair in arr {
		println(pair)
		rets << '${pair}'
	}
	assert rets[0] == '[0, 0]'
	assert rets[1] == '[0, 0]'
	assert rets[2] == '[0, 0]'
}

fn test_for_mut_in_array_of_fixed_array() {
	mut rets := []string{}
	mut arr := [][2]int{len: 3}

	for mut pair in arr {
		println(pair)
		rets << '${pair}'
	}
	assert rets[0] == '[0, 0]'
	assert rets[1] == '[0, 0]'
	assert rets[2] == '[0, 0]'
}

fn test_for_in_fixed_array_of_fixed_array() {
	mut rets := []string{}
	arr := [[1, 2]!, [3, 4]!, [5, 6]!]!

	for pair in arr {
		println(pair)
		rets << '${pair}'
	}
	assert rets[0] == '[1, 2]'
	assert rets[1] == '[3, 4]'
	assert rets[2] == '[5, 6]'
}

fn test_for_mut_in_fixed_array_of_fixed_array() {
	mut rets := []string{}
	mut arr := [[1, 2]!, [3, 4]!, [5, 6]!]!

	for mut pair in arr {
		println(pair)
		rets << '${pair}'
	}
	assert rets[0] == '[1, 2]'
	assert rets[1] == '[3, 4]'
	assert rets[2] == '[5, 6]'
}

fn test_for_in_fixed_array_of_fixed_array_literal() {
	mut rets := []string{}

	for pair in [[1, 2]!, [3, 4]!, [5, 6]!]! {
		println(pair)
		rets << '${pair}'
	}
	assert rets[0] == '[1, 2]'
	assert rets[1] == '[3, 4]'
	assert rets[2] == '[5, 6]'
}

fn test_for_in_map_of_fixed_array() {
	mut rets := []string{}
	m := {
		'aa': [1, 2]!
		'bb': [3, 4]!
		'cc': [5, 6]!
	}

	for k, v in m {
		println('${k}, ${v}')
		rets << '${k}, ${v}'
	}
	assert rets[0] == 'aa, [1, 2]'
	assert rets[1] == 'bb, [3, 4]'
	assert rets[2] == 'cc, [5, 6]'
}

fn test_for_in_map_of_fixed_array_literal() {
	mut rets := []string{}

	for k, v in {
		'aa': [1, 2]!
		'bb': [3, 4]!
		'cc': [5, 6]!
	} {
		println('${k}, ${v}')
		rets << '${k}, ${v}'
	}
	assert rets[0] == 'aa, [1, 2]'
	assert rets[1] == 'bb, [3, 4]'
	assert rets[2] == 'cc, [5, 6]'
}

fn test_for_mut_in_map_of_fixed_array() {
	mut rets := []string{}
	mut m := {
		'aa': [1, 2]!
		'bb': [3, 4]!
		'cc': [5, 6]!
	}

	for k, mut v in m {
		println('${k}, ${v}')
		rets << '${k}, ${v}'
	}
	assert rets[0] == 'aa, [1, 2]'
	assert rets[1] == 'bb, [3, 4]'
	assert rets[2] == 'cc, [5, 6]'
}
