fn test_for_c_label_continue_break() {
	mut rets := []int{}
	outer: for i := 4; true; i++ {
		println(i)
		rets << i
		for {
			if i < 7 {
				continue outer
			} else {
				break outer
			}
		}
	}
	assert rets.len == 4
	assert rets[0] == 4
	assert rets[1] == 5
	assert rets[2] == 6
	assert rets[3] == 7
}

fn test_for_in_array_label_continue_break() {
	mut rets := []int{}
	arr := [4, 5, 6, 7, 8, 9]
	outer: for i in arr {
		println(i)
		rets << i
		for {
			if i < 7 {
				continue outer
			} else {
				break outer
			}
		}
	}
	assert rets.len == 4
	assert rets[0] == 4
	assert rets[1] == 5
	assert rets[2] == 6
	assert rets[3] == 7
}

fn test_for_in_fixed_array_label_continue_break() {
	mut rets := []int{}
	arr := [4, 5, 6, 7, 8, 9]!
	outer: for i in arr {
		println(i)
		rets << i
		for {
			if i < 7 {
				continue outer
			} else {
				break outer
			}
		}
	}
	assert rets.len == 4
	assert rets[0] == 4
	assert rets[1] == 5
	assert rets[2] == 6
	assert rets[3] == 7
}

fn test_for_in_map_label_continue_break() {
	mut rets := []string{}
	m := map{'a': 4, 'b': 5, 'c': 6, 'd': 7, 'e': 8, 'f': 9}
	outer: for k, v in m {
		println('$k, $v')
		rets << '$k, $v'
		for {
			if v < 7 {
				continue outer
			} else {
				break outer
			}
		}
	}
	assert rets.len == 4
	assert rets[0] == 'a, 4'
	assert rets[1] == 'b, 5'
	assert rets[2] == 'c, 6'
	assert rets[3] == 'd, 7'
}
