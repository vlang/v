const nums = [1, 2, 3]

fn test_for_char_in() {
	mut sum := 0
	for ch in nums {
		sum += ch
	}
	assert sum == 6
}

fn test_for_char_in_range() {
	mut sum := 0
	for ch in 0 .. nums.len {
		sum += nums[ch]
	}
	assert sum == 6
}

fn test_for_blank_in_range() {
	mut sum := 0
	for _ in 1 .. 3 {
		sum++
	}
	assert sum == 2
}

fn test_for_char_complex() {
	mut sum := 0
	for ch := 0; ch < nums.len; ch++ {
		sum += nums[ch]
	}
	assert sum == 6
}

fn test_for_char_in_string() {
	s := 'abcd'
	mut sum := 0
	for ch in s {
		sum += ch
	}
	assert sum == 394 // ascii codes of `a` + `b` + `c` + `d`
}

fn test_for_string_in_map() {
	m := {
		'a': 'b'
		'c': 'd'
	}
	mut acc := ''
	for k, v in m {
		acc += '${k}: ${v}, '
	}
	assert acc == 'a: b, c: d, '

	mut m2 := {
		'a': 3
		'b': 4
		'c': 5
	}
	m2.delete('b')
	acc = ''
	for k, v in m2 {
		acc += '${k}: ${v}, '
	}
	assert acc == 'a: 3, c: 5, '
}

fn test_mut_for() {
	mut vals := [1, 2, 3]
	for mut val in vals {
		(*val)++
	}
	assert vals == [2, 3, 4]
	println(vals)
}
