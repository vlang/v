const (
	nums = [1, 2, 3]
)

fn test_for_char_in() {
	mut sum := 0
	for char in nums {
		sum += char
	}
	assert sum == 6
}

fn test_for_char_in_range() {
	mut sum := 0
	for char in 0 .. nums.len {
		sum += nums[char]
	}
	assert sum == 6
}

fn test_for_char_complex() {
	mut sum := 0
	for char := 0; char < nums.len; char++ {
		sum += nums[char]
	}
	assert sum == 6
}

fn test_for_char_in_string() {
	s := 'abcd'
	mut sum := 0
	for char in s {
		sum += char
	}
	assert sum == 394 // ascii codes of `a` + `b` + `c` + `d`
}

fn test_for_char_in_map() {
	m := {
		'a': 'b'
		'c': 'd'
	}
	mut acc := ''
	for k, char in m {
		acc += '$k: $char, '
	}
	assert acc == 'a: b, c: d, '
}
