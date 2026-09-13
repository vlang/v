fn main() {
	mut nums := [[1, 2], [3, 4]]
	nums2 := nums.clone()
	nums[0][0] = 9
	assert nums2[0][0] == 1

	mut m := {
		'a': 1
		'b': 2
	}
	for k, v in m {
		assert m[k] == v
	}

	x := char(0b11111111)
	assert int(x) == -1

	s := 'hello! world!'
	assert s.replace_each(['!', ':)', 'hello', 'hey']) == 'hey:) world:)'
}
