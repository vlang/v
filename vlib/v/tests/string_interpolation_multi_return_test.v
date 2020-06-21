fn test_multi_return_interpolation() {
	s1 := '${return_multi_1()}'
	assert s1 == "(1, 'aaa')"

	s2 := '${return_multi_2()}'
	assert s2 == "('aaa', true, [1, 2, 3])"

	s3 := '${return_multi_3()}'
	assert s3 == "(2.2, false, ['a', 'b', 'c'])"
}

fn return_multi_1() (int, string) {
	return 1, 'aaa'
}

fn return_multi_2() (string, bool, []int) {
	return 'aaa', true, [1,2,3]
}

fn return_multi_3() (f32, bool, []string) {
	return 2.2, false, ['a', 'b', 'c']
}