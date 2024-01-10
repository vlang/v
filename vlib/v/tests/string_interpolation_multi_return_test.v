struct RetTest {}

fn (r RetTest) return_multi_1() (int, string) {
	return 1, 'aaa'
}

fn (r RetTest) return_multi_2() (string, bool, []int) {
	return 'aaa', true, [1, 2, 3]
}

fn (r RetTest) return_multi_3() (f32, bool, []string) {
	return 2.2, false, ['a', 'b', 'c']
}

fn return_multi_4() (int, string) {
	return 1, 'aaa'
}

fn return_multi_5() (string, bool, []int) {
	return 'aaa', true, [1, 2, 3]
}

fn return_multi_6() (f32, bool, []string) {
	return 2.2, false, ['a', 'b', 'c']
}

fn test_multi_return_interpolation() {
	r := RetTest{}

	s1 := '${r.return_multi_1()}'
	assert s1 == "(1, 'aaa')"

	s2 := '${r.return_multi_2()}'
	assert s2 == "('aaa', true, [1, 2, 3])"

	s3 := '${r.return_multi_3()}'
	assert s3 == "(2.2, false, ['a', 'b', 'c'])"

	s4 := '${return_multi_4()}'
	assert s4 == "(1, 'aaa')"

	s5 := '${return_multi_5()}'
	assert s5 == "('aaa', true, [1, 2, 3])"

	s6 := '${return_multi_6()}'
	assert s6 == "(2.2, false, ['a', 'b', 'c'])"
}
