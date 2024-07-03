@[translated]
module main

fn test_NotSnakeCaseFunction() {
	assert true
	assert 8 == 2 * 4
	assert 2 * 3 == 6
}

const ssf = [1, 2, 3]!

fn test_const_name_without_main_prefix() {
	assert ssf[0] == 1
}
