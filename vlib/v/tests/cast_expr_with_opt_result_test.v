type Sum = int | string

struct Test {
}

fn (t Test) test() !Sum {
	return 0
}

fn (t Test) test2() ?Sum {
	return 0
}

fn ret_sum() !Sum {
	return 0
}

fn ret_sum2() ?Sum {
	return 0
}

fn test_main() {
	value := ret_sum() as int
	assert value == 0

	value2 := ret_sum2() as int
	assert value2 == 0

	t := Test{}
	value3 := t.test() as int
	assert value3 == 0

	value4 := t.test2() as int
	assert value4 == 0
}
