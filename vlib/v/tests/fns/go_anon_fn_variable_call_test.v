// vtest flaky: true
// vtest retry: 3

fn sum1(a int, b int) int {
	sum_func1 := fn (a int, b int) int {
		return a + b
	}
	sum_func2 := sum_func1

	g := spawn sum_func2(a, b)

	result := g.wait()
	return result
}

fn add(a int, b int) int {
	return a + b
}

fn sum2(a int, b int) int {
	sum_func1 := add
	sum_func2 := sum_func1

	g := spawn sum_func2(a, b)

	result := g.wait()
	return result
}

fn test_go_anon_fn_variable_call() {
	ret1 := sum1(22, 33)
	println(ret1)
	assert ret1 == 55

	ret2 := sum2(2, 3)
	println(ret2)
	assert ret2 == 5
}
