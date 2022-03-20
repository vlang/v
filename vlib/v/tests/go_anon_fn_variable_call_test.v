fn sum(a int, b int) int {
	// Simply proxy the function into an anonymous function for demo purposes
	sum_func := fn (a int, b int) int {
		return a + b
	}

	// and run it concurrently
	g := go sum_func(a, b)

	result := g.wait()
	return result
}

fn test_go_anon_fn_variable_call() {
	ret := sum(22, 33)
	println(ret)
	assert ret == 55
}
