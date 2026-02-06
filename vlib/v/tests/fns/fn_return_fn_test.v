fn make_squarer() fn (int) int {
	return fn (n int) int {
		return n * n
	}
}

fn test_fn_return_fn() {
	squarer := make_squarer()
	assert squarer(10) == 100
}

fn test_anon_fn_return_fn() {
	make_doubler := fn () fn (int) int {
		return fn (n int) int {
			return n + n
		}
	}
	doubler := make_doubler()
	assert doubler(10) == 20
}
