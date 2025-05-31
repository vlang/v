fn test_cross_assign_fixed_array() {
	number := 5
	ans := fib(number)

	println(ans)
	assert ans == 21
}

fn fib(n int) u64 {
	if n <= 0 {
		panic('Bad number')
	}
	return match n {
		1 | 2 {
			1
		}
		else {
			mut pair := [1, 2]!
			for _ in 0 .. n {
				pair[0], pair[1] = pair[1], pair[0] + pair[1]
			}
			pair[1]
		}
	}
}
