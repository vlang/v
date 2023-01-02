fn sum(a ...int) int {
	mut total := 0
	for x in a {
		total += x
	}
	return total
}

fn test_fixed_array_explicit_decompose() {
	arr := [1, 2, 3, 4]!
	assert sum(...arr[..]) == 10
}
