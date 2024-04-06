fn clip[N](x []N, min N, max N) []N {
	mut result := []N{cap: x.len}
	for value in x {
		result << if value < min {
			min
		} else if value > max {
			max
		} else {
			value
		}
	}
	return result
}

fn test_generic_comparison_for_conditional_assign() {
	a := [1, 2, 3, 4, 5]
	dump(a)
	b := clip(a, 2, 4)
	dump(b)
	assert b == [2, 2, 3, 4, 4]
}
