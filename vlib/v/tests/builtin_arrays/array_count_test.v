fn test_main() {
	a := []int{len: 10, init: index}
	assert a.count(it % 2) == 5

	b := [10]int{init: index}
	assert a.count(it % 2) == 5
}

fn test_zero() {
	a := []int{len: 10, init: index}
	assert a.count(it == 1000) == 0

	b := [10]int{init: index}
	assert a.count(it == 1000) == 0
}
