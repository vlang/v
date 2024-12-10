fn test_main() {
	t1 := example[[4]int]([1, 2, 3, 4]!)?
	assert t1 == [1, 2, 3, 4]!
	t2 := example[[4]string](['A', 'B', 'C', 'D']!)?
	assert t2 == ['A', 'B', 'C', 'D']!
	t3 := example[[4]bool]([true, false, true, false]!)?
	assert t3 == [true, false, true, false]!
	t4 := example[[4]f64]([1.0, 2.0, 3.0, 4.0]!)?
	assert t4 == [1.0, 2.0, 3.0, 4.0]!
}

fn example[T](arr T) ?T {
	return arr
}
