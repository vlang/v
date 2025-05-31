fn test_2_expressions_in_same_for_increment_part() {
	mut a := 0
	for i := 0; i < 10; i++, a++ {
		assert a == i
	}
}
