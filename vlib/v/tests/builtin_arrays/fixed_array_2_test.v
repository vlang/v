struct Test {}

fn (t Test) conversion_error(value u16) [4]u8 {
	return conversion_error(value)
}

fn arr_opt() ?[2]int {
	return [1, 2]!
}

fn conversion_error(value u16) [4]u8 {
	mut return_value := [4]u8{}
	for i := 0; i < 4; i++ {
		return_value[i] = u8(value)
	}
	return return_value
}

fn test_assign() {
	// -- Block below works fine
	value := conversion_error(42)
	println(value)
}

fn test_assign_method() {
	value2 := Test{}.conversion_error(42)
	println(value2)
}

fn test_ret() {
	// -- Block above works fine
	println(conversion_error(42))
	println(Test{}.conversion_error(42))
}

fn test_assign_dump() {
	y := dump(conversion_error(42))
	dump(y)
	y2 := dump(Test{}.conversion_error(42))
	dump(y2)
}

fn test_assert() {
	a := [1, 2]!
	assert dump(a) == [1, 2]!
}

fn test_call() {
	conversion_error(42)
	Test{}.conversion_error(42)
	dump(arr_opt())
}
