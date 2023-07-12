type Abc = [3]int

fn return_fixed_array() [3]int {
	return [1, 2, 3]!
}

fn return_fixed_array_in_multi_return() ([3]int, [3]int) {
	return [1, 2, 3]!, [4, 5, 6]!
}

// vfmt off
fn return_fixed_array_with_alias() Abc {
	return [1, 2, 3]!
}
// vfmt on

fn test_with_alias() {
	a := return_fixed_array_with_alias()
	assert a == [1, 2, 3]!
}

fn test_without_alias() {
	a := return_fixed_array()
	assert a == [1, 2, 3]!
}
