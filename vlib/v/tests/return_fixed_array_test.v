type Abc = [3]int

fn return_fixed_array() [3]int {
	return [1, 2, 3]!
}

fn return_fixed_array_in_multi_return() ([3]int, [3]int) {
	return [1, 2, 3]!, [4, 5, 6]!
}

fn return_fixed_array_with_alias() Abc {
	return [1, 2, 3]!
}

fn test_with_alias() {
	a := return_fixed_array_with_alias()
	assert a == [1, 2, 3]!
}

fn test_without_alias() {
	a := return_fixed_array()
	assert a == [1, 2, 3]!
}

// for issue 20366: returns mut fixed array
fn returns_mut_fixed_array(mut fixed_array [3]int) [3]int {
	return fixed_array
}

fn test_returns_mut_fixed_array() {
	mut fixed := [59, 101, 200]!
	res := returns_mut_fixed_array(mut fixed)
	assert res == [59, 101, 200]!
}

// for issue 20373: returns option / result fixed array
pub fn returns_option_fixed_array(fixed [3]int) ?[3]int {
	return fixed
}

pub fn returns_result_fixed_array(fixed [3]int) ![3]int {
	return fixed
}

fn test_returns_option_and_result_fixed_array() {
	mut fixed := [int(59), 101, 200]!

	mut res := returns_option_fixed_array(fixed) or {
		assert false
		return
	}
	assert res == [59, 101, 200]!

	res = returns_result_fixed_array(fixed) or {
		assert false
		return
	}
	assert res == [59, 101, 200]!
}
