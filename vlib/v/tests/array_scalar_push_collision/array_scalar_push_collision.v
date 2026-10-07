module array_scalar_push_collision

fn array_push(mut values []int, value &int) {
	values[0] = *value + 1000
}

// check exercises a user function with the runtime append helper's name.
pub fn check() {
	mut values := []int{len: 1, cap: 4, init: 1}
	value := 42
	array_push(mut values, &value)
	assert values == [1042]
}
