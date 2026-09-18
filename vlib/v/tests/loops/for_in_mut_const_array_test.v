const mutable_iteration_values = [1, 2, 3]

fn test_for_in_mut_val_of_const_array() {
	for mut value in mutable_iteration_values {
		value++
	}
	assert mutable_iteration_values == [2, 3, 4]
}
