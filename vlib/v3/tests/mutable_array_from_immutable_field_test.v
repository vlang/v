struct ArrayOutput {
	values []int
}

fn test_mutable_array_can_be_moved_from_an_immutable_struct_field() {
	output := ArrayOutput{
		values: [1, 2]
	}
	mut values := output.values.clone()
	values << 3
	assert values == [1, 2, 3]
}
