struct MapLengthReference {
	values &map[string]int
}

fn map_length[T](mut values map[string]T) int {
	return values.len
}

fn map_reference(values &map[string]int) &map[string]int {
	return values
}

fn test_map_length_through_references_and_mutable_parameters() {
	mut values := {
		'one': 1
		'two': 2
	}
	assert map_length(mut values) == 2
	pointer := &values
	nested := &pointer
	assert pointer.len == 2
	assert (*nested).len == 2
	holder := MapLengthReference{pointer}
	assert holder.values.len == 2
	assert map_reference(pointer).len == 2
	values.clear()
	assert map_length(mut values) == 0
	assert pointer.len == 0
}
