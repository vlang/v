fn a() {}

fn test_array_push_keeps_its_receiver_type_with_a_same_named_function() {
	a()
	mut values := []int{cap: 1}
	values << 1
	values << 2
	assert values == [1, 2]
	// Keep spare capacity in the shared buffer so pushing takes the detach path.
	mut slice := unsafe { values[..] }
	assert slice.pop() == 2
	slice << 7
	assert slice == [1, 7]
	assert values == [1, 2]
}
