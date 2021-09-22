fn create_random_frames(amount int, pixels int) [][][]int {
	return [[[amount, pixels]]]
}

fn test_go_can_be_used_with_functions_returning_arrays() {
	x := go create_random_frames(2, 2)
	res := x.wait()
	assert res == [[[2, 2]]]
}
