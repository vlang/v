fn test_map_clear_done_several_times() {
	mut ints := map[int]int{}

	ints[5] = 5
	dump(ints.len)
	assert ints.len == 1

	ints.clear()
	dump(ints.len)
	assert ints.len == 0

	ints[5] = 3
	dump(ints.len)
	assert ints.len == 1

	ints.clear()
	dump(ints.len)
	assert ints.len == 0

	ints[5] = 123
	dump(ints.len)
	assert ints.len == 1
}

fn test_map_clear_in_loop_metas_should_be_cleared_too() {
	mut ints := map[int]int{}
	for i in 0 .. 100 {
		ints[i] = i * 123
		ints.clear()
		assert ints.len == 0
		// dump(ints)
		ints[i] = i
		// dump(ints)
		assert ints.len == 1
		ints[1000 + i] = 1000 * i
		assert ints.len == 2
	}
}

fn test_map_clear_in_loop_delete_keys() {
	mut ints := map[int]int{}
	for i in 0 .. 100 {
		ints[i] = i * 123
		ints[i + 2] = i
		ints.delete(i)
		ints.delete(i + 1)
		ints.delete(i + 2)
		ints.delete(i + 3)
		ints.delete(i + 4)
		ints.clear()
		assert ints.len == 0
		// dump(ints)
		ints[i] = 5
		// dump(ints)
		assert ints.len == 1
		ints[1000 + i] = 1000 * i
		assert ints.len == 2
	}
}
