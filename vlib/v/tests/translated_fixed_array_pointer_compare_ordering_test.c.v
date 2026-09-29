@[translated]
module main

fn translated_array_next_index(mut calls []int) int {
	calls << 0
	return 0
}

fn test_array_pointer_comparisons_keep_original_storage() {
	mut rows := [[1, 2]!, [3, 4]!]!
	pointer := unsafe { &rows[0][0] }
	flag := true
	mut calls := []int{}
	assert rows[translated_array_next_index(mut calls)] == if flag { pointer } else { pointer }
	assert calls.len == 1
	assert (if flag { pointer } else { pointer }) == rows[translated_array_next_index(mut calls)]
	assert calls.len == 2
}
