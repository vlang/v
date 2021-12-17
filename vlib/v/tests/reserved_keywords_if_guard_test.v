fn array_n_opt(n int) ?[]int {
	return if n >= 0 { []int{len: n} } else { none }
}

fn test_reserved_keyword_of_if_guard() {
	if array := array_n_opt(2) {
		assert array.len == 2
	} else {
		assert false
	}
}
