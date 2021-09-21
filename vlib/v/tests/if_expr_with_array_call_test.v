fn test_if_expr_with_array_all_any() {
	arr := ['']

	for i in arr {
		if i.len == 0 || i[1..].bytes().all(it.is_letter()) {
			println('empty or all char from second is letter!')
			assert true
		}
	}

	for i in arr {
		if i.len == 0 || i[1..].bytes().any(it.is_letter()) {
			println('empty or all char from second is letter!')
			assert true
		}
	}
}
