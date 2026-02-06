fn test_if_expr_with_array_call_all() {
	arr := ['']

	for i in arr {
		if i.len == 0 || i[1..].bytes().all(it.is_letter()) {
			println('empty or all char from second is letter!')
			assert true
		}
	}

	for i in arr {
		if i.len == 0 || (i[1..].bytes().all(it.is_letter())) {
			println('empty or all char from second is letter!')
			assert true
		}
	}
}

fn test_if_expr_with_array_call_any() {
	arr := ['']

	for i in arr {
		if i.len == 0 || i[1..].bytes().any(it.is_letter()) {
			println('empty or all char from second is letter!')
			assert true
		}
	}

	for i in arr {
		if i.len == 0 || (i[1..].bytes().any(it.is_letter())) {
			println('empty or all char from second is letter!')
			assert true
		}
	}
}

fn test_if_expr_with_array_call_map() {
	arr := ['abc']

	if arr.len == 1 || arr[1].bytes().map(it.is_letter())[0] {
		println('yes')
		assert true
	}

	if arr.len == 1 || (arr[1].bytes().map(it.is_letter())[0]) {
		println('yes')
		assert true
	}
}

fn test_if_expr_with_array_call_filter() {
	arr := ['abc']

	if arr.len == 1 || arr[1].bytes().filter(it.is_letter()).len == 0 {
		println('yes')
		assert true
	}

	if arr.len == 1 || arr[1].bytes().filter(it.is_letter()).len == 0 {
		println('yes')
		assert true
	}
}
