fn test_if_expr_with_array_call() {
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

	arr2 := ['abc']

	if arr2.len == 1 || arr2[1].bytes().map(it.is_letter())[0] {
		println('yes')
		assert true
	}

	if arr2.len == 1 || (arr2[1].bytes().map(it.is_letter())[0]) {
		println('yes')
		assert true
	}

	if arr2.len == 1 || arr2[1].bytes().filter(it.is_letter()).len == 0 {
		println('yes')
		assert true
	}

	if arr2.len == 1 || (arr2[1].bytes().filter(it.is_letter()).len == 0) {
		println('yes')
		assert true
	}
}
