fn test_if_expr_with_nested_array_call1() {
	arr := ['']
	if arr.len == 1 || (arr[1] == '' && arr.all(it[0].is_letter())) {
		println('yes')
		assert true
	}
}

fn test_if_expr_with_nested_array_call2() {
	arr := ['abc']
	if (arr.len == 1 && arr.all(it.len > 0 && it.bytes().any(it == `b`))) || arr[1] == '' {
		println('yes')
		assert true
	}
}

fn test_if_expr_with_nested_array_call3() {
	arr := ['abc']
	if arr.len == 0 || arr.all(it.bytes().map(it).filter(it != ` `).any(it == `c`)) {
		println('yes')
		assert true
	}
}
