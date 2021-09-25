fn test_if_expr_with_nested_array_call() {
	arr := ['']
	if arr.len == 1 || (arr[1] == '' && arr.all(it[0].is_letter())) {
		println('yes')
		assert true
	}
}
