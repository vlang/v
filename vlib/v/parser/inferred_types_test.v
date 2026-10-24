module parser

fn test_inferred_types_relocate_every_nested_expression_link() {
	assert shift_inferred_type('![]&ui2.Signal[typeof(__expr_12)]', 100) == '![]&ui2.Signal[typeof(__expr_112)]'
	assert shift_inferred_type('fn(typeof(__expr_1)) typeof(__expr_22)', 10) == 'fn(typeof(__expr_11)) typeof(__expr_32)'
	assert shift_inferred_type('[]int', 10) == '[]int'
}
