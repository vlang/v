fn test_nested_if_expr_method_call() {
	str_from_nested_exp1 := if true {
		if true { 'foo.bar' } else { 'foo.bar.baz' }.all_after('foo.')
	} else {
		'foo'
	}
	println(str_from_nested_exp1)
	assert str_from_nested_exp1 == 'bar'

	str_from_nested_exp2 := if true {
		(if true { 'foo.bar' } else { 'foo.bar.baz' }).all_after('foo.')
	} else {
		'foo'
	}
	println(str_from_nested_exp2)
	assert str_from_nested_exp2 == 'bar'

	str_from_nested_exp3 := if true {
		'foo'
	} else {
		if true { 'foo.bar' } else { 'foo.bar.baz' }.all_after('foo.')
	}
	println(str_from_nested_exp3)
	assert str_from_nested_exp3 == 'foo'

	str_from_nested_exp4 := if true {
		'foo'
	} else {
		(if true { 'foo.bar' } else { 'foo.bar.baz' }).all_after('foo.')
	}
	println(str_from_nested_exp4)
	assert str_from_nested_exp4 == 'foo'
}
