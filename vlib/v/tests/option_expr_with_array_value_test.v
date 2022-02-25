struct Empty {
	empty string
}

fn print_error() ?[]Empty {
	mut test := []Empty{}
	test << Empty{
		empty: 'Test'
	}
	if test[0].empty != '' {
		return error('Not empty')
	}
	return test
}

fn test_option_expr_with_array_value() {
	test_error := print_error() or {
		eprintln(err)
		[]Empty{}
	}
	println(test_error)
	assert '$test_error' == '[]'
}
