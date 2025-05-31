type Foo = int | string | Bar

struct Bar {}

fn foo_result_struct_second() !Foo {
	return match true {
		true { 1 }
		else { Bar{} }
	}
}

fn foo_option_struct_second() ?Foo {
	return match true {
		true { 1 }
		else { Bar{} }
	}
}

fn foo_result_string_second() !Foo {
	return match true {
		true { 1 }
		else { '' }
	}
}

fn foo_option_string_second() ?Foo {
	return match true {
		true { 1 }
		else { '' }
	}
}

fn foo_result_struct_first() !Foo {
	return match true {
		true { Bar{} }
		else { 7 }
	}
}

fn foo_option_struct_first() ?Foo {
	return match true {
		true { Bar{} }
		else { 7 }
	}
}

fn test_return_match_expr_of_sumtype_opt_res() {
	mut ret := Foo{}

	ret = foo_result_struct_second() or { return }
	println(ret)
	assert '${ret}' == 'Foo(1)'

	ret = foo_option_struct_second() or { return }
	println(ret)
	assert '${ret}' == 'Foo(1)'

	ret = foo_result_string_second() or { return }
	println(ret)
	assert '${ret}' == 'Foo(1)'

	ret = foo_option_string_second() or { return }
	println(ret)
	assert '${ret}' == 'Foo(1)'

	ret = foo_result_struct_first() or { return }
	println(ret)
	assert '${ret}' == 'Foo(Bar{})'

	ret = foo_option_struct_first() or { return }
	println(ret)
	assert '${ret}' == 'Foo(Bar{})'
}
