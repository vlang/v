type Foo = int | string

fn foo1() !Foo {
	return match true {
		true { 1 }
		else { '' }
	}
}

fn foo2() ?Foo {
	return match true {
		true { 1 }
		else { '' }
	}
}

fn test_return_match_expr_of_sumtype_opt_res() {
	ret1 := foo1() or { return }
	println(ret1)
	assert '$ret1' == 'Foo(1)'

	ret2 := foo2() or { return }
	println(ret2)
	assert '$ret2' == 'Foo(1)'
}
