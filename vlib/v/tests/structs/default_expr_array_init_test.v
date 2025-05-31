struct Foo {
pub mut:
	integer_range_for_discrete []int = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
}

fn foo(a string) Foo {
	return match a {
		'a' { Foo{} }
		else { panic('foo') }
	}
}

fn test_main() {
	assert foo('a') == Foo{}
}
