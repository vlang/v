struct Foo {
	foo string
	bar ?string
}

fn (f Foo) str() string {
	mut ret := f.foo
	bar_value := f.bar as string
	if bar_value == '' {
		return ret
	}
	return ret + '%' + bar_value
}

fn test_selector_expr_as_cast() {
	ff := Foo{
		foo: 'Fooooo'
	}
	assert '${ff}' == 'Fooooo'
}
