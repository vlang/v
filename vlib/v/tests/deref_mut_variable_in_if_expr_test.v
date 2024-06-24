struct Foo {
	str string
}

fn (mut f Foo) foo(f2 Foo) string {
	return (if f.str != '' {
		f
	} else {
		f2
	}).str
}

fn test_deref_mut_var_in_if_expr() {
	mut foo := Foo{}
	dump(foo.foo(Foo{ str: 'a' }))
	assert foo.foo(Foo{ str: 'a' }) == 'a'
}
