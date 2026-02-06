struct Foo {
	bar string
}

fn test_go_anon_fn_call_with_ref_arg() {
	foo := &Foo{
		bar: 'hello'
	}
	g := spawn fn (foo Foo) string {
		return foo.bar
	}(foo)
	ret := g.wait()
	println(ret)
	assert ret == 'hello'
}
