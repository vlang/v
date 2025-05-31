struct Foo {}

struct Bar {}

struct Baz {}

type Sum = Bar | Baz | Foo

fn foo(s Sum) Sum {
	match s {
		Foo, Bar { return s }
		Baz { return Baz{} }
	}
}

fn test_match_sumtype_var_aggregate() {
	a := Foo{}
	ret := foo(a)
	println(ret)
	assert '${ret}' == 'Sum(Foo{})'
}
