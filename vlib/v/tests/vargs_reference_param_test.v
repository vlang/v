[heap]
struct Foo {
	name string
}

fn agg_stuff(stuffs ...&Foo) []&Foo {
	stuffs2 := stuffs.clone()
	return stuffs2
}

fn arr_stuff(stuffs []&Foo) []&Foo {
	stuffs2 := stuffs.clone()
	return stuffs2
}

fn test_vargs_with_reference_params() {
	foo1 := &Foo{'foo'}
	foo2 := &Foo{'bar'}

	foo11 := agg_stuff(foo1, foo2)
	println(foo11)

	foo22 := arr_stuff([foo1, foo2])
	println(foo22)

	assert '${foo11}' == '${foo22}'
}
