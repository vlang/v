struct Blah {
mut:
	arr []string
}

fn test<T>() T {
	return T{}
}

fn test_generics_struct_init() {
	mut b := test<Blah>()
	b.arr << 'item'
	println(b.arr)
	assert b.arr == ['item']
}
