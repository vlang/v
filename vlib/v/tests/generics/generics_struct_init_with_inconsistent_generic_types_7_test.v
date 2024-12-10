struct MyData[T] {
}

fn myfunc[U](a MyData[U]) string {
	println(a)
	return '${a}'
}

fn test_generics_struct_init_with_inconsistent_generic_types() {
	d1 := MyData[int]{}
	r1 := myfunc(d1)
	println(r1)
	assert r1 == 'MyData[int]{}'
}
