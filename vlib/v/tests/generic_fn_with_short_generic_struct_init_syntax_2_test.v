struct Params[T] {
	a []T
	b int
}

fn take_input[T](p Params[T]) []f32 {
	mut res := []f32{}
	for x in p.a {
		res << f32(x)
	}
	res << f32(p.b)
	return res
}

fn test_generic_fn_with_generic_struct_init_syntax() {
	a_in := [int(1), 2, 4]
	res := take_input(a: a_in, b: 3)

	// res := take_input(Params[int]{a: a_in, b: 3})  // this works
	println(res)
	assert res == [f32(1.0), 2.0, 4.0, 3.0]
}
