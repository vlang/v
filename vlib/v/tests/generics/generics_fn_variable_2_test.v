fn g[T]() u32 {
	return sizeof(T)
}

fn f[T]() u32 {
	p := g[T]
	return p()
}

fn test_generic_fn_variable() {
	r1 := f[int]()
	println(r1)
	assert r1 == 4

	r2 := f[string]()
	println(r2)
	assert r2 == $if x64 {
		16
	} $else {
		12
	}

	r3 := f[f64]()
	println(r3)
	assert r3 == 8

	r4 := f[bool]()
	println(r4)
	assert r4 == 1
}
