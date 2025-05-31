struct Bar {
	f []fn (int) int
}

fn func(n int) int {
	return n
}

@[direct_array_access]
fn (b Bar) foo() int {
	return b.f[0](22)
}

fn test_array_of_fns_index_call_with_direct_array_access() {
	bar := Bar{[func]}
	ret := bar.foo()
	println(ret)
	assert ret == 22
}
