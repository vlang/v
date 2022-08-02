fn gfn1<T>(var T) T {
	return var
}

fn gfn2<T>(var T) T {
	return gfn1<T>(var)
}

fn gfn3<T>(var T) T {
	return gfn2<T>(var)
}

fn gfn4<T>(var T) T {
	return gfn3<T>(var)
}

fn test_generics_with_cascaded_multiple_nested_generics_fn() {
	println(gfn4(1234))
	assert gfn4(1234) == 1234
}
