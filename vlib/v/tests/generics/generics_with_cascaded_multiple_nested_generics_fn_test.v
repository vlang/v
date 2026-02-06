fn gfn1[T](var T) T {
	return var
}

fn gfn2[T](var T) T {
	return gfn1[T](var)
}

fn gfn3[T](var T) T {
	return gfn2[T](var)
}

fn gfn4[T](var T) T {
	return gfn3[T](var)
}

// don't give concrete types
fn gfn2_infer[T](var T) T {
	return gfn1(var)
}

fn gfn3_infer[T](var T) T {
	return gfn2_infer(var)
}

fn gfn4_infer[T](var T) T {
	return gfn3_infer(var)
}

fn test_generics_with_cascaded_multiple_nested_generics_fn() {
	println(gfn4(1234))
	assert gfn4(1234) == 1234
	println(gfn4_infer(1234))
	assert gfn4_infer(1234) == 1234
}
