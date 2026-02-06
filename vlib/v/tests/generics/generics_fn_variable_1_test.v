type Fn[T] = fn (x T, i int) T

fn func[T](x T, i int, f_ Fn[T]) T {
	return f_(x, i)
}

fn f1[T](x T, i int) T {
	return x
}

fn test_generic_fn_variable() {
	ff1 := f1[int]
	ret1 := ff1(1, 11)
	println(ret1)
	assert ret1 == 1

	ff2 := f2[int]
	ret2 := ff2(1, 11)
	println(ret2)
	assert ret2 == 1

	x1 := func[f64](2.0, 3, f1[f64])
	println(x1)
	assert x1 == 2.0

	x2 := func[f64](2.0, 3, f2[f64])
	println(x2)
	assert x2 == 2.0
}

fn f2[T](x T, i int) T {
	return x
}
