type Fn[T] = fn (x T, i int) T

fn func[T](x T, i int, f_ Fn[T]) T {
	return f_(x, i)
}

fn f[T](x T, i int) T {
	return x
}

fn test_generic_fn_variable() {
	ff := f[int]
	ret := ff(1, 11)
	println(ret)
	assert ret == 1

	x := func[f64](2.0, 3, f[f64])
	println(x)
	assert x == 2.0
}
