type Sumtype = string | int

fn generic_fn[T](x Sumtype) bool {
	y := [x].map(it as T)
	mut arr := []T{}
	arr << x as T
	dump(arr)
	return arr.contains(y[0])
}

fn test_main() {
	assert generic_fn[string]('hello')
	assert generic_fn[int](123)
}
