type I64 = i64

fn test[T](a ?T) ?T {
	w := ?T(a)
	return w
}

fn test_main() {
	a := ?i64(123)
	b := test[I64](a)
	println(b)
	assert b != none
	assert b? == 123
	c := test[I64](none)
	println(c)
	assert c == none
}
