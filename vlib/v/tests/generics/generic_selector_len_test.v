struct Foo[T] {
	len T
}

fn t[T](val T) string {
	a := val.len + val.len
	println(a)
	return a.str()
}

fn test_main() {
	assert t(Foo[string]{ len: 'hello' }) == 'hellohello'
	assert t(Foo[int]{ len: 123 }) == '246'
	assert t([1, 2, 3]) == '6'
	assert t([1.2, 2.2, 3.3]) == '6'
	assert t(['', '', '']) == '6'
}
