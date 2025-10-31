fn test_main() {
	la := 'lalala'
	la2 := &la
	la3 := &la2
	a(la3)
}

fn a[T](t &&T) {
	println(t)
	dump(t)
	assert **t == 'lalala'
}
