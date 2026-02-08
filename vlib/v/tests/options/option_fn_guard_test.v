type MyFn = fn ()

fn hello() {
	println(@FN)
}

fn get_ptr() ?&MyFn {
	return &hello
}

fn test_main() {
	if p := get_ptr() {
		assert voidptr(*p) == voidptr(hello)
		dump(p)
		w := *p
		w()
	}

	p2 := get_ptr() or { return }
	w2 := *p2
	w2()
	assert voidptr(*p2) == voidptr(hello)
}
