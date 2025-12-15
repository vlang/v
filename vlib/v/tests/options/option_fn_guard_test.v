type MyFn = fn ()

fn hello() {
	println(@FN)
}

fn get_ptr() ?&MyFn {
	return &hello
}

fn test_main() {
	if p := get_ptr() {
		assert voidptr(p) == voidptr(hello)
		dump(p)
		p()
	}

	p2 := get_ptr() or { return }
	p2()
	assert voidptr(p2) == voidptr(hello)
}
