type MyFn = fn ()

fn hello() {}

fn get_ptr() ?&MyFn {
	return &hello
}

fn test_main() {
	if p := get_ptr() {
		dump(p)
		p()
	}

	p2 := get_ptr() or { return }
	p2()
}
