struct AA {
mut:
	a shared []&int
}

fn test_shared_array_ptr() {
	mut a := AA{}
	b := 3
	lock a.a {
		a.a << &b
	}
}
