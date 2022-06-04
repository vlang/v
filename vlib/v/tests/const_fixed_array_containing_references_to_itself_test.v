struct Abc {
	prev &Abc
}

const a = [Abc{voidptr(0)}, Abc{unsafe { &a[0] }}, Abc{unsafe { &a[1] }}]!

fn test_fixed_array() {
	eprintln(a)
	eprintln(voidptr(&a[0]))
	eprintln(voidptr(&a[1]))
	eprintln(voidptr(&a[2]))
	eprintln(voidptr(a[0].prev))
	eprintln(voidptr(a[1].prev))
	eprintln(voidptr(a[2].prev))
	assert voidptr(&a[0]) == voidptr(a[1].prev)
	assert voidptr(&a[1]) == voidptr(a[2].prev)
}
