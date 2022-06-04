struct Abc {
	prev &Abc
}

const a = [Abc{voidptr(0)}, Abc{unsafe { &a[0] }}, Abc{unsafe { &a[1] }}]!

fn test_fixed_array() {
	dump(a)
	dump(voidptr(&a[0]))
	dump(voidptr(&a[1]))
	dump(voidptr(&a[2]))
	dump(voidptr(a[0].prev))
	dump(voidptr(a[1].prev))
	dump(voidptr(a[2].prev))
	assert voidptr(&a[0]) == voidptr(a[1].prev)
	assert voidptr(&a[1]) == voidptr(a[2].prev)
}
