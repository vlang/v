module main

fn f_a() {
}

fn test_main() {
	f := f_a
	f()
	assert voidptr(f) == voidptr(f_a)
	ref := &f
	deref := *ref
	assert voidptr(deref) == voidptr(f_a)
	deref()
}
