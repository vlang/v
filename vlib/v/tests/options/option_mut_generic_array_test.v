fn test_main() {
	mut arr := [1, 2]
	mut bbb := unwrap(mut arr)
	bbb << 3
	dump(bbb)
	assert bbb == [1, 2, 3]
	assert arr == [1, 2]
}

fn unwrap[T](mut t ?&T) T {
	return t or { panic('unexpected `none`') }
}
