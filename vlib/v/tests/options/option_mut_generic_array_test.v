fn test_main() {
	mut arr := []int{}
	mut bbb := unwrap(mut arr)
	bbb << 1
	dump(bbb)
	assert bbb == [1]
}

fn unwrap[T](mut t ?&T) T {
	return t or { panic('unexpected `none`') }
}
