fn test_main() {
	a := []?int{len: 3, init: none}
	mut t := a[0]
	assert t == none
	t = a[1]
	assert t == none
	t = a[2]
	assert t == none
	assert a.len == 3
}
