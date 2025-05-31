fn test_main() {
	a := [?int(1), 0, 2]
	mut t := a[0]
	assert t? == 1
	t = a[1]
	assert t? == 0
	t = a[2]
	assert t? == 2

	_ := [?int(none), none]
}
