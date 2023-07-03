fn test_main() {
	mut a := []?&int{}
	a << ?&int(none)
	a << none
	dump(a[0])
	dump(a[1])
	println(a)

	assert a[0] == none
	assert a[1] == none
}
