fn test_main() {
	mut a := []?int{}
	a << none
	assert a == [?int(none)]
}
