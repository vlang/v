fn test_fixed_array_of_option() {
	mut a := [3]?int{init: ?int(1)}
	a[0] = none
	a[1] = 2
	println(a)
	assert '${a}' == '[Option(none), Option(2), Option(1)]'
}
