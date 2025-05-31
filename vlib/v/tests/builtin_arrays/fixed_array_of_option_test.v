fn test_fixed_array_of_option() {
	mut a1 := [3]?int{init: ?int(1)}
	a1[0] = none
	a1[1] = 2
	println(a1)
	assert '${a1}' == '[Option(none), Option(2), Option(1)]'

	mut a11 := [3]?int{init: 1}
	a11[0] = none
	a11[1] = 2
	println(a11)
	assert '${a11}' == '[Option(none), Option(2), Option(1)]'

	mut a2 := [3]?int{}
	a2[0] = 1
	println(a2)
	assert '${a2}' == '[Option(1), Option(none), Option(none)]'

	a3 := [3]?int{init: ?int(index * 2)}
	println(a3)
	assert '${a3}' == '[Option(0), Option(2), Option(4)]'

	a33 := [3]?int{init: index * 2}
	println(a33)
	assert '${a33}' == '[Option(0), Option(2), Option(4)]'
}
