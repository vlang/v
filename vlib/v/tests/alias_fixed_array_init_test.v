type Tres = [3]int

fn test_alias_fixed_array_init() {
	fixed_three := [1, 2, 3]!
	x := Tres(fixed_three)
	println(x)
	assert '${x}' == 'Tres([1, 2, 3])'

	y := Tres([2, 3, 4]!)
	println(y)
	assert '${y}' == 'Tres([2, 3, 4])'
}
