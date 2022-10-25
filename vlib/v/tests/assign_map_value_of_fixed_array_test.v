fn test_assign_map_value_of_fixed_array() {
	mut m := map[string][2]f64{}

	m['A'] = [1.1, 2.2]!
	m['B'] = [0.1, 0.2]!

	mut arr := m['A']
	println(arr)
	assert '$arr' == '[1.1, 2.2]'

	arr = m['B']
	println(arr)
	assert '$arr' == '[0.1, 0.2]'

	arr = m['C']
	println(arr)
	assert '$arr' == '[0.0, 0.0]'
}
