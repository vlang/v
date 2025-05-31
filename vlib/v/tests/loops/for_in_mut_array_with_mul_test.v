fn test_for_in_mut_array_with_mul() {
	mut ints_a := [1, 2]
	for mut i in ints_a {
		i = i * i
	}
	print('${ints_a}')
	assert ints_a == [1, 4]
}
