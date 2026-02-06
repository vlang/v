fn test_filtered_fixed_arrays() {
	a := [1, 2, 3]!
	assert a.filter(it > 2).len == 1
	assert typeof(a.filter(it > 2)).name == '[]int'

	sfa := ['abc', 'def', 'xyz']!
	sfaf := sfa.filter(it[0] == `d`)
	assert sfaf == ['def']
	assert typeof(sfaf).name == '[]string'

	ufa := [u8(99), 77, 61, 120]!
	ufaf := ufa.filter(it > 80)
	assert ufaf == [u8(99), 120]
	assert typeof(ufaf).name == '[]u8'

	fafa := [[1, 2, 3]!, [4, 5, 6]!, [6, 7, 8]!]!
	fafaf := fafa.filter(it[2] > 5)
	assert fafaf == [[4, 5, 6]!, [6, 7, 8]!]
	assert typeof(fafaf).name == '[][3]int'
}
