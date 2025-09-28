const k = 2

fn test_complex_dim_fixed_array() {
	result := [[0, 0]!, [0, 0]!]!

	assert result == [2][k]int{}
	assert result == [2][k + 1 - 1]int{}
	assert result == [k][(k + 1) * 2 - 4]int{}
	assert result == [k][int(k)]int{}
}
