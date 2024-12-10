fn init_a(n_rows int) []map[int]int {
	mut tally := []map[int]int{}
	for _ in 0 .. n_rows {
		tally << map[int]int{}
	}
	return tally
}

fn init_b(n_rows int) []map[int]int {
	mut tally := []map[int]int{len: n_rows, init: map[int]int{}}
	return tally
}

pub fn tallys_in_array(indices []int, values [][]int, init fn (int) []map[int]int) []map[int]int {
	mut tally := init(indices.len)
	for row in 0 .. values.len {
		for i, index in indices {
			tally[i][values[row][index]]++
		}
	}
	return tally
}

fn test_array_of_map_with_default() {
	indices := [0, 1]
	values := [[1, 201], [1, 3], [1, 201], [1, 3]]

	out1 := tallys_in_array(indices, values, init_a)
	println(out1)
	out2 := tallys_in_array(indices, values, init_b)
	println(out2)

	mut maps := []map[int]int{}
	maps << map[int]int{}
	maps << map[int]int{}
	maps[0][1] = 4
	maps[1][3] = 2
	maps[1][201] = 2

	assert out1 == maps
	assert out2 == maps
}
