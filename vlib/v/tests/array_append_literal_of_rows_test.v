struct Solution {
mut:
	rows [][]string
}

fn test_append_literal_of_array_values() {
	mut s := Solution{}
	first := ['a']
	second := ['b', 'c']
	s.rows << [first]
	s.rows << [first, second]
	assert s.rows == [['a'], ['a'], ['b', 'c']]
}

fn test_append_literal_of_fixed_array_values() {
	mut grid := [][2]int{}
	cell := [1, 2]!
	grid << [cell, cell]
	grid << [3, 4]!
	assert grid.len == 3
	assert grid[2] == [3, 4]!
}

fn test_push_literal_into_nested_array() {
	mut rows := [][]int{}
	rows << [1, 2]
	rows << [[3], [4, 5]]
	assert rows == [[1, 2], [3], [4, 5]]
}
