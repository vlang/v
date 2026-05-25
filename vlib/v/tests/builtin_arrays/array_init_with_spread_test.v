const base_array = [
	1
	2
]

const base_strings = ['a', 'b']

struct Point {
	x int
	y int
}

const base_points = [Point{1, 2}, Point{3, 4}]

fn test_array_init_with_spread() {
	complete := [
		...base_array,
		3,
		4,
	]
	assert base_array == [1, 2]
	assert complete == [1, 2, 3, 4]
	assert complete.len == 4

	second := [...base_array, 5, 6, 7]
	assert second == [1, 2, 5, 6, 7]
}

fn test_array_init_with_only_spread() {
	mut copy := [...base_array]
	assert copy == [1, 2]
	copy << 99
	// spread produces an independent copy
	assert base_array == [1, 2]
	assert copy == [1, 2, 99]
}

fn test_array_spread_strings() {
	merged := [...base_strings, 'c', 'd']
	assert merged == ['a', 'b', 'c', 'd']
}

fn test_array_spread_structs() {
	pts := [...base_points, Point{5, 6}]
	assert pts.len == 3
	assert pts[0] == Point{1, 2}
	assert pts[1] == Point{3, 4}
	assert pts[2] == Point{5, 6}
}

fn test_array_spread_local_var() {
	v_arr := [7, 8, 9]
	x := [...v_arr, 100]
	assert v_arr == [7, 8, 9]
	assert x == [7, 8, 9, 100]
}

fn test_array_spread_chained() {
	first := [...base_array, 3]
	second := [...first, 4]
	assert second == [1, 2, 3, 4]
}
