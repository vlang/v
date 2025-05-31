fn test_complex_map_op1() {
	mut test_map := map[string]Point_map{}

	test_map['test1'].points['point3'] << Point{10, 20}
	test_map['test2'].points['point4'] << Point{50, 60}

	println(test_map)

	assert test_map.len == 2
	assert Point{10, 20} in test_map['test1'].points['point3']
	assert Point{50, 60} in test_map['test2'].points['point4']
	assert Point{10, 20} !in test_map['test2'].points['point4']
	assert Point{1, 2} !in test_map['test1'].points['point3']
}

fn test_complex_map_op2() {
	mut test_map := map[string]map[string][]Point{}

	test_map['test1']['point3'] << Point{10, 20}
	test_map['test2']['point4'] << Point{50, 60}

	println(test_map)

	assert test_map.len == 2
	assert Point{10, 20} in test_map['test1']['point3']
	assert Point{50, 60} in test_map['test2']['point4']
	assert Point{10, 20} !in test_map['test2']['point4']
	assert Point{1, 2} !in test_map['test1']['point3']
}

struct Point {
mut:
	x int
	y int
}

struct Point_map {
mut:
	points map[string][]Point
}
