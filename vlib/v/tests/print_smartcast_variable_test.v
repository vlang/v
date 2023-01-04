struct Point {
	x int
	y int
}

struct Line {
	p1 Point
	p2 Point
}

// Sum type
type ObjSumType = Line | Point

fn test_print_smartcast_variable() {
	// Type checking and casts
	mut point := ObjSumType(Point{2, 5})

	if point is Point {
		println('Point')
	}

	if point !is Point {
		println('Not Point')
	}

	if mut point is Point {
		println(point)
		assert point.str().contains('x: 2')
		assert point.str().contains('y: 5')
		assert '${point}'.contains('x: 2')
		assert '${point}'.contains('y: 5')
	}
}
