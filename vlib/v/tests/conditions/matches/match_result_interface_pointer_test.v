interface Shape {
	area() int
}

struct Square {
	side int
}

fn (s &Square) area() int {
	return s.side * s.side
}

struct Circle {
	radius int
}

fn (c &Circle) area() int {
	return 3 * c.radius * c.radius
}

fn make_square(side int) !&Shape {
	if side < 0 {
		return error('square')
	}
	return &Square{side}
}

fn make_circle(radius int) !&Shape {
	if radius < 0 {
		return error('circle')
	}
	return &Circle{radius}
}

fn make_shape(kind string, size int) !&Shape {
	shape := match kind {
		'square' { make_square(size)! }
		else { make_circle(size)! }
	}
	return shape
}

fn test_match_expression_propagates_result_of_interface_pointer() {
	assert (make_shape('square', 2) or { panic(err) }).area() == 4
	assert (make_shape('circle', 1) or { panic(err) }).area() == 3
	for kind in ['square', 'circle'] {
		make_shape(kind, -1) or {
			assert err.msg() == kind
			continue
		}
		assert false
	}
}
