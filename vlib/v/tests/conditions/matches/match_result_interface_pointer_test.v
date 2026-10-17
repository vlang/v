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

fn make_shape_if(square bool, size int) !&Shape {
	shape := if square { make_square(size)! } else { make_circle(size)! }
	return shape
}

fn test_if_expression_propagates_result_of_interface_pointer() {
	assert (make_shape_if(true, 2) or { panic(err) }).area() == 4
	assert (make_shape_if(false, 1) or { panic(err) }).area() == 3
	for square in [true, false] {
		expected := if square { 'square' } else { 'circle' }
		make_shape_if(square, -1) or {
			assert err.msg() == expected
			continue
		}
		assert false
	}
}

fn shape_pointer(shape &Shape) &Shape {
	return shape
}

fn make_concrete_shape(kind string, size int) &Shape {
	return shape_pointer(match kind {
		'square' { &Square{size} }
		else { &Circle{size} }
	})
}

fn test_match_expression_boxes_concrete_branches_as_interface_pointers() {
	assert make_concrete_shape('square', 2).area() == 4
	assert make_concrete_shape('circle', 2).area() == 12
}

fn make_concrete_square(side int) !&Square {
	if side < 0 {
		return error('square')
	}
	return &Square{side}
}

fn make_concrete_circle(radius int) !&Circle {
	if radius < 0 {
		return error('circle')
	}
	return &Circle{radius}
}

fn make_concrete_shape_result(kind string, size int) !&Shape {
	return shape_pointer(match kind {
		'square' { make_concrete_square(size)! }
		else { make_concrete_circle(size)! }
	})
}

fn test_match_expression_boxes_propagated_concrete_results_as_interface_pointers() {
	assert (make_concrete_shape_result('square', 2) or { panic(err) }).area() == 4
	assert (make_concrete_shape_result('circle', 2) or { panic(err) }).area() == 12
	for kind in ['square', 'circle'] {
		make_concrete_shape_result(kind, -1) or {
			assert err.msg() == kind
			continue
		}
		assert false
	}
}
