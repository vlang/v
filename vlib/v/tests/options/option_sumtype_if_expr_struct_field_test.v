struct Circle27440 {
	radius int
}

struct Square27440 {
	size int
}

type Shape27440 = Circle27440 | Square27440

struct Box27440 {
	shape ?Shape27440
}

fn new_box27440(use_circle bool, n int) Box27440 {
	return Box27440{
		shape: if use_circle {
			Shape27440(Circle27440{
				radius: n
			})
		} else {
			Shape27440(Square27440{
				size: n
			})
		}
	}
}

fn test_option_sumtype_if_expr_struct_field() {
	circle_box := new_box27440(true, 4)
	circle_shape := circle_box.shape or {
		assert false
		return
	}

	assert circle_shape is Circle27440
	assert circle_shape.radius == 4

	square_box := new_box27440(false, 5)
	square_shape := square_box.shape or {
		assert false
		return
	}

	assert square_shape is Square27440
	assert square_shape.size == 5
}
