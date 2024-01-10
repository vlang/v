module main

import geometry { Point }

fn test_operator_overloading() {
	one := Point{
		x: 1
		y: 2
	}
	two := Point{
		x: 5
		y: 1
	}
	sum := one + two
	assert sum.x == 6
	assert sum.y == 3
}

fn test_str_method() {
	one := Point{
		x: 1
		y: 2
	}
	assert '${one}' == '1 2'
}
