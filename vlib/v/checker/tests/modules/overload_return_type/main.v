module main

import point { Point }

fn main() {
	one := Point{
		x: 1
		y: 2
	}
	mut two := Point{
		x: 5
		y: 1
	}
	two = one + two
}
