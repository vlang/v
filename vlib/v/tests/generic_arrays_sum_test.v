import arrays

struct Point {
pub mut:
	x int
	y int
}

fn (p1 Point) + (p2 Point) Point {
	return Point{
		x: p1.x + p2.x
		y: p1.y + p2.y
	}
}

fn test_generic_arrays_sum() {
	ret := arrays.sum<Point>([Point{ x: 1, y: 1 }, Point{
		x: 2
		y: 2
	}]) or { Point{} }
	println(ret)
	assert ret == Point{3, 3}
}
