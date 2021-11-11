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
	ret1 := arrays.sum<Point>([Point{ x: 1, y: 1 }, Point{
		x: 2
		y: 2
	}]) or { Point{} }
	println(ret1)
	assert ret1 == Point{3, 3}

	ret2 := arrays.sum<int>([1, 2, 3, 4]) or { 0 }
	println(ret2)
	assert ret2 == 10
}
