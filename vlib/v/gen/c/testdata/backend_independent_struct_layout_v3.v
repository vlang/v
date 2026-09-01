module main

struct COutputPoint {
	x int
	y int
}

fn offset_point(point COutputPoint, delta int) COutputPoint {
	return COutputPoint{
		x: point.x + delta
		y: point.y
	}
}

fn main() {
	point := offset_point(COutputPoint{
		x: 1
		y: 2
	}, 3)
	println(point.x)
}
