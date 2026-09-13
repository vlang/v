module main

struct COutputPoint {
	x i64
	y i64
}

fn offset_point(point COutputPoint, delta i64) COutputPoint {
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
