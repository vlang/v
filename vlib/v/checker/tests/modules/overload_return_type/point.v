module point

pub struct Point {
mut:
	x int
	y int
}

pub fn (a Point) + (b Point) int {
	return a.x + b.x
}
