module point

pub struct Point {
pub mut:
	x int
	y int
}

pub fn (a Point) + (b Point) int {
	return a.x + b.x
}
