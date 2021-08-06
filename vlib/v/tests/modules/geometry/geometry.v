module geometry

const (
	module_name = 'geometry'
)

pub enum Shape {
	circle
	rectangle
	triangle
}

pub type ShapeMap = map[Shape]string

// used by vlib/v/tests/map_enum_keys_test.v
pub enum Form3D {
	sphere
	cylinder
	cone
	cube
	invalid
}

pub struct Point {
pub mut:
	x int
	y int
}

pub struct Line {
pub mut:
	ps []Point
}

pub fn (a Point) + (b Point) Point {
	return Point{
		x: a.x + b.x
		y: a.y + b.y
	}
}

pub fn (a Point) str() string {
	return '${a.x} ${a.y}'
}

pub fn point_str(a Point) string {
	return a.str()
}

pub type PointCond = fn (p Point) bool
