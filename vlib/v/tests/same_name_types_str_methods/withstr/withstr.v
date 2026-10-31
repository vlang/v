module withstr

// Mirrors `json2.Any`: `str` methods on the sum type, its array and its map.
pub type Any = []Any | map[string]Any | int | string

pub fn (a []Any) str() string {
	return 'withstr-array'
}

pub fn (m map[string]Any) str() string {
	return 'withstr-map'
}

pub fn (a Any) str() string {
	return 'withstr-any'
}

pub struct Point {
pub:
	x int
}

pub fn (p Point) str() string {
	return 'withstr-point'
}

pub enum Color {
	red
}

pub fn (c Color) str() string {
	return 'withstr-color'
}
