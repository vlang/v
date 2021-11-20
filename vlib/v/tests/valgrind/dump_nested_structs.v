struct NestedInner {
	x int
	y int
}

struct Inner {
	x           int
	y           int
	nestedinner NestedInner
}

struct Outer {
	x     int
	y     int
	inner Inner
}

fn main() {
	a := Outer{123, 456, Inner{789, 999, NestedInner{111, 222}}}
	dump(a)
}
