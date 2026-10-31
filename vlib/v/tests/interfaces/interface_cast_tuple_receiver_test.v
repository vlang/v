interface Position {
	id     string
	values []int
	x      int
	y      int
}

struct Point {
	id     string
	values []int
	x      int
	y      int
}

fn (p &Position) coordinates() (int, int) { return p.x, p.y }

fn test_interface_cast_tuple_receiver() {
	p := &Point{ id: 'point', values: [1, 2], x: 3, y: 4 }
	x, y := Position(p).coordinates()
	assert x == 3 && y == 4
	nil_point := unsafe { &Point(nil) }
	zero_x, zero_y := Position(nil_point).coordinates()
	assert zero_x == 0 && zero_y == 0
}

interface Holder {
mut:
	point &Point
}

struct Container {
mut:
	point &Point
}

fn (p &Point) location() (int, int) { return p.x, p.y }

fn holder_location(mut holder Holder, enabled bool) (int, int) {
	return if enabled { holder.point.location() } else { 0, 0 }
}

fn test_interface_pointer_field_tuple_receiver() {
	mut holder := Holder(&Container{ point: &Point{ x: 7, y: 8 } })
	x, y := holder_location(mut holder, true)
	assert x == 7 && y == 8
	zero_x, zero_y := holder_location(mut holder, false)
	assert zero_x == 0 && zero_y == 0
}
