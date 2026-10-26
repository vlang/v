// A `mut` value that is not an option can be passed to a `mut a ?T` parameter. Like
// V 0.5.2, the callee gets it in a temporary option: assigning `none` or a new value to
// the parameter only replaces the temporary. The temporary refers to the caller's value,
// so the callee's changes through it reach the caller (see
// option_mut_param_by_reference_test.v).
struct Point {
mut:
	x int
	y int
	z int
}

type PointAlias = Point

struct Holder {
mut:
	p Point
}

type Shape = Point | int

enum Color {
	red
	green
}

struct Service {
	step int
}

fn bump(mut a ?Point) {
	if mut p := a {
		p.x++
	}
}

fn clear_point(mut a ?Point) bool {
	a = none
	return a == none
}

fn replace_point(mut a ?Point) int {
	a = Point{
		x: 100
	}
	return point_x(mut a)
}

fn point_x(mut a ?Point) int {
	p := a or { return -1 }
	return p.x
}

fn forward_bump(mut p Point) {
	bump(mut p)
}

fn alias_x(mut a ?PointAlias) int {
	if mut p := a {
		p.x += 10
		return p.x
	}
	return -1
}

fn bump_ref(mut a ?&Point) {
	if mut p := a {
		p.x++
	}
}

fn forward_bump_ref(mut p Point) {
	bump_ref(mut p)
}

fn (s Service) bump(mut a ?Point) {
	if mut p := a {
		p.x += s.step
	}
}

fn sum_ints(mut a ?[]int) int {
	v := a or { return -1 }
	mut sum := 0
	for x in v {
		sum += x
	}
	return sum
}

fn clear_ints(mut a ?[]int) bool {
	a = none
	return sum_ints(mut a) == -1
}

fn replace_ints(mut a ?[]int) int {
	a = [9]
	return sum_ints(mut a)
}

fn forward_sum(mut v []int) int {
	return sum_ints(mut v)
}

fn map_value(mut a ?map[string]int) int {
	if m := a {
		return m['k']
	}
	return -1
}

fn fixed_sum(mut a ?[2]int) int {
	f := a or { return -1 }
	return f[0] + f[1]
}

fn color_name(mut a ?Color) string {
	c := a or { return 'none' }
	return c.str()
}

fn shape_int(mut a ?Shape) int {
	s := a or { return -1 }
	return match s {
		Point { s.x }
		int { s }
	}
}

fn clear_generic[T](mut a ?T) bool {
	a = none
	return a == none
}

fn test_struct_payload_refers_to_the_caller_value() {
	mut pt := Point{1, 2, 3}
	bump(mut pt)
	assert pt == Point{2, 2, 3}
	assert point_x(mut pt) == 2
	assert clear_point(mut pt)
	assert pt == Point{2, 2, 3}
	assert replace_point(mut pt) == 100
	assert pt == Point{2, 2, 3}
	forward_bump(mut pt)
	assert pt.x == 3
	Service{
		step: 5
	}.bump(mut pt)
	assert pt.x == 8
}

fn test_struct_payload_from_fields_and_elements() {
	mut h := Holder{
		p: Point{
			x: 5
		}
	}
	bump(mut h.p)
	assert h.p.x == 6
	assert clear_point(mut h.p)
	assert h.p.x == 6
	mut pts := [Point{
		x: 7
	}]
	bump(mut pts[0])
	assert pts[0].x == 8
	assert point_x(mut pts[0]) == 8
}

fn test_struct_alias_and_reference_payloads() {
	mut pa := PointAlias(Point{
		x: 1
	})
	assert alias_x(mut pa) == 11
	assert pa.x == 11
	mut pt := Point{
		x: 1
	}
	bump_ref(mut pt)
	assert pt.x == 2
	forward_bump_ref(mut pt)
	assert pt.x == 3
}

fn test_value_payloads_are_not_replaced() {
	mut arr := [1, 2, 3]
	assert sum_ints(mut arr) == 6
	assert clear_ints(mut arr)
	assert arr == [1, 2, 3]
	assert replace_ints(mut arr) == 9
	assert arr == [1, 2, 3]
	assert forward_sum(mut arr) == 6
	mut m := {
		'k': 4
	}
	assert map_value(mut m) == 4
	mut f := [3, 4]!
	assert fixed_sum(mut f) == 7
	mut c := Color.green
	assert color_name(mut c) == 'green'
	mut s := Shape(12)
	assert shape_int(mut s) == 12
	mut sp := Shape(Point{
		x: 3
	})
	assert shape_int(mut sp) == 3
}

fn test_generic_payloads() {
	mut pt := Point{
		x: 4
	}
	assert clear_generic(mut pt)
	assert pt.x == 4
	mut arr := [5]
	assert clear_generic(mut arr)
	assert arr == [5]
}
