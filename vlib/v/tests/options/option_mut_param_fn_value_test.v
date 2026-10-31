// A `mut a ?T` parameter is passed as `&?&T`. Function literals,
// closures, fn types, fn-type aliases and fn-typed fields with such a parameter use the
// same signature as a named fn, in their bodies and at their call sites.
struct Point {
mut:
	x int
	y int
}

type PointFn = fn (mut a ?Point)

type IntsFn = fn (mut a ?[]int)

struct Ops {
	f   fn (mut a ?Point) = unsafe { nil }
	g   PointFn           = unsafe { nil }
	h   IntsFn            = unsafe { nil }
	opt ?fn (mut a ?Point)
}

struct Service {
	step int
}

fn (s Service) bump(mut a ?Point) {
	if mut p := a {
		p.x += s.step
	}
}

fn named(mut a ?Point) {
	if mut p := a {
		p.x++
	}
}

fn apply_all(fns []PointFn, mut a ?Point) {
	for f in fns {
		f(mut a)
	}
}

fn call_through_ref(o &Ops, mut a ?Point) {
	o.f(mut a)
	o.g(mut a)
}

fn point_of(a ?Point) Point {
	return a or { Point{-1, -1} }
}

fn test_fn_literals() {
	add_ten := fn (mut a ?Point) {
		if mut p := a {
			p.x += 10
		}
	}
	clear := fn (mut a ?Point) {
		a = none
	}
	set := fn (mut a ?Point) {
		a = Point{7, 7}
	}
	read_x := fn (mut a ?Point) int {
		p := a or { return -1 }
		return p.x
	}
	mut pt := ?Point(Point{1, 2})
	add_ten(mut pt)
	assert point_of(pt) == Point{11, 2}
	assert read_x(mut pt) == 11
	clear(mut pt)
	assert pt == none
	assert read_x(mut pt) == -1
	set(mut pt)
	assert point_of(pt) == Point{7, 7}
	mut plain := Point{5, 5}
	add_ten(mut plain)
	assert plain == Point{15, 5}
}

fn test_closures() {
	step := 100
	add_step := fn [step] (mut a ?Point) {
		if mut p := a {
			p.y += step
		}
	}
	mut pt := ?Point(Point{1, 2})
	add_step(mut pt)
	assert point_of(pt) == Point{1, 102}
	mut plain := Point{5, 5}
	add_step(mut plain)
	assert plain == Point{5, 105}
}

fn test_fn_type_aliases_and_values() {
	mut pt := ?Point(Point{0, 0})
	alias := PointFn(named)
	alias(mut pt)
	assert point_of(pt).x == 1
	value := named
	value(mut pt)
	assert point_of(pt).x == 2
	add_ten := fn (mut a ?Point) {
		if mut p := a {
			p.x += 10
		}
	}
	apply_all([PointFn(named), add_ten, alias], mut pt)
	assert point_of(pt).x == 14
	fns := [alias, add_ten]
	fns[1](mut pt)
	assert point_of(pt).x == 24
	by_name := {
		'named': PointFn(named)
	}
	by_name['named'](mut pt)
	assert point_of(pt).x == 25
	s := Service{
		step: 1000
	}
	bound := s.bump
	bound(mut pt)
	assert point_of(pt).x == 1025
}

fn test_fn_typed_fields() {
	add_ten := fn (mut a ?Point) {
		if mut p := a {
			p.x += 10
		}
	}
	ops := Ops{
		f:   add_ten
		g:   named
		h:   fn (mut a ?[]int) {
			if mut v := a {
				v << 9
			}
		}
		opt: named
	}
	mut pt := ?Point(Point{0, 0})
	ops.f(mut pt)
	ops.g(mut pt)
	assert point_of(pt).x == 11
	call_through_ref(&ops, mut pt)
	assert point_of(pt).x == 22
	if f := ops.opt {
		f(mut pt)
	}
	assert point_of(pt).x == 23
	mut plain := Point{0, 0}
	ops.f(mut plain)
	ops.g(mut plain)
	assert plain.x == 11
	mut arr := ?[]int([1])
	ops.h(mut arr)
	assert arr or { []int{} } == [1, 9]
	mut none_pt := ?Point(none)
	ops.f(mut none_pt)
	assert none_pt == none
}
