// The value of a generic fn or of a generic method keeps the `mut` of its
// parameters, as the value of a plain fn does: it is called with `mut` args.
struct Point {
mut:
	x int
}

struct Setter {
	x int
}

fn clear[T](mut a ?T) {
	a = none
}

fn reset[T](mut a T) {
	a = T{}
}

fn bump[T](mut a T, by int) {
	a.x += by
}

fn (s Setter) set[T](mut a T) {
	a.x = s.x
}

fn (s Setter) clear[T](mut a ?T) {
	a = none
}

fn test_generic_fn_values() {
	mut o := ?Point(Point{
		x: 1
	})
	f := clear[Point]
	f(mut o)
	assert o == none
	mut p := Point{
		x: 3
	}
	g := reset[Point]
	g(mut p)
	assert p.x == 0
	h := bump[Point]
	h(mut p, 5)
	assert p.x == 5
}

fn test_generic_method_values() {
	s := Setter{
		x: 7
	}
	mut p := Point{
		x: 3
	}
	set := s.set[Point]
	set(mut p)
	assert p.x == 7
	mut o := ?Point(p)
	clear_o := s.clear[Point]
	clear_o(mut o)
	assert o == none
}
