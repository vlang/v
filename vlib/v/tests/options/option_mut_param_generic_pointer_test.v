// A generic `mut a ?T` parameter whose `T` is a pointer is `mut a ?&Point` in its
// instance, as in V 0.5.2: calls of generic fns, of methods of generic structs and
// of generic interface methods accept a `?&Point` for it.
struct Point {
mut:
	x int
}

struct Holder[T] {
mut:
	v ?T
}

fn (mut h Holder[T]) put(mut a ?T) {
	a = h.v
}

fn (h Holder[T]) name(mut a ?T) string {
	return typeof(a).name
}

interface Putter[T] {
	put(mut a ?T)
}

struct PointPutter {}

fn (p PointPutter) put(mut a ?&Point) {
	a = &Point{11}
}

fn bump[T](mut a ?T) {
	if mut v := a {
		v.x = 42
	}
}

fn set[T](mut a ?T, b T) {
	a = b
}

fn clear[T](mut a ?T) {
	a = none
}

fn name[T](mut a ?T) string {
	return typeof(a).name
}

fn x_of(a ?&Point) int {
	p := a or { return -1 }
	return p.x
}

fn test_generic_fn_with_a_pointer_type_argument() {
	mut p := &Point{1}
	mut pr := ?&Point(p)
	bump[&Point](mut pr)
	assert p.x == 42
	p.x = 1
	bump(mut pr)
	assert p.x == 42
	assert name[&Point](mut pr) == '?&&Point'
	assert name(mut pr) == '?&&Point'
	set[&Point](mut pr, &Point{3})
	assert x_of(pr) == 3
	set(mut pr, &Point{4})
	assert x_of(pr) == 4
	clear(mut pr)
	assert pr == none
	mut ar := ?&[]int(&[1, 2])
	assert name(mut ar) == '?&&[]int'
}

fn test_generic_struct_methods_and_interfaces() {
	mut pr := ?&Point(&Point{1})
	mut h := Holder[&Point]{
		v: &Point{6}
	}
	h.put(mut pr)
	assert x_of(pr) == 6
	assert h.name(mut pr) == '?&&Point'
	putter := Putter[&Point](PointPutter{})
	putter.put(mut pr)
	assert x_of(pr) == 11
}
