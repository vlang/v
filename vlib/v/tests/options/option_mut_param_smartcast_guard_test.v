// A generic body can guard a `mut a ?T` parameter while `a` is smartcast to its payload
// (inside `if a != none`; V 0.5.2 and a non-generic body reject that guard). As without
// the smartcast, `if mut v := a` binds the caller's payload: changes through `v` reach the
// caller's option, whatever the payload type.
struct Point {
mut:
	x int
	y int
}

fn (mut p Point) bump() {
	p.x += 100
}

fn set_x[T](mut a ?T, x int) {
	if a != none {
		if mut v := a {
			v.x = x
		}
	}
}

fn bump_in_else[T](mut a ?T) {
	if a == none {
		return
	} else {
		if mut v := a {
			v.bump()
		}
	}
}

fn set_in_match[T](mut a ?T, n int) {
	if a != none {
		match n {
			1 {
				if mut v := a {
					v.x = 11
				}
			}
			else {
				if mut v := a {
					v.y = 12
				}
			}
		}
	}
}

fn replace_point[T](mut a ?T) {
	if a != none {
		if mut v := a {
			v = T{
				x: 20
				y: 21
			}
		}
	}
}

fn push_one[T](mut a ?T) {
	if a != none {
		if mut v := a {
			v << 1
		}
	}
}

fn push_in_match[T](mut a ?T, n int) {
	if a != none {
		match n {
			1 {
				if mut v := a {
					v << 10
				}
			}
			else {
				if mut v := a {
					v[0] = 20
				}
			}
		}
	}
}

fn increment[T](mut a ?T) {
	if a != none {
		if mut v := a {
			v++
		}
	}
}

fn test_struct_payload() {
	mut p := ?Point(Point{1, 2})
	set_x(mut p, 5)
	assert p or { Point{} } == Point{5, 2}
	bump_in_else(mut p)
	assert p or { Point{} } == Point{105, 2}
	set_in_match(mut p, 1)
	assert p or { Point{} } == Point{11, 2}
	set_in_match(mut p, 2)
	assert p or { Point{} } == Point{11, 12}
	replace_point(mut p)
	assert p or { Point{} } == Point{20, 21}
}

fn test_array_payload() {
	mut a := ?[]int([0])
	push_one(mut a)
	assert a or { []int{} } == [0, 1]
	push_in_match(mut a, 1)
	assert a or { []int{} } == [0, 1, 10]
	push_in_match(mut a, 2)
	assert a or { []int{} } == [20, 1, 10]
}

fn test_int_payload() {
	mut i := ?int(41)
	increment(mut i)
	assert i or { 0 } == 42
}

fn test_none_is_left_alone() {
	mut p := ?Point(none)
	set_x(mut p, 5)
	bump_in_else(mut p)
	assert p == none
	mut a := ?[]int(none)
	push_one(mut a)
	push_in_match(mut a, 1)
	assert a == none
}
