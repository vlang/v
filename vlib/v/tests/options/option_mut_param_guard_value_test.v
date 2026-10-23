// An `if mut v := a` guard that is used as a value binds the caller's payload of a
// `mut a ?T` parameter, as the statement form does: changes through `v` reach the caller's
// option.
struct Point {
mut:
	x int
	y int
}

fn (mut p Point) bump() {
	p.x += 100
}

struct Wrap {
	n int
}

fn id(n int) int {
	return n
}

fn decl_push(mut a ?[]int) int {
	n := if mut v := a {
		v << 1
		v.len
	} else {
		-1
	}
	return n
}

fn assign_push(mut a ?[]int) int {
	mut n := 0
	n = if mut v := a {
		v << 2
		v.len
	} else {
		-1
	}
	return n
}

fn arg_push(mut a ?[]int) int {
	return id(if mut v := a {
		v << 3
		v.len
	} else {
		-1
	})
}

fn field_push(mut a ?[]int) Wrap {
	return Wrap{
		n: if mut v := a {
			v << 4
			v.len
		} else {
			-1
		}
	}
}

fn else_if_push(mut a ?[]int, mut b ?[]int) int {
	n := if mut v := a {
		v << 5
		v.len
	} else if mut w := b {
		w << 50
		w.len
	} else {
		-1
	}
	return n
}

fn match_push(mut a ?[]int, k int) int {
	n := match k {
		1 {
			if mut v := a {
				v << 6
				v.len
			} else {
				-1
			}
		}
		else {
			0
		}
	}
	return n
}

fn interpolated_push(mut a ?[]int) string {
	return 'len ${if mut v := a {
		v << 7
		v.len
	} else {
		-1
	}}'
}

fn decl_point(mut a ?Point) int {
	n := if mut v := a {
		v.x = 5
		v.bump()
		v.x
	} else {
		-1
	}
	return n
}

fn generic_decl_push[T](mut a ?[]T, x T) int {
	n := if mut v := a {
		v << x
		v.len
	} else {
		-1
	}
	return n
}

// generic_smartcast_push guards `a` while it is smartcast to its payload, which a generic
// body accepts.
fn generic_smartcast_push[T](mut a ?T) int {
	if a != none {
		return if mut v := a {
			v << 8
			v.len
		} else {
			-1
		}
	}
	return -2
}

fn test_array_payload() {
	mut a := ?[]int([0])
	assert decl_push(mut a) == 2
	assert a or { []int{} } == [0, 1]
	assert assign_push(mut a) == 3
	assert a or { []int{} } == [0, 1, 2]
	assert arg_push(mut a) == 4
	assert a or { []int{} } == [0, 1, 2, 3]
	assert field_push(mut a).n == 5
	assert a or { []int{} } == [0, 1, 2, 3, 4]
	mut b := ?[]int([9])
	assert else_if_push(mut a, mut b) == 6
	assert a or { []int{} } == [0, 1, 2, 3, 4, 5]
	assert b or { []int{} } == [9]
	assert match_push(mut a, 1) == 7
	assert a or { []int{} } == [0, 1, 2, 3, 4, 5, 6]
	assert interpolated_push(mut a) == 'len 8'
	assert a or { []int{} } == [0, 1, 2, 3, 4, 5, 6, 7]
	assert generic_decl_push(mut a, 9) == 9
	assert a or { []int{} } == [0, 1, 2, 3, 4, 5, 6, 7, 9]
	assert generic_smartcast_push(mut a) == 10
	assert a or { []int{} } == [0, 1, 2, 3, 4, 5, 6, 7, 9, 8]
}

fn test_else_if_branch() {
	mut a := ?[]int(none)
	mut b := ?[]int([9])
	assert else_if_push(mut a, mut b) == 2
	assert a == none
	assert b or { []int{} } == [9, 50]
}

fn test_struct_payload() {
	mut p := ?Point(Point{1, 2})
	assert decl_point(mut p) == 105
	assert p or { Point{} } == Point{105, 2}
}

fn test_none_is_left_alone() {
	mut a := ?[]int(none)
	assert decl_push(mut a) == -1
	assert assign_push(mut a) == -1
	assert arg_push(mut a) == -1
	assert field_push(mut a).n == -1
	assert match_push(mut a, 1) == -1
	assert interpolated_push(mut a) == 'len -1'
	assert generic_decl_push(mut a, 1) == -1
	assert generic_smartcast_push(mut a) == -2
	assert a == none
	mut p := ?Point(none)
	assert decl_point(mut p) == -1
	assert p == none
}
