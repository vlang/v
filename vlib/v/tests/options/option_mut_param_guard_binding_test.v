// `if mut v := a` over a `mut a ?T` parameter binds the caller's payload, as in V 0.5.2:
// pushes, index writes, method calls and assignments through `v` reach the caller's
// option, whatever the payload type.
struct Point {
mut:
	x int
	y int
}

type Ints = []int

type Shape = int | string

fn push_four(mut a ?[]int) {
	if mut v := a {
		v << 4
	}
}

fn reassign(mut a ?[]int) {
	if mut v := a {
		v = [7, 7]
	}
}

fn edit_in_place(mut a ?[]int) {
	if mut v := a {
		v.sort(a > b)
		v.delete(0)
		v[0]++
		v << [10, 11]
	}
}

fn append_one(mut x []int) {
	x << 1
}

fn forward_and_sum(mut a ?[]int) int {
	if mut v := a {
		append_one(mut v)
		mut sum := 0
		for x in v {
			sum += x
		}
		return sum
	} else {
		return -1
	}
}

fn push_and_len(mut a ?[]int) int {
	return if mut v := a {
		v << 0
		v.len
	} else {
		-1
	}
}

fn cross_push(mut a ?[]int, mut b ?[]int) {
	if mut x := a {
		if mut y := b {
			x << y.len
			y << x.len
		}
	}
}

fn push_and_address(mut a ?[]int) &[]int {
	if mut v := a {
		mut p := &v
		p << 6
		return p
	}
	return &[]int{}
}

fn map_edit(mut a ?map[string]int) {
	if mut v := a {
		v.delete('a')
		v['z'] = 26
	}
}

fn fixed_edit(mut a ?[3]int) {
	if mut v := a {
		v[1] = 77
	}
}

fn fixed_double(mut a ?[3]int) {
	if mut v := a {
		for mut e in v {
			e *= 2
		}
		v = [v[0], v[1], 100]!
	}
}

fn point_edit(mut a ?Point) {
	if mut v := a {
		v.x = 99
	}
}

fn shape_set(mut a ?Shape) {
	if mut v := a {
		v = Shape('s')
	}
}

fn alias_push(mut a ?Ints) {
	if mut v := a {
		v << 9
	}
}

fn string_edit(mut a ?[]string) {
	if mut v := a {
		v[0] += '!'
	}
}

fn generic_push[T](mut a ?[]T, x T) {
	if mut v := a {
		v << x
	}
}

fn test_array_payload() {
	mut a := ?[]int([1, 2, 3])
	push_four(mut a)
	assert a or { []int{} } == [1, 2, 3, 4]
	reassign(mut a)
	assert a or { []int{} } == [7, 7]
	edit_in_place(mut a)
	assert a or { []int{} } == [8, 10, 11]
	assert forward_and_sum(mut a) == 30
	assert a or { []int{} } == [8, 10, 11, 1]
	assert push_and_len(mut a) == 5
	assert a or { []int{} } == [8, 10, 11, 1, 0]
	mut b := ?[]int([0])
	cross_push(mut a, mut b)
	assert a or { []int{} } == [8, 10, 11, 1, 0, 1]
	assert b or { []int{} } == [0, 6]
	p := push_and_address(mut b)
	assert *p == [0, 6, 6]
	assert b or { []int{} } == [0, 6, 6]
}

fn test_none_payload_is_untouched() {
	mut a := ?[]int(none)
	push_four(mut a)
	assert a == none
	assert forward_and_sum(mut a) == -1
	assert push_and_len(mut a) == -1
	assert a == none
}

fn test_map_and_fixed_array_payloads() {
	mut m := ?map[string]int({
		'a': 1
		'b': 2
	})
	map_edit(mut m)
	assert m or { map[string]int{} } == {
		'b': 2
		'z': 26
	}
	mut f := ?[3]int([1, 2, 3]!)
	fixed_edit(mut f)
	assert f or { [0, 0, 0]! } == [1, 77, 3]!
	fixed_double(mut f)
	assert f or { [0, 0, 0]! } == [2, 154, 100]!
}

fn test_other_payloads() {
	mut pt := ?Point(Point{1, 2})
	point_edit(mut pt)
	assert (pt or { Point{} }) == Point{99, 2}
	mut sh := ?Shape(Shape(1))
	shape_set(mut sh)
	assert sh or { Shape(0) } == Shape('s')
	mut al := ?Ints(Ints([1]))
	alias_push(mut al)
	assert al or { Ints([]int{}) } == Ints([1, 9])
	mut strs := ?[]string(['x'])
	string_edit(mut strs)
	generic_push(mut strs, 'y')
	assert strs or { []string{} } == ['x!', 'y']
	mut ints := ?[]int([1])
	generic_push(mut ints, 2)
	assert ints or { []int{} } == [1, 2]
}

fn test_fn_literal_param() {
	push := fn (mut a ?[]int) {
		if mut v := a {
			v << 5
		}
	}
	mut a := ?[]int([1])
	push(mut a)
	assert a or { []int{} } == [1, 5]
}
