import arrays
import maps

// Regression test for https://github.com/vlang/v/issues/30045:
// a call to a generic function used directly in a string interpolation, with or
// without an `or` block. Such an interpolation was not lowered at all, so a map or
// array method in the call's arguments reached C generation unlowered, and results
// that are not plain numbers or strings were printed wrong.

struct Point {
	x int
	y int
}

struct Box[T] {
	val T
}

fn (b Box[T]) first_of(a []T) !T {
	if a.len == 0 {
		return error('empty')
	}
	return a[0]
}

struct Picker {}

fn (p Picker) pick[T](a []T) T {
	return a[0]
}

type Num = f64 | int

type Label = string

interface Shape {
	area() f64
}

struct Square {
	side f64
}

fn (s Square) area() f64 {
	return s.side * s.side
}

struct Counter {
mut:
	n int
}

fn (mut c Counter) next() int {
	c.n++
	return c.n
}

struct Holder {
	m map[string]int
}

fn (h Holder) describe() string {
	return 'holder ${arrays.max(h.m.values()) or { -1 }} ${head(h.m.keys())}'
}

fn first[T](a []T) !T {
	if a.len == 0 {
		return error('empty')
	}
	return a[0]
}

fn maybe[T](a []T) ?T {
	if a.len == 0 {
		return none
	}
	return a[0]
}

fn head[T](a []T) T {
	return a[0]
}

fn same[T](a T) T {
	return a
}

fn wrap[T](a T) ?T {
	return a
}

fn ref_of[T](a &T) &T {
	return a
}

fn propagate_result(m map[string]int) !string {
	return 'r ${first(m.values())!}'
}

fn propagate_option(m map[string]int) ?string {
	return 'o ${maybe(m.keys())?}'
}

const const_int_part = 'c ${first([1, 2]) or { -1 }}'
const const_float_part = 'c ${first([1.5]) or { 0.0 }}'
const const_array_part = 'c ${first([[1], [2]]) or { []int{} }}'

fn test_issue_example() {
	m := {
		`a`: 1
		`b`: 2
	}
	assert 'B ${arrays.min(m.values()) or { -1 }}' == 'B 1'
	assert 'B ${arrays.max(m.keys()) or { `z` }}' == 'B b'
}

fn test_map_method_in_generic_call_with_or_block() {
	m := {
		'a': 1
		'b': 2
	}
	assert '${first(m.values()) or { -1 }}' == '1'
	assert '${first(m.keys()) or { 'none' }}' == 'a'
	assert '${maybe(m.values()) or { -1 }}' == '1'
	assert '${first[int](m.values()) or { -1 }}' == '1'
	assert '${(first(m.values()) or { -1 })}' == '1'
	assert '${first(m.values()) or { -1 }:04d}' == '0001'
	assert 'x ${first(m.keys()) or { 'none' }} y ${first(m.values()) or { -1 }} z' == 'x a y 1 z'
	assert '${arrays.sum(m.values()) or { 0 }}' == '3'
	assert propagate_result(m)! == 'r 1'
	assert propagate_option(m) or { 'none' } == 'o a'
}

fn test_map_method_in_generic_call_without_or_block() {
	m := {
		'a': 1
		'b': 2
	}
	assert '${head(m.values())}' == '1'
	assert '${head(m.keys())}' == 'a'
	assert '${same(m.values())}' == '[1, 2]'
	assert '${same(head(m.values()))}' == '1'
	assert 'a ${same(1)} b ${head(m.keys())} c ${same(2.5)} d' == 'a 1 b a c 2.5 d'
}

fn test_map_method_in_generic_method_call() {
	m := {
		'a': 1
		'b': 2
	}
	picker := Picker{}
	assert '${picker.pick(m.values())}' == '1'
	b := Box[int]{
		val: 5
	}
	assert '${b.first_of(m.values()) or { -1 }}' == '1'
	assert '${b.first_of([]int{}) or { m.values().len }}' == '2'
	assert Holder{
		m: m
	}.describe() == 'holder 2 a'
}

fn test_array_method_in_generic_call() {
	a := [3, 1, 2]
	assert '${first(a.map(it * 2)) or { -1 }}' == '6'
	assert '${first(a.filter(it < 3)) or { -1 }}' == '1'
	assert '${first(a.sorted()) or { -1 }}' == '1'
	assert '${first(a.sorted(a > b)) or { -1 }}' == '3'
	assert '${first(a.reverse()) or { -1 }}' == '2'
	assert '${first(a.clone()) or { -1 }}' == '3'
	assert '${head(a.map(it + 1))}' == '4'
	assert '${same(a.any(it > 2))}' == 'true'
}

fn test_other_lowered_arguments_of_generic_call() {
	m := {
		'a': 1
		'b': 2
	}
	a := [3, 1, 2]
	assert '${same(if a.len > 2 { m.values() } else { []int{} })}' == '[1, 2]'
	assert '${same(match a.len {
		3 { m.keys() }
		else { []string{} }
	})}' == "['a', 'b']"
	assert '${same('n ${head(m.values())}')}' == 'n 1'
	assert '${same(Point{
		x: head(m.values())
		y: 2
	})}' == 'Point{\n    x: 1\n    y: 2\n}'
	assert '${maps.to_array(m, fn (k string, v int) string {
		return k + v.str()
	})}' == "['a1', 'b2']"
}

fn test_or_block_body_of_generic_call() {
	m := {
		'a': 1
		'b': 2
	}
	assert '${first([]int{}) or { head(m.values()) }}' == '1'
	assert '${first([]int{}) or { m.values()[1] }}' == '2'
	assert '${first([]int{}) or {
		x := head(m.values())
		x + 10
	}}' == '11'
	assert '${first([][]int{}) or { same(m.values()) }}' == '[1, 2]'
	assert '${first([]Point{}) or { same(Point{4, 5}) }}' == 'Point{\n    x: 4\n    y: 5\n}'
}

fn test_parts_are_evaluated_in_source_order() {
	m := {
		'a': 1
		'b': 2
	}
	mut c := Counter{}
	s := '${c.next()} ${first(m.values()) or { c.next() }} ${same(c.next())} ${first([]int{}) or {
		c.next()
	}} ${c.next()}'
	assert s == '1 1 2 3 4'
	assert c.n == 4
	t := '${same(c.next())} ${c.next()} ${head(m.values().map(it + c.next()))} ${c.next()}'
	assert t == '5 6 8 9'
}

fn test_result_of_generic_call_is_printed_by_its_concrete_type() {
	p := Point{1, 2}
	assert '${same(`x`)}' == 'x'
	assert '${same([1, 2, 3])}' == '[1, 2, 3]'
	assert '${same(['a', 'b'])}' == "['a', 'b']"
	assert '${same([1, 2, 3]!)}' == '[1, 2, 3]'
	assert '${same(p)}' == 'Point{\n    x: 1\n    y: 2\n}'
	assert '${same(Box[int]{
		val: 3
	})}' == 'Box[int]{\n    val: 3\n}'
	assert '${same(Num(3))}' == 'Num(3)'
	assert '${same(Label('lbl'))}' == 'lbl'
	assert '${wrap(5)}' == 'Option(5)'
	assert '${maybe([]int{})}' == 'Option(none)'
	assert '${ref_of(&p)}' == '&Point{\n    x: 1\n    y: 2\n}'
	assert '${same(Shape(Square{2}))}' == 'Shape(Square{\n    side: 2.0\n})'
}

fn test_generic_call_with_or_block_in_const_interpolation() {
	assert const_int_part == 'c 1'
	assert const_float_part == 'c 1.5'
	assert const_array_part == 'c [1]'
}
