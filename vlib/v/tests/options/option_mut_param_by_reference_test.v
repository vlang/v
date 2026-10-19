// A `mut a ?T` parameter is passed as `&?&T`, whatever its payload, as in V 0.5.2 (where it
// is `?&T`): the option the callee gets refers to the caller's value. Changes made through
// `if mut v := a`, or through `a` smartcast to its payload, reach the caller, whether the
// caller passed a plain value or an option variable. `a = none` and `a = value` replace the
// option itself: an option variable gets the new option back after the call, while a plain
// value keeps its value, as in V 0.5.2.
type Shape = int | string

type MyInt = int

type Ints = []int

enum Color {
	red
	green
	blue
}

interface Named {
	name() string
mut:
	rename(s string)
}

struct Cat {
mut:
	n string
}

fn (c Cat) name() string {
	return c.n
}

fn (mut c Cat) rename(s string) {
	c.n = s
}

struct Point {
mut:
	x int
}

struct Holder {
mut:
	opt ?[]int
}

type IntsFn = fn (mut a ?[]int)

struct Ops {
	f  fn (mut a ?[]int) = unsafe { nil }
	g  IntsFn            = unsafe { nil }
	of ?fn (mut a ?[]int)
}

struct Svc {
	step int
}

fn (s Svc) push(mut a ?[]int) {
	if mut v := a {
		v << s.step
	}
}

interface Pusher {
	push(mut a ?[]int)
}

fn swap(mut a ?[]int, mut b ?[]int) {
	a, b = b, a
}

fn arr_push(mut a ?[]int) {
	if mut v := a {
		v << 4
	}
}

fn map_insert(mut a ?map[string]int) {
	if mut v := a {
		v['b'] = 2
	}
}

fn fixed_write(mut a ?[3]int) {
	if mut v := a {
		v[1] = 20
	}
}

fn sum_replace(mut a ?Shape) {
	if mut v := a {
		v = Shape('replaced')
	}
}

fn enum_assign(mut a ?Color) {
	if mut v := a {
		v = .blue
	}
}

fn int_assign(mut a ?MyInt) {
	if mut v := a {
		v = 42
	}
}

fn alias_push(mut a ?Ints) {
	if mut v := a {
		v << 9
	}
}

fn iface_rename(mut a ?Named) {
	if mut v := a {
		v.rename('tom')
	}
}

fn iface_replace(mut a ?Named) {
	if mut v := a {
		v = Named(Cat{'felix'})
	}
}

fn smartcast_edit(mut a ?[]int) int {
	if a != none {
		a << 4
		a[0] = 10
		return a.len
	}
	return -1
}

fn smartcast_map(mut a ?map[string]int) {
	if a != none {
		a['x'] = 1
	}
}

fn forward(mut a ?[]int) {
	arr_push(mut a)
}

fn clear_arr(mut a ?[]int) bool {
	a = none
	return a == none
}

fn set_arr(mut a ?[]int) int {
	a = [9, 9, 9]
	return take(a)
}

fn forward_set(mut a ?[]int) int {
	return set_arr(mut a)
}

fn set_then_push(mut a ?[]int) {
	a = [5]
	if mut v := a {
		v << 6
	}
}

fn set_map(mut a ?map[string]int) {
	a = {
		'z': 26
	}
}

fn set_fixed(mut a ?[3]int) {
	a = [7, 8, 9]!
}

fn set_sum(mut a ?Shape) {
	a = 'set'
}

fn set_enum(mut a ?Color) {
	a = .green
}

fn set_int(mut a ?MyInt) {
	a = MyInt(7)
}

fn clear_generic[T](mut a ?T) {
	a = none
}

fn take(x ?[]int) int {
	v := x or { return -1 }
	return v.len
}

fn ret(mut a ?[]int) ?[]int {
	return a
}

fn prop(mut a ?[]int) ?int {
	v := a?
	return v.len
}

fn reads(mut a ?[]int) string {
	f := fn [a] () int {
		v := a or { return -2 }
		return v.len
	}
	r := ret(mut a) or { [] }
	p := prop(mut a) or { -1 }
	d := (a or { [0] }).len
	return '${a == none} ${take(a)} ${r.len} ${p} ${d} ${f()} ${a}'
}

fn gen_push[T](mut a ?[]T, x T) {
	if mut v := a {
		v << x
	}
}

fn gen_any[T](mut a ?T) {
	if mut v := a {
		$if T is $array {
			v << 4
		} $else $if T is Point {
			v.x += 100
		}
	}
}

fn bump(mut a ?Point) {
	if mut p := a {
		p.x += 10
	}
}

fn gen_forward[T](mut a ?T) {
	bump(mut a)
}

fn test_plain_values_reach_the_caller() {
	mut arr := [1, 2, 3]
	arr_push(mut arr)
	assert arr == [1, 2, 3, 4]
	mut m := {
		'a': 1
	}
	map_insert(mut m)
	assert m == {
		'a': 1
		'b': 2
	}
	mut f := [1, 2, 3]!
	fixed_write(mut f)
	assert f == [1, 20, 3]!
	mut s := Shape(1)
	sum_replace(mut s)
	assert s == Shape('replaced')
	mut c := Color.red
	enum_assign(mut c)
	assert c == .blue
	mut i := MyInt(7)
	int_assign(mut i)
	assert i == 42
	mut al := Ints([1])
	alias_push(mut al)
	assert al == Ints([1, 9])
	mut n := Named(Cat{'kitty'})
	iface_rename(mut n)
	assert n.name() == 'tom'
	iface_replace(mut n)
	assert n.name() == 'felix'
}

fn test_option_variables_reach_the_caller() {
	mut arr := ?[]int([1, 2, 3])
	arr_push(mut arr)
	assert arr or { []int{} } == [1, 2, 3, 4]
	mut m := ?map[string]int({
		'a': 1
	})
	map_insert(mut m)
	assert m or { map[string]int{} } == {
		'a': 1
		'b': 2
	}
	mut f := ?[3]int([1, 2, 3]!)
	fixed_write(mut f)
	assert f or { [0, 0, 0]! } == [1, 20, 3]!
	mut s := ?Shape(Shape(1))
	sum_replace(mut s)
	assert s or { Shape(0) } == Shape('replaced')
	mut c := ?Color(Color.red)
	enum_assign(mut c)
	assert c or { Color.red } == .blue
	mut i := ?MyInt(MyInt(7))
	int_assign(mut i)
	assert i or { MyInt(0) } == 42
	mut al := ?Ints(Ints([1]))
	alias_push(mut al)
	assert al or { Ints([]int{}) } == Ints([1, 9])
	mut n := ?Named(Named(Cat{'kitty'}))
	iface_rename(mut n)
	assert (n or { Named(Cat{}) }).name() == 'tom'
	iface_replace(mut n)
	assert (n or { Named(Cat{}) }).name() == 'felix'
	mut none_arr := ?[]int(none)
	arr_push(mut none_arr)
	assert none_arr == none
}

fn test_option_fields_and_elements() {
	mut h := Holder{
		opt: [1]
	}
	arr_push(mut h.opt)
	assert h.opt or { []int{} } == [1, 4]
	assert clear_arr(mut h.opt)
	assert h.opt == none
	mut opts := [?[]int([2])]
	arr_push(mut opts[0])
	assert opts[0] or { []int{} } == [2, 4]
}

fn test_smartcast_writes_reach_the_caller() {
	mut arr := [1, 2]
	assert smartcast_edit(mut arr) == 3
	assert arr == [10, 2, 4]
	mut opt := ?[]int([1, 2])
	assert smartcast_edit(mut opt) == 3
	assert opt or { []int{} } == [10, 2, 4]
	mut m := map[string]int{}
	smartcast_map(mut m)
	assert m == {
		'x': 1
	}
	mut om := ?map[string]int(map[string]int{})
	smartcast_map(mut om)
	assert om or { map[string]int{} } == {
		'x': 1
	}
}

fn test_assignments_replace_only_the_option_of_a_plain_value() {
	mut arr := [1, 2]
	assert clear_arr(mut arr)
	assert arr == [1, 2]
	assert set_arr(mut arr) == 3
	assert arr == [1, 2]
	assert forward_set(mut arr) == 3
	assert arr == [1, 2]
	set_then_push(mut arr)
	assert arr == [1, 2]
	mut m := {
		'a': 1
	}
	set_map(mut m)
	assert m == {
		'a': 1
	}
	mut f := [1, 2, 3]!
	set_fixed(mut f)
	assert f == [1, 2, 3]!
	mut s := Shape(1)
	set_sum(mut s)
	assert s == Shape(1)
	mut c := Color.red
	set_enum(mut c)
	assert c == .red
	mut i := MyInt(1)
	set_int(mut i)
	assert i == 1
	clear_generic(mut arr)
	assert arr == [1, 2]
}

fn test_assignments_reach_an_option_variable() {
	mut arr := ?[]int([1, 2])
	assert set_arr(mut arr) == 3
	assert arr or { []int{} } == [9, 9, 9]
	assert clear_arr(mut arr)
	assert arr == none
	assert forward_set(mut arr) == 3
	assert arr or { []int{} } == [9, 9, 9]
	set_then_push(mut arr)
	assert arr or { []int{} } == [5, 6]
	clear_generic(mut arr)
	assert arr == none
	mut m := ?map[string]int(none)
	set_map(mut m)
	assert m or { map[string]int{} } == {
		'z': 26
	}
	mut f := ?[3]int(none)
	set_fixed(mut f)
	assert f or { [0, 0, 0]! } == [7, 8, 9]!
	mut s := ?Shape(Shape(1))
	set_sum(mut s)
	assert s or { Shape(0) } == Shape('set')
	mut c := ?Color(none)
	set_enum(mut c)
	assert c or { Color.red } == .green
	mut i := ?MyInt(none)
	set_int(mut i)
	assert i or { MyInt(0) } == 7
}

fn test_reads_of_the_parameter() {
	mut arr := [1, 2]
	assert reads(mut arr) == 'false 2 2 2 2 2 Option([1, 2])'
	mut opt := ?[]int([1, 2, 3])
	assert reads(mut opt) == 'false 3 3 3 3 3 Option([1, 2, 3])'
	mut none_opt := ?[]int(none)
	assert reads(mut none_opt) == 'true -1 0 -1 1 -2 Option(none)'
}

fn test_forwarded_parameters() {
	mut arr := [1]
	forward(mut arr)
	assert arr == [1, 4]
	mut opt := ?[]int([1])
	forward(mut opt)
	assert opt or { []int{} } == [1, 4]
}

fn test_fn_values_fields_and_methods() {
	lit := fn (mut a ?[]int) {
		if mut v := a {
			v << 5
		}
	}
	lit_set := fn (mut a ?[]int) {
		a = [8]
	}
	ops := Ops{
		f:  arr_push
		g:  IntsFn(arr_push)
		of: arr_push
	}
	svc := Svc{6}
	method_value := svc.push
	fn_value := arr_push
	mut arr := [1]
	lit(mut arr)
	fn_value(mut arr)
	ops.f(mut arr)
	ops.g(mut arr)
	if f := ops.of {
		f(mut arr)
	}
	method_value(mut arr)
	svc.push(mut arr)
	assert arr == [1, 5, 4, 4, 4, 4, 6, 6]
	lit_set(mut arr)
	assert arr == [1, 5, 4, 4, 4, 4, 6, 6]
	mut opt := ?[]int([1])
	lit(mut opt)
	fn_value(mut opt)
	ops.f(mut opt)
	ops.g(mut opt)
	if f := ops.of {
		f(mut opt)
	}
	method_value(mut opt)
	svc.push(mut opt)
	assert opt or { []int{} } == [1, 5, 4, 4, 4, 4, 6, 6]
	lit_set(mut opt)
	assert opt or { []int{} } == [8]
	pusher := Pusher(svc)
	pusher.push(mut arr)
	assert arr == [1, 5, 4, 4, 4, 4, 6, 6, 6]
	pusher.push(mut opt)
	assert opt or { []int{} } == [8, 6]
}

fn test_multi_assign() {
	mut x := ?[]int([1])
	mut y := ?[]int([2])
	swap(mut x, mut y)
	assert x or { []int{} } == [2]
	assert y or { []int{} } == [1]
	mut p := [3]
	swap(mut p, mut x)
	assert p == [3]
	assert x or { []int{} } == [3]
}

fn test_generic_parameters() {
	mut strs := ['x']
	gen_push(mut strs, 'y')
	assert strs == ['x', 'y']
	mut opt_strs := ?[]string(['x'])
	gen_push(mut opt_strs, 'z')
	assert opt_strs or { []string{} } == ['x', 'z']
	mut arr := [1]
	gen_any(mut arr)
	assert arr == [1, 4]
	mut opt := ?[]int([1])
	gen_any(mut opt)
	assert opt or { []int{} } == [1, 4]
	mut pt := Point{1}
	gen_any(mut pt)
	assert pt.x == 101
	gen_forward(mut pt)
	assert pt.x == 111
	gen_forward[Point](mut pt)
	assert pt.x == 121
	mut opt_pt := ?Point(Point{1})
	gen_any(mut opt_pt)
	gen_forward(mut opt_pt)
	assert opt_pt or { Point{} } == Point{111}
}
