// A `mut a ?T` parameter whose payload is a struct is passed as `&?&T`, like any other
// payload, and its body reads it as the value `?T` it declares: reads copy the caller's
// payload out, so it can be passed on as `?T`, returned, and defaulted with `or`. Changes
// through `if mut v := a`, or through `a` smartcast to its payload, reach the caller.
// `a = value` stores a heap copy of the value, never a reference to the callee's stack.
struct Point {
mut:
	x int
	y int
}

struct Holder {
mut:
	opt ?Point
}

struct RefHolder {
	ref ?&Point
}

@[heap]
struct Node {
mut:
	val  int
	kids []int
}

type PointFn = fn (mut a ?Point) int

type SetFn = fn (mut a ?Point)

fn (mut p Point) bump() {
	p.x += 100
}

fn (p Point) sum() int {
	return p.x + p.y
}

fn modify(mut p Point) {
	p.y += 1000
}

fn take_ref(p ?&Point) int {
	v := p or { return -1 }
	return v.x
}

fn take(p ?Point) int {
	v := p or { return -1 }
	return v.x
}

fn take_generic[T](p ?T) ?T {
	return p
}

// Fills the stack where a callee's locals were, so that a reference to one of them is
// seen to be dangling.
@[noinline]
fn clobber() int {
	mut arr := [64]int{}
	mut s := 0
	for i in 0 .. arr.len {
		arr[i] = 1000 + i
		s += arr[i]
	}
	return s
}

fn pass_on(mut a ?Point) int {
	return take(a)
}

fn pass_on_generic(mut a ?Point) int {
	b := take_generic(a) or { return -1 }
	return b.x
}

fn ret(mut a ?Point) ?Point {
	return a
}

fn copy_to_local(mut a ?Point) ?Point {
	b := a
	a = none
	return b
}

fn or_default(mut a ?Point) Point {
	return a or { Point{-1, -1} }
}

fn or_default_x(mut a ?Point) int {
	v := a or { Point{-2, -2} }
	return v.x
}

fn copy_out(mut a ?Point) Point {
	mut v := a or { return Point{} }
	v.x = 99
	return v
}

fn guard_edit(mut a ?Point) {
	if mut v := a {
		v.x += 10
	}
}

fn smartcast_edit(mut a ?Point) {
	if a != none {
		a.y += 5
	}
}

fn smartcast_calls(mut a ?Point) int {
	if a != none {
		a.bump()
		modify(mut a)
		return a.sum()
	}
	return -1
}

fn guard_calls(mut a ?Point) {
	if mut v := a {
		v.bump()
		modify(mut v)
	}
}

fn to_ref_holder(mut a ?Point) RefHolder {
	return RefHolder{
		ref: a
	}
}

fn pass_ref(mut a ?Point) int {
	return take_ref(a)
}

fn ret_ref(mut a ?Point) ?&Point {
	return a
}

fn node_push(mut a ?Node, x int) int {
	if mut n := a {
		n.kids << x
	}
	copy := a or { return -1 }
	return copy.kids.len
}

fn node_set(mut a ?Node) {
	local := Node{
		val:  5
		kids: [1, 2]
	}
	a = local
}

fn set_local(mut a ?Point) {
	local := Point{7, 8}
	a = local
}

fn set_mut_local(mut a ?Point) {
	mut local := Point{1, 1}
	local.x = 11
	a = local
	local.x = 12
}

fn set_opt_local(mut a ?Point) {
	local := ?Point(Point{9, 10})
	a = local
}

fn set_from(mut a ?Point, b ?Point) {
	a = b
}

fn set_and_read(mut a ?Point) string {
	set_local(mut a)
	clobber()
	return '${a}'
}

fn set_opt_and_read(mut a ?Point) int {
	set_opt_local(mut a)
	clobber()
	return take(a)
}

fn str_of(mut a ?Point) string {
	return '${a}'
}

fn guard_type_name(mut a ?Point) string {
	if v := a {
		return typeof(v).name
	}
	return 'none'
}

fn g_pass_on[T](mut a ?T) ?T {
	return take_generic(a)
}

fn g_or[T](mut a ?T, d T) T {
	return a or { d }
}

fn g_set_local[T](mut a ?T, value T) {
	local := value
	a = local
}

fn g_set_from[T](mut a ?T, b ?T) {
	a = b
}

fn g_set_and_read[T](mut a ?T, value T) ?T {
	g_set_local(mut a, value)
	clobber()
	return a
}

fn g_guard_type[T](mut a ?T) string {
	if v := a {
		return typeof(v).name
	}
	return 'none'
}

fn g_mut_guard_type[T](mut a ?T) string {
	if mut v := a {
		return typeof(v).name
	}
	return 'none'
}

fn g_value_guard_type[T](a ?T) string {
	if v := a {
		return typeof(v).name
	}
	return 'none'
}

fn test_passing_on_and_returning() {
	mut p := Point{5, 6}
	assert pass_on(mut p) == 5
	assert pass_on_generic(mut p) == 5
	assert ret(mut p) or { Point{} } == Point{5, 6}
	mut o := ?Point(Point{6, 7})
	assert pass_on(mut o) == 6
	assert pass_on_generic(mut o) == 6
	assert ret(mut o) or { Point{} } == Point{6, 7}
	assert copy_to_local(mut o) or { Point{} } == Point{6, 7}
	assert o == none
	assert pass_on(mut o) == -1
	assert pass_on_generic(mut o) == -1
	assert ret(mut o) == none
	mut h := Holder{
		opt: Point{8, 9}
	}
	assert pass_on(mut h.opt) == 8
	mut arr := [?Point(Point{3, 4})]
	assert pass_on(mut arr[0]) == 3
}

fn test_or_defaults_and_copies() {
	mut p := Point{5, 6}
	assert or_default(mut p) == Point{5, 6}
	assert or_default_x(mut p) == 5
	assert copy_out(mut p) == Point{99, 6}
	assert p == Point{5, 6}
	mut n := ?Point(none)
	assert or_default(mut n) == Point{-1, -1}
	assert or_default_x(mut n) == -2
	assert copy_out(mut n) == Point{}
	mut o := ?Point(Point{1, 2})
	assert copy_out(mut o) == Point{99, 2}
	assert o or { Point{} } == Point{1, 2}
}

fn test_writes_reach_the_caller() {
	mut p := Point{1, 2}
	guard_edit(mut p)
	smartcast_edit(mut p)
	assert p == Point{11, 7}
	mut o := ?Point(Point{1, 2})
	guard_edit(mut o)
	smartcast_edit(mut o)
	assert o or { Point{} } == Point{11, 7}
	mut h := Holder{
		opt: Point{1, 2}
	}
	guard_edit(mut h.opt)
	smartcast_edit(mut h.opt)
	assert h.opt or { Point{} } == Point{11, 7}
}

fn test_calls_reach_the_caller() {
	mut p := Point{1, 2}
	assert smartcast_calls(mut p) == 1103
	assert p == Point{101, 1002}
	guard_calls(mut p)
	assert p == Point{201, 2002}
	mut o := ?Point(Point{1, 2})
	assert smartcast_calls(mut o) == 1103
	guard_calls(mut o)
	assert o or { Point{} } == Point{201, 2002}
	mut n := ?Point(none)
	assert smartcast_calls(mut n) == -1
}

fn test_passing_on_the_reference() {
	mut p := Point{1, 2}
	h := to_ref_holder(mut p)
	assert take_ref(h.ref) == 1
	assert pass_ref(mut p) == 1
	if r := ret_ref(mut p) {
		assert r.x == 1
	} else {
		assert false
	}
	mut n := ?Point(none)
	assert to_ref_holder(mut n).ref == none
	assert pass_ref(mut n) == -1
	assert ret_ref(mut n) == none
}

fn test_heap_struct_payload() {
	mut nd := Node{
		val: 1
	}
	assert node_push(mut nd, 4) == 1
	assert nd.kids == [4]
	mut o := ?Node(none)
	assert node_push(mut o, 4) == -1
	node_set(mut o)
	clobber()
	assert node_push(mut o, 3) == 3
	nd2 := o or { Node{} }
	assert nd2.val == 5
	assert nd2.kids == [1, 2, 3]
}

fn test_assigning_a_local_copies_it() {
	mut o := ?Point(none)
	set_local(mut o)
	clobber()
	assert o or { Point{} } == Point{7, 8}
	set_mut_local(mut o)
	clobber()
	assert o or { Point{} } == Point{11, 1}
	set_opt_local(mut o)
	clobber()
	assert o or { Point{} } == Point{9, 10}
	mut n := ?Point(none)
	assert set_and_read(mut n) == 'Option(Point{\n    x: 7\n    y: 8\n})'
	assert n or { Point{} } == Point{7, 8}
	n = none
	assert set_opt_and_read(mut n) == 9
	assert n or { Point{} } == Point{9, 10}
	mut h := Holder{}
	set_local(mut h.opt)
	clobber()
	assert h.opt or { Point{} } == Point{7, 8}
	// A plain value keeps its value, as in V 0.5.2.
	mut p := Point{1, 2}
	set_local(mut p)
	assert p == Point{1, 2}
	assert set_and_read(mut p) == 'Option(Point{\n    x: 7\n    y: 8\n})'
	assert p == Point{1, 2}
}

fn test_assigning_an_option() {
	mut o := ?Point(none)
	set_from(mut o, Point{3, 4})
	assert o or { Point{} } == Point{3, 4}
	other := ?Point(Point{5, 6})
	set_from(mut o, other)
	assert o or { Point{} } == Point{5, 6}
	set_from(mut o, none)
	assert o == none
	mut p := Point{1, 2}
	set_from(mut p, other)
	assert p == Point{1, 2}
}

fn test_reads_like_a_value_option() {
	mut p := Point{1, 2}
	assert str_of(mut p) == 'Option(Point{\n    x: 1\n    y: 2\n})'
	assert guard_type_name(mut p) == 'Point'
	mut n := ?Point(none)
	assert str_of(mut n) == 'Option(none)'
	assert guard_type_name(mut n) == 'none'
}

fn test_generic_parameters() {
	mut p := Point{5, 6}
	assert g_pass_on(mut p) or { Point{} } == Point{5, 6}
	assert g_or(mut p, Point{}) == Point{5, 6}
	assert g_guard_type(mut p) == 'Point'
	assert g_mut_guard_type(mut p) == 'Point'
	assert g_value_guard_type(?Point(p)) == 'Point'
	mut o := ?Point(none)
	assert g_pass_on(mut o) == none
	assert g_or(mut o, Point{-1, -1}) == Point{-1, -1}
	assert g_guard_type(mut o) == 'none'
	g_set_local(mut o, Point{3, 4})
	clobber()
	assert o or { Point{} } == Point{3, 4}
	g_set_from(mut o, ?Point(Point{7, 8}))
	assert o or { Point{} } == Point{7, 8}
	g_set_from(mut o, ?Point(none))
	assert o == none
	assert g_set_and_read(mut o, Point{9, 9}) or { Point{} } == Point{9, 9}
	assert o or { Point{} } == Point{9, 9}
	mut arr := [1, 2]
	assert g_guard_type(mut arr) == '[]int'
	assert g_mut_guard_type(mut arr) == '[]int'
	assert g_value_guard_type(?[]int(arr)) == '[]int'
	assert g_or(mut arr, []int{}) == [1, 2]
}

fn test_fn_values() {
	pass := fn (mut a ?Point) int {
		return take(a)
	}
	or_lit := fn (mut a ?Point) int {
		v := a or { Point{-3, -3} }
		return v.x
	}
	set_lit := fn (mut a ?Point) {
		local := Point{3, 4}
		a = local
	}
	from_lit := fn (mut a ?Point, b ?Point) {
		a = b
	}
	named_pass := PointFn(pass_on)
	named_set := SetFn(set_local)
	mut p := Point{5, 6}
	assert pass(mut p) == 5
	assert or_lit(mut p) == 5
	assert named_pass(mut p) == 5
	mut o := ?Point(none)
	assert pass(mut o) == -1
	assert or_lit(mut o) == -3
	set_lit(mut o)
	clobber()
	assert o or { Point{} } == Point{3, 4}
	assert named_pass(mut o) == 3
	from_lit(mut o, ?Point(Point{8, 8}))
	assert o or { Point{} } == Point{8, 8}
	from_lit(mut o, none)
	assert o == none
	named_set(mut o)
	clobber()
	assert o or { Point{} } == Point{7, 8}
	fns := [named_pass, PointFn(or_lit)]
	assert fns[1](mut o) == 7
}
