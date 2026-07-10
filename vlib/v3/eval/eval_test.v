module eval

import os
import v3.parser

const vexe = @VEXE
const v3_dir = os.dir(os.dir(@FILE))
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_eval_function_call_and_for_range() {
	mut e := create()
	e.run_text('
fn sum(n int) int {
	mut acc := 0
	for i in 0 .. n {
		acc += i
	}
	return acc
}

fn main() {
	println(sum(5))
}
') or {
		panic(err)
	}
	assert e.stdout() == '10\n'
}

fn test_eval_registers_top_level_comptime_block_declarations() {
	mut e := create()
	e.run_text('
	$if windows {
	fn gated_message() string {
		return "ok"
	}
} $else $if macos {
	fn gated_message() string {
		return "ok"
	}
} $else {
	fn gated_message() string {
		return "ok"
	}
}

fn main() {
	println(gated_message())
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'ok\n'
}

fn test_eval_executes_implicit_main_top_level_statements() {
	mut e := create()
	e.run_text('
	mut x := 1
	x += 2
	println(x)
	') or { panic(err) }
	assert e.stdout() == '3\n'
}

fn test_eval_disabled_if_call_skips_arguments() {
	mut e := create()
	e.run_text('
	__global hit int

@[if trace ?]
fn trace(x int) {}

fn side_effect() int {
	hit = 99
	return 1
}

fn main() {
	trace(side_effect())
	println(int_str(hit))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n'
}

fn test_eval_labeled_for_in_flow_targets_named_loop() {
	mut e := create()
	e.run_text('
fn main() {
	mut out := ""
	outer: for x in 0 .. 3 {
		for y in 0 .. 3 {
			if x == 1 && y == 0 {
				continue outer
			}
			if x == 2 && y == 1 {
				break outer
			}
			out += "\${x}:\${y};"
		}
	}
	println(out)
}
') or {
		panic(err)
	}
	assert e.stdout() == '0:0;0:1;0:2;2:0;\n'
}

fn test_eval_labeled_c_style_for_flow_targets_named_loop() {
	mut e := create()
	e.run_text('
fn main() {
	mut out := ""
	outer: for x := 0; x < 3; x++ {
		for y := 0; y < 3; y++ {
			if x == 1 && y == 0 {
				continue outer
			}
			if x == 2 && y == 1 {
				break outer
			}
			out += "\${x}:\${y};"
		}
	}
	println(out)
}
') or {
		panic(err)
	}
	assert e.stdout() == '0:0;0:1;0:2;2:0;\n'
}

fn test_eval_labeled_c_style_multi_init_flow_targets_named_loop() {
	mut e := create()
	e.run_text('
fn main() {
	mut out := ""
	outer: for x, stop := 0, 3; x < stop; x++ {
		for y := 0; y < 3; y++ {
			if x == 1 && y == 0 {
				continue outer
			}
			if x == 2 && y == 1 {
				break outer
			}
			out += "\${x}:\${y};"
		}
	}
	assert out == "0:0;0:1;0:2;2:0;"

	mut gx := 0
	mut hits := 0
	mut guarded := ""
	guarded_outer: for gx, hits = hits, hits + 1; gx < 3; gx++ {
		for gy := 0; gy < 3; gy++ {
			if gx == 1 && gy == 0 {
				continue guarded_outer
			}
			if gx == 2 && gy == 1 {
				break guarded_outer
			}
			guarded += "\${gx}:\${gy};"
		}
	}
	assert guarded == "0:0;0:1;0:2;2:0;"
	assert hits == 1
	println(out)
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0:0;0:1;0:2;2:0;\n'
}

fn test_eval_labeled_user_block_with_multi_init_shape_is_not_loop_label() {
	mut e := create()
	_ := e.run_text('
fn main() {
	mut a := 0
	mut b := 0
	outer: {
		a, b = 0, 3
		for ; a < b; a++ {
			continue outer
		}
	}
}
	') or {
		assert err.msg().contains('unexpected `continue_` escaped')
		return
	}
	assert false
}

fn test_eval_if_expr_value() {
	mut e := create()
	e.run_text('
fn main() {
	x := if 3 > 2 {
		41
	} else {
		0
	}
	println(x + 1)
}
') or {
		panic(err)
	}
	assert e.stdout() == '42\n'
}

fn test_eval_char_literal_escapes() {
	mut e := create()
	e.run_text(r"
fn main() {
	println(int_str(`\0`))
	println(int_str(`\\`))
	println(int_str(`\'`))
}
	") or {
		panic(err)
	}
	assert e.stdout() == '0\n92\n39\n'
}

fn test_eval_match_statement_propagates_return() {
	mut e := create()
	e.run_text('
fn choose(x int) int {
	match x {
		0 {
			return 1
		}
		else {}
	}
	return 2
}

fn main() {
	println(int_str(choose(0)))
	println(int_str(choose(1)))
}
') or {
		panic(err)
	}
	assert e.stdout() == '1\n2\n'
}

fn test_eval_match_condition_propagates_return() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() int {
	x := 1
	match x {
		maybe() or {
			return 7
		} {}
		else {}
	}
	return 0
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_match_branch_shadows_outer_locals() {
	mut e := create()
	e.run_text('
fn stmt_shadow(n int) int {
	x := 1
	match n {
		0 {
			x := 2
			if x != 2 {
				return 99
			}
		}
		else {}
	}
	return x
}

fn value_shadow(n int) int {
	x := 1
	y := match n {
		0 {
			x := 2
			x
		}
		else {
			3
		}
	}
	return x * 10 + y
}

fn main() {
	println(int_str(stmt_shadow(0)))
	println(int_str(stmt_shadow(1)))
	println(int_str(value_shadow(0)))
	println(int_str(value_shadow(1)))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n1\n12\n13\n'
}

fn test_eval_c_style_for_scopes_initializer_and_body() {
	mut e := create()
	e.run_text('
fn main() {
	i := 10
	for i := 0; i < 1; i++ {}
	println(int_str(i))

	x := 1
	for j := 0; j < 2; j++ {
		x := j + 2
		_ = x
	}
	println(int_str(x))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '10\n1\n'
}

fn test_eval_array_append_and_string_interpolation() {
	mut e := create()
	e.run_text('
fn main() {
	mut arr := []int{}
	arr << 7
	arr << 9
	println("\${arr[0]}:\${arr[1]}:\${arr.len}")
}
') or {
		panic(err)
	}
	assert e.stdout() == '7:9:2\n'
}

fn test_eval_array_append_adapts_sum_elements() {
	mut e := create()
	e.run_text('
type Any = int | string

fn main() {
	mut xs := []Any{}
	xs << 1
	xs << "s"
	println(int_str(xs[0]._typ))
	println(int_str(xs[1]._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n'
}

fn test_eval_array_append_keeps_nested_array_rhs_as_single_element() {
	mut e := create()
	e.run_text('
	fn main() {
	mut nested := [][]int{}
	nested << [1, 2]
	println(int_str(nested.len))
	println(int_str(nested[0].len))
	println(int_str(nested[0][1]))

	mut flat := []int{}
	flat << [3, 4]
	println(int_str(flat.len))
	println(int_str(flat[1]))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n2\n2\n2\n4\n'
}

fn test_eval_array_append_index_target_evaluates_once() {
	mut e := create()
	e.run_text('
	fn next(mut i int) int {
		old := i
		i++
		return old
	}

	fn main() {
		mut xs := [][]int{}
		xs << [1]
		xs << [2]
		mut i := 0
		xs[next(mut i)] << 9
		println(int_str(xs[0].len))
		println(int_str(xs[0][1]))
		println(int_str(xs[1].len))
		println(int_str(i))
	}
		') or {
		panic(err)
	}
	assert e.stdout() == '2\n9\n1\n1\n'
}

fn test_eval_array_append_rhs_uses_element_enum_type() {
	mut e := create()
	e.run_text('
	enum Color {
		red = 10
	}

	enum Other {
		red = 1
	}

	fn main() {
		mut xs := []Color{}
		xs << .red
		println(xs[0] == Color.red)
	}
		') or {
		panic(err)
	}
	assert e.stdout() == 'true\n'
}

fn test_eval_value_blocks_execute_array_append_statements() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn main() {
	mut xs := []int{}
	n := if true {
		xs << 1
		xs.len
	} else {
		0
	}
	m := maybe() or {
		xs << 2
		xs.len
	}
	println(int_str(n))
	println(int_str(m))
	println(int_str(xs.len))
	println(int_str(xs[0]))
	println(int_str(xs[1]))
}
		') or {
		panic(err)
	}
	assert e.stdout() == '1\n2\n2\n1\n2\n'
}

fn test_eval_match_value_branches_execute_array_append_statements() {
	mut e := create()
	e.run_text('
fn main() {
	mut xs := []int{}
	n := match 0 {
		0 {
			xs << 1
			xs.len
		}
		else {
			0
		}
	}
	m := match 1 {
		0 {
			0
		}
		else {
			xs << 2
			xs.len
		}
	}
	println(int_str(n))
	println(int_str(m))
	println(int_str(xs.len))
	println(int_str(xs[0]))
	println(int_str(xs[1]))
}
		') or {
		panic(err)
	}
	assert e.stdout() == '1\n2\n2\n1\n2\n'
}

fn test_eval_or_block_return_propagates_from_array_append_rhs() {
	mut e := create()
	e.run_text('
	fn maybe() ?int {
	return none
}

fn run() int {
	mut xs := []int{}
	xs << (maybe() or {
		return 7
	})
	return xs.len
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_for_mut_array_writes_back_elements() {
	mut e := create()
	e.run_text('
struct Item {
mut:
	n int
}

fn main() {
	mut xs := [Item{n: 1}, Item{n: 2}]
	for mut item in xs {
		item.n += 10
	}
	println(int_str(xs[0].n))
	println(int_str(xs[1].n))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '11\n12\n'
}

fn test_eval_for_mut_reuses_resolved_array_container() {
	mut e := create()
	e.run_text('
fn next(mut i int) int {
	old := i
	i++
	return old
}

fn main() {
	mut buckets := [[1], [10]]
	mut i := 0
	for mut x in buckets[next(mut i)] {
		x++
	}
	println(int_str(buckets[0][0]))
	println(int_str(buckets[1][0]))
	println(int_str(i))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '2\n10\n1\n'
}

fn test_eval_array_init_preserves_cap() {
	mut e := create()
	e.run_text('
	fn main() {
	a := []int{cap: 4}
	b := []int{len: 1, cap: 4}
	println(int_str(a.len))
	println(int_str(a.cap))
	println(int_str(b.len))
	println(int_str(b.cap))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n4\n1\n4\n'
}

fn test_eval_array_init_adapts_sum_elements() {
	mut e := create()
	e.run_text('
type Any = int | string

fn main() {
	xs := []Any{1, "s"}
	ys := []Any{len: 2, init: 1}
	println(int_str(xs[0]._typ))
	println(int_str(xs[1]._typ))
	println(int_str(ys[1]._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n0\n'
}

fn test_eval_array_init_flow_propagates_return() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() []int {
	return []int{len: 1, init: maybe() or { return [7] }}
}

fn main() {
	xs := run()
	println(int_str(xs.len))
	println(int_str(xs[0]))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n7\n'
}

fn test_eval_array_init_evaluates_init_for_each_index() {
	mut e := create()
	e.run_text('
fn bump(mut calls int) int {
	calls++
	return calls
}

fn main() {
	mut calls := 0
	xs := []int{len: 3, init: index}
	ys := []int{len: 3, init: index + bump(mut calls)}
	fixed := [3]int{init: index}
	println(int_str(xs[0]))
	println(int_str(xs[2]))
	println(int_str(ys[0]))
	println(int_str(ys[2]))
	println(int_str(calls))
	println(int_str(fixed[2]))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n2\n1\n5\n3\n2\n'
}

fn test_eval_fixed_array_init_uses_declared_len() {
	mut e := create()
	e.run_text('
fn main() {
	a := [3]int{}
	b := [3]int{init: 5}
	nested := [2][2]int{}
	dynamic := [][2]int{}
	println(int_str(a.len))
	println(int_str(a[2]))
	println(int_str(b.len))
	println(int_str(b[1]))
	println(int_str(nested[1][1]))
	println(int_str(dynamic.len))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '3\n0\n3\n5\n0\n0\n'
}

fn test_eval_fixed_array_init_resolves_const_len() {
	mut e := create()
	e.run_text('
const n = 3

struct Box {
	xs [n]int
}

fn main() {
	a := [n]int{}
	b := [n]int{init: index}
	box := Box{}
	println(int_str(a.len))
	println(int_str(a[2]))
	println(int_str(b.len))
	println(int_str(b[2]))
	println(int_str(box.xs.len))
	println(int_str(box.xs[2]))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '3\n0\n3\n2\n3\n0\n'
}

fn test_eval_right_shift_compound_assignment() {
	mut e := create()
	e.run_text('
fn main() {
	mut x := 8
	x >>= 1
	println(int_str(x))
}
') or { panic(err) }
	assert e.stdout() == '4\n'
}

fn test_eval_unsigned_right_shift_and_compound_assignment() {
	mut e := create()
	e.run_text('
fn main() {
	println(int_str(i64(-5) >>> 1))
	mut x := i64(-5)
	x >>>= 1
	println(int_str(x))
}
') or {
		panic(err)
	}
	assert e.stdout() == '9223372036854775805\n9223372036854775805\n'
}

fn test_eval_compound_index_target_evaluates_once() {
	mut e := create()
	e.run_text('
fn next(mut i int) int {
	old := i
	i++
	return old
}

fn main() {
	mut xs := [1, 2]
	mut i := 0
	xs[next(mut i)] += 10
	println(int_str(xs[0]))
	println(int_str(xs[1]))
	println(int_str(i))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '11\n2\n1\n'
}

fn test_eval_selector_index_receiver_evaluates_once_on_write() {
	mut e := create()
	e.run_text('
struct Item {
mut:
	x int
}

fn next(mut i int) int {
	old := i
	i++
	return old
}

fn main() {
	mut xs := [Item{x: 1}, Item{x: 2}]
	mut i := 0
	xs[next(mut i)].x = 5
	println(int_str(xs[0].x))
	println(int_str(xs[1].x))
	println(int_str(i))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '5\n2\n1\n'
}

fn test_eval_struct_method_and_map_update() {
	mut e := create()
	e.run_text("
struct Point {
	x int
	y int
}

fn (p Point) sum() int {
	return p.x + p.y
}

fn main() {
	p := Point{x: 2, y: 5}
	mut m := map[string]int{}
	m['a'] = p.sum()
	m['b'] += 3
	println(int_str(m['a'] + m['b']))
}
") or {
		panic(err)
	}
	assert e.stdout() == '10\n'
}

fn test_eval_static_method_dispatches_from_type_value() {
	mut e := create()
	e.run_text('
struct Foo {
	n int
}

fn Foo.new(n int) Foo {
	return Foo{n: n}
}

fn main() {
	foo := Foo.new(7)
	println(int_str(foo.n))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_overloaded_plus_operator_dispatches_method() {
	mut e := create()
	e.run_text('
struct Number {
	value int
}

fn (a Number) + (b Number) Number {
	return Number{value: a.value + b.value}
}

fn main() {
	c := Number{value: 2} + Number{value: 5}
	println(int_str(c.value))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_alias_receiver_method_dispatches_by_static_type() {
	mut e := create()
	e.run_text('
type UserId = int

fn (id UserId) next() int {
	return int(id) + 1
}

fn main() {
	id := UserId(1)
	println(int_str(id.next()))
	println(int_str(UserId(2).next()))
}
') or {
		panic(err)
	}
	assert e.stdout() == '2\n3\n'
}

fn test_eval_typeof_name_selector_returns_type_name() {
	mut e := create()
	e.run_text('
fn main() {
	println(typeof(1).name)
	println(typeof("x").name)
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'int\nstring\n'
}

fn test_eval_map_delete_mutates_receiver() {
	mut e := create()
	e.run_text("
	fn main() {
	mut m := map[string]int{}
	m['a'] = 7
	m.delete('a')
	println(int_str(m.len))
	println('a' in m)
	println(int_str(m['a']))
}
") or {
		panic(err)
	}
	assert e.stdout() == '0\nfalse\n0\n'
}

fn test_eval_map_delete_index_receiver_evaluates_once() {
	mut e := create()
	e.run_text("
	fn next(mut i int) int {
		old := i
		i++
		return old
	}

	fn main() {
		mut maps := []map[string]int{}
		maps << map[string]int{'a': 1}
		maps << map[string]int{'a': 2}
		mut i := 0
		maps[next(mut i)].delete('a')
		println('a' in maps[0])
		println('a' in maps[1])
		println(int_str(i))
	}
	") or {
		panic(err)
	}
	assert e.stdout() == 'false\ntrue\n1\n'
}

fn test_eval_map_literal_adapts_sum_values() {
	mut e := create()
	e.run_text('
	type Any = int | string

fn main() {
	m := map[string]Any{"a": 1, "b": "s"}
	println(int_str(m["a"]._typ))
	println(int_str(m["b"]._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n'
}

fn test_eval_map_literal_keys_use_declared_enum_type() {
	mut e := create()
	e.run_text('
enum Color {
	red
}

enum Other {
	red = 10
}

fn main() {
	m := map[Color]int{.red: 7}
	println(int_str(m[Color.red]))
	println(int_str(m[.red] or { 0 }))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n7\n'
}

fn test_eval_container_methods_use_declared_enum_arg_type() {
	mut e := create()
	e.run_text('
enum Color {
	red
	blue
}

enum Other {
	red = 10
}

fn main() {
	mut m := map[Color]int{.red: 1}
	m.delete(.red)
	println(int_str(m.len))
	xs := [Color.red, Color.blue]
	println(xs.contains(.red))
	println(int_str(xs.index(.red)))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\ntrue\n0\n'
}

fn test_eval_map_literal_value_flow_propagates_return() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() map[string]int {
	return {
		"a": maybe() or {
			return {
				"b": 7
			}
		}
	}
}

fn main() {
	m := run()
	println(int_str(m["b"]))
	println(int_str(m["a"]))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n0\n'
}

fn test_eval_map_lookup_adapts_sum_keys() {
	mut e := create()
	e.run_text('
type Any = int | string

fn main() {
	m := map[Any]int{1: 2, "s": 3}
	println(int_str(m[1]))
	println(int_str(m["s"]))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '2\n3\n'
}

fn test_eval_indexed_writes_adapt_sum_values() {
	mut e := create()
	e.run_text('
type Any = int | string

fn main() {
	mut xs := []Any{len: 2}
	xs[0] = 1
	xs[1] = "s"
	println(int_str(xs[0]._typ))
	println(int_str(xs[1]._typ))

	mut m := map[string]Any{}
	m["a"] = 1
	m["b"] = "s"
	println(int_str(m["a"]._typ))
	println(int_str(m["b"]._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n0\n1\n'
}

fn test_eval_index_assignment_lhs_index_propagates_return() {
	mut e := create()
	e.run_text('
fn idx() ?int {
	return none
}

fn run() int {
	mut xs := [0]
	xs[idx() or {
		return 7
	}] = 1
	return xs[0]
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_postfix_index_target_evaluates_once() {
	mut e := create()
	e.run_text('
fn next(mut i int) int {
	old := i
	i++
	return old
}

fn main() {
	mut xs := [1, 2]
	mut i := 0
	old := xs[next(mut i)]++
	println(int_str(old))
	println(int_str(xs[0]))
	println(int_str(xs[1]))
	println(int_str(i))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n2\n2\n1\n'
}

fn test_eval_postfix_index_target_propagates_flow() {
	mut e := create()
	e.run_text('
fn idx() ?int {
	return none
}

fn run() int {
	mut xs := [0]
	xs[idx() or {
		return 7
	}]++
	return xs[0]
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_array_literal_preserves_element_type_for_sum_checks() {
	mut e := create()
	e.run_text('
type Any = []int | string

fn main() {
	x := Any([1, 2])
	println(x is []int)
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'true\n'
}

fn test_eval_shorthand_map_literal_infers_key_value_types() {
	mut e := create()
	e.run_text('
fn main() {
	m := {"a": 1}
	println(int_str(m["a"]))
	println(int_str(m["missing"]))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n0\n'
}

fn test_eval_typed_map_target_adapts_entries() {
	mut e := create()
	e.run_text('
type Any = int | string

struct S {
	m map[string]Any
}

fn main() {
	s := S{
		m: {
			"a": 1
			"b": "s"
		}
	}
	println(int_str(s.m["a"]._typ))
	println(int_str(s.m["b"]._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n'
}

fn test_eval_mut_receiver_method_updates_original() {
	mut e := create()
	e.run_text('
struct Point {
mut:
	x int
}

fn (mut p Point) inc() int {
	p.x += 1
	return p.x
}

fn main() {
	mut p := Point{x: 1}
	println(int_str(p.inc()))
	println(int_str(p.x))
}
') or {
		panic(err)
	}
	assert e.stdout() == '2\n2\n'
}

fn test_eval_mut_function_arg_updates_original() {
	mut e := create()
	e.run_text('
fn inc(mut x int) {
	x++
}

fn main() {
	mut a := 1
	inc(mut a)
	println(int_str(a))
}
') or {
		panic(err)
	}
	assert e.stdout() == '2\n'
}

fn test_eval_mut_index_argument_writes_back_once() {
	mut e := create()
	e.run_text('
fn next(mut i int) int {
	old := i
	i++
	return old
}

fn inc(mut x int) {
	x++
}

fn main() {
	mut xs := [1, 2]
	mut i := 0
	inc(mut xs[next(mut i)])
	println(int_str(xs[0]))
	println(int_str(xs[1]))
	println(int_str(i))
}
') or {
		panic(err)
	}
	assert e.stdout() == '2\n2\n1\n'
}

fn test_eval_mut_function_literal_arg_updates_original() {
	mut e := create()
	e.run_text('
fn main() {
	f := fn (mut x int) {
		x++
	}
	mut a := 1
	f(mut a)
	println(int_str(a))
}
') or {
		panic(err)
	}
	assert e.stdout() == '2\n'
}

fn test_eval_function_literal_arg_adapts_to_sum_param() {
	mut e := create()
	e.run_text('
type Any = int | string

fn main() {
	f := fn (x Any) {
		println(int_str(x._typ))
	}
	f(1)
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n'
}

fn test_eval_variadic_arguments_pack_into_array() {
	mut e := create()
	e.run_text('
fn count(xs ...int) int {
	return xs.len
}

fn sum(seed int, xs ...int) int {
	mut total := seed
	for x in xs {
		total += x
	}
	return total
}

fn main() {
	println(int_str(count()))
	println(int_str(count(1, 2)))
	println(int_str(sum(10, 1, 2, 3)))
	f := fn (xs ...int) int {
		return xs.len
	}
	println(int_str(f(4, 5, 6)))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n2\n16\n3\n'
}

fn test_eval_function_literal_captures_escaping_scope() {
	mut e := create()
	e.run_text('
fn make() fn () int {
	x := 7
	return fn [x] () int {
		return x
	}
}

fn main() {
	f := make()
	println(int_str(f()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_mut_function_literal_capture_persists_between_calls() {
	mut e := create()
	e.run_text('
fn new_counter() fn () int {
	mut i := 0
	return fn [mut i] () int {
		i++
		return i
	}
}

fn main() {
	counter := new_counter()
	println(int_str(counter()))
	println(int_str(counter()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n2\n'
}

fn test_eval_mut_closure_param_preserves_capture_state() {
	mut e := create()
	e.run_text('
fn new_counter() fn () int {
	mut i := 0
	return fn [mut i] () int {
		i++
		return i
	}
}

fn use(f fn () int) {
	println(int_str(f()))
	println(int_str(f()))
}

fn main() {
	counter := new_counter()
	use(counter)
	println(int_str(counter()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n2\n3\n'
}

fn test_eval_function_valued_selector_call_updates_captures() {
	mut e := create()
	e.run_text('
struct S {
	f fn () int
}

fn new_counter() fn () int {
	mut i := 0
	return fn [mut i] () int {
		i++
		return i
	}
}

fn main() {
	mut s := S{
		f: new_counter()
	}
	println(int_str(s.f()))
	println(int_str(s.f()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n2\n'
}

fn test_eval_mut_method_arg_updates_original() {
	mut e := create()
	e.run_text('
struct S {}

fn (s S) inc(mut x int) {
	x++
}

fn main() {
	s := S{}
	mut a := 1
	s.inc(mut a)
	println(int_str(a))
}
') or {
		panic(err)
	}
	assert e.stdout() == '2\n'
}

fn test_eval_multi_return_assignment_spreads_tuple() {
	mut e := create()
	e.run_text("
fn pair() (int, string) {
	return 2, 'ok'
}

fn main() {
	a, b := pair()
	println(int_str(a))
	println(b)
}
") or {
		panic(err)
	}
	assert e.stdout() == '2\nok\n'
}

fn test_eval_multi_return_assignment_spreads_three_value_tuple() {
	mut e := create()
	e.run_text("
fn triple() (int, string, int) {
	return 1, 'two', 3
}

fn main() {
	a, b, c := triple()
	println(int_str(a))
	println(b)
	println(int_str(c))
}
") or {
		panic(err)
	}
	assert e.stdout() == '1\ntwo\n3\n'
}

fn test_eval_multi_return_adapts_items_to_declared_types() {
	mut e := create()
	e.run_text('
type Any = int | string

fn pair() (Any, Any) {
	return 1, "s"
}

fn main() {
	a, b := pair()
	println(int_str(a._typ))
	println(int_str(b._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n'
}

fn test_eval_imported_return_adapts_in_callee_module() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_import_return_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub type Any = int | string

pub fn make() Any {
	return 1
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

fn main() {
	x := m.make()
	println(int_str(x._typ))
	if x is int {
		println("int")
	}
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '0\nint\n'
}

fn test_eval_imported_array_return_qualifies_element_type() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_import_array_return_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub struct Item {}

pub fn items() []Item {
	return [Item{}]
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

fn main() {
	mut xs := m.items()
	xs << [m.Item{}, m.Item{}]
	println(int_str(xs.len))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '3\n'
}

fn test_eval_imported_struct_field_default_adapts_in_declaring_module() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_import_field_default_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub struct A {}

pub type Any = A | string

pub struct Box {
pub:
	x Any = A{}
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

fn main() {
	box := m.Box{}
	println(int_str(box.x._typ))
	if box.x is m.A {
		println("a")
	}
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '0\na\n'
}

fn test_eval_imported_struct_field_container_type_uses_declaring_module() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_import_field_container_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub struct Item {}

pub struct Box {
pub mut:
	xs []Item
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

fn main() {
	box := m.Box{xs: [m.Item{}]}
	mut xs := box.xs
	xs << [m.Item{}, m.Item{}]
	println(int_str(xs.len))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '3\n'
}

fn test_eval_value_block_preserves_multi_return_values() {
	mut e := create()
	e.run_text("
fn choose(ok bool) (int, string) {
	return if ok { 1, 'a' } else { 2, 'b' }
}

fn main() {
	a, b := choose(true)
	c, d := choose(false)
	println('\${a}:\${b}:\${c}:\${d}')
}
") or {
		panic(err)
	}
	assert e.stdout() == '1:a:2:b\n'
}

fn test_eval_value_block_uses_last_sequential_expression_value() {
	mut e := create()
	e.run_text('
	fn trace() int {
		return 1
	}

	fn choose(ok bool) int {
		return if ok {
			trace()
		2
	} else {
		3
	}
}

fn main() {
	println(int_str(choose(true)))
	println(int_str(choose(false)))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '2\n3\n'
}

fn test_eval_multi_assign_preserves_rhs_values() {
	mut e := create()
	e.run_text('
fn main() {
	mut a := 1
	mut b := 2
	a, b = b, a
	println(int_str(a))
	println(int_str(b))
}
') or {
		panic(err)
	}
	assert e.stdout() == '2\n1\n'
}

fn test_eval_multi_assign_propagates_rhs_flow() {
	mut e := create()
	e.run_text('
fn maybe_pair() ?(int, int) {
	return none
}

fn run() int {
	a, b := maybe_pair() or {
		return 7
	}
	return a + b
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_struct_equality_compares_fields() {
	mut e := create()
	e.run_text('
struct Point {
	x int
}

fn main() {
	a := Point{x: 1}
	b := Point{x: 2}
	c := Point{x: 1}
	println(a == b)
	println(a == c)
}
') or {
		panic(err)
	}
	assert e.stdout() == 'false\ntrue\n'
}

fn test_eval_scopes_enum_constants_by_enum_type() {
	mut e := create()
	e.run_text('
enum A {
	x
}

enum B {
	x = 10
}

fn main() {
	println(int_str(A.x))
	println(int_str(B.x))
}
') or {
		panic(err)
	}
	assert e.stdout() == '0\n10\n'
}

fn test_eval_enum_selector_preserves_sum_variant_type() {
	mut e := create()
	e.run_text('
enum Test {
	abc
}

type SumTest = Test | u8

fn main() {
	b := SumTest(Test.abc)
	println(b is Test)
	println(b is u8)
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'true\nfalse\n'
}

fn test_eval_match_shorthand_enum_uses_target_enum_type() {
	mut e := create()
	e.run_text('
enum A {
	x
}

enum B {
	x = 10
}

fn main() {
	match A.x {
		.x {
			println("a")
		}
		else {
			println("bad")
		}
	}
}
') or {
		panic(err)
	}
	assert e.stdout() == 'a\n'
}

fn test_eval_match_shorthand_enum_uses_tracked_var_type() {
	mut e := create()
	e.run_text('
enum A {
	x
}

enum B {
	x = 10
}

fn main() {
	a := A.x
	match a {
		.x {
			println("a")
		}
		else {
			println("bad")
		}
	}
}
') or {
		panic(err)
	}
	assert e.stdout() == 'a\n'
}

fn test_eval_enum_zero_values_use_first_field() {
	mut e := create()
	e.run_text('
	enum Color {
	red
	blue
}

struct S {
	c Color
}

__global g Color

fn main() {
	s := S{}
	arr := [2]Color{}
	m := map[string]Color{}
	println(s.c == Color.red)
	println(g == Color.red)
	println(arr[0] == Color.red)
	println(m["missing"] == Color.red)
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'true\ntrue\ntrue\ntrue\n'
}

fn test_eval_enum_cast_preserves_enum_identity() {
	mut e := create()
	e.run_text('
	enum Color {
		red
		blue
	}

	type Any = Color | int

	fn main() {
		c := Color(1)
		x := Any(Color(1))
		println(c == Color.blue)
		println(c)
		println(x is Color)
	}
		') or {
		panic(err)
	}
	assert e.stdout() == 'true\nblue\ntrue\n'
}

fn test_eval_global_assignment_uses_declared_type() {
	mut e := create()
	e.run_text('
	enum State {
	idle
	busy
}

__global state State

fn main() {
	state = .busy
	println(state)
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'busy\n'
}

fn test_eval_enum_shorthand_return_preserves_enum_type() {
	mut e := create()
	e.run_text('
enum Color {
	red
	blue
}

type Any = Color | int

fn color() Color {
	return .red
}

fn main() {
	c := color()
	x := Any(c)
	println(c == Color.red)
	println(x is Color)
	println(int_str(x._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'true\ntrue\n0\n'
}

fn test_eval_enum_shorthand_return_uses_expected_type_in_value_branches() {
	mut e := create()
	e.run_text('
enum Color {
	red = 10
	blue = 20
}

enum Other {
	red = 1
	blue = 2
}

fn pick_if(b bool) Color {
	return if b {
		.red
	} else {
		.blue
	}
}

fn pick_match(n int) Color {
	return match n {
		0 {
			.red
		}
		else {
			.blue
		}
	}
}

fn main() {
	println(pick_if(true) == Color.red)
	println(pick_if(false) == Color.blue)
	println(pick_match(0) == Color.red)
	println(pick_match(1) == Color.blue)
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'true\ntrue\ntrue\ntrue\n'
}

fn test_eval_enum_shorthand_uses_typed_call_and_struct_contexts() {
	mut e := create()
	e.run_text('
enum A {
	x
}

enum B {
	x = 10
}

struct S {
	c A
}

fn take_a(a A) int {
	return int(a)
}

fn main() {
	s := S{c: .x}
	println(int_str(take_a(.x)))
	println(int_str(s.c))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n0\n'
}

fn test_eval_imported_function_param_hints_are_qualified() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_import_param_hint_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub enum Color {
	red
	blue
}

pub fn take(c Color) string {
	if c == Color.red {
		return "m-red"
	}
	return "wrong"
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

enum Other {
	red = 10
}

fn main() {
	println(m.take(.red))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == 'm-red\n'
}

fn test_eval_imported_fn_value_param_hints_are_qualified() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_import_fn_value_hint_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub enum Color {
	red
	blue
}

pub fn make_taker() fn (Color) string {
	return fn (c Color) string {
		if c == Color.red {
			return "m-red"
		}
		return "wrong"
	}
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

enum Other {
	red = 10
}

fn main() {
	f := m.make_taker()
	println(f(.red))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == 'm-red\n'
}

fn test_eval_match_range_patterns_are_inclusive() {
	mut e := create()
	e.run_text('
fn classify_stmt(n int) string {
	mut out := ""
	match n {
		1...3 {
			out = "range"
		}
		else {
			out = "other"
		}
	}
	return out
}

fn classify_expr(n int) string {
	return match n {
		1...3 {
			"range"
		}
		else {
			"other"
		}
	}
}

fn main() {
	println(classify_stmt(3))
	println(classify_expr(1))
	println(classify_expr(4))
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'range\nrange\nother\n'
}

fn test_eval_match_primitive_sum_branches_are_type_patterns() {
	mut e := create()
	e.run_text('
	type Any = int | string

fn classify(x Any) string {
	return match x {
		int {
			"int"
		}
		string {
			"string"
		}
		else {
			"other"
		}
	}
}

fn main() {
	println(classify(1))
	println(classify("s"))
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'int\nstring\n'
}

fn test_eval_match_value_pattern_is_not_type_pattern() {
	mut e := create()
	e.run_text('
	type Any = int | string

	fn classify(x Any) string {
		return match x {
			"int" {
				"literal"
			}
			int {
				"type"
			}
			else {
				"other"
			}
		}
	}

	fn main() {
		println(classify(1))
	}
		') or {
		panic(err)
	}
	assert e.stdout() == 'type\n'
}

fn test_eval_sum_smartcasts_bind_branch_payloads() {
	mut e := create()
	e.run_text('
	type Any = int | string

fn if_cast(x Any) int {
	if x is int {
		return x + 1
	}
	return 0
}

fn match_cast(x Any) int {
	return match x {
		int {
			x + 2
		}
		else {
			0
		}
	}
}

fn main() {
	println(int_str(if_cast(1)))
	println(int_str(if_cast("s")))
	println(int_str(match_cast(1)))
	println(int_str(match_cast("s")))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '2\n0\n3\n0\n'
}

fn test_eval_logical_and_rhs_uses_is_smartcast() {
	mut e := create()
	e.run_text('
type Any = int | string

fn positive(x Any) bool {
	return x is int && x > 0
}

fn main() {
	println(positive(2))
	println(positive(-1))
	println(positive("s"))
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'true\nfalse\nfalse\n'
}

fn test_eval_calls_function_value_from_identifier() {
	mut e := create()
	e.run_text('
fn main() {
	f := fn () int {
		return 1
	}
	println(int_str(f()))
}
') or {
		panic(err)
	}
	assert e.stdout() == '1\n'
}

fn test_eval_prefix_minus_preserves_float() {
	mut e := create()
	e.run_text('
fn main() {
	x := 1.5
	println(-1.5)
	println(-x)
}
	') or { panic(err) }
	assert e.stdout() == '-1.5\n-1.5\n'
}

fn test_eval_mixed_numeric_equality_compares_as_float() {
	mut e := create()
	e.run_text('
fn main() {
	println(1 == 1.5)
	println(1.5 == 1)
	println(1 == 1.0)
	println(1.0 == 1)
	println(1 != 1.5)
}
') or {
		panic(err)
	}
	assert e.stdout() == 'false\nfalse\ntrue\ntrue\ntrue\n'
}

fn test_eval_number_literals_strip_separators() {
	mut e := create()
	e.run_text('
fn main() {
	println(int_str(1_234))
	println(int_str(0x1_f))
	println(1_234.5 == 1234.5)
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1234\n31\ntrue\n'
}

fn test_eval_sum_numeric_as_cast_unwraps_payload() {
	mut e := create()
	e.run_text('
type Any = int | string

fn main() {
	x := Any(7)
	println(int_str(x as int))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_alias_field_zero_value_uses_underlying_type() {
	mut e := create()
	e.run_text('
type UserId = int

struct S {
	id UserId
}

fn main() {
	println(int_str(S{}.id))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n'
}

fn test_eval_alias_to_sum_param_adapts_argument() {
	mut e := create()
	e.run_text('
type Any = int | string
type Alias = Any

fn variant(x Alias) int {
	return x._typ
}

fn main() {
	println(int_str(variant(1)))
	println(int_str(variant("s")))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n'
}

fn test_eval_struct_field_writes_adapt_sum_values() {
	mut e := create()
	e.run_text('
type Any = int | string

struct S {
mut:
	a Any
}

fn main() {
	mut s := S{}
	s.a = 1
	println(int_str(s.a._typ))
	s.a = "s"
	println(int_str(s.a._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n'
}

fn test_eval_struct_init_fields_adapt_sum_values() {
	mut e := create()
	e.run_text('
type Any = int | string

struct S {
	a Any
	b Any
}

fn main() {
	s := S{a: 1, b: "s"}
	println(int_str(s.a._typ))
	println(int_str(s.b._typ))
	t := S{
		...s
		a: "x"
	}
	println(int_str(t.a._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n1\n'
}

fn test_eval_defer_runs_before_function_return() {
	mut e := create()
	e.run_text('
fn run() int {
	defer {
		println("first")
	}
	defer {
		println("second")
	}
	println("body")
	return 7
}

fn main() {
	println(int_str(run()))
}
') or {
		panic(err)
	}
	assert e.stdout() == 'body\nsecond\nfirst\n7\n'
}

fn test_eval_scoped_defers_run_at_scope_exit() {
	mut e := create()
	e.run_text('
fn main() {
	mut out := ""
	if true {
		defer {
			out += "d"
		}
		out += "b"
	}
	out += "a"
	for i in 0 .. 2 {
		defer {
			out += int_str(i)
		}
		out += "i"
	}
	println(out)
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'bdai0i1\n'
}

fn test_eval_defer_fn_runs_at_function_exit() {
	mut e := create()
	e.run_text('
__global hit int

fn run() {
	for _ in 0 .. 2 {
		defer(fn) {
			hit += 100
		}
		println(int_str(hit))
		hit++
	}
	println(int_str(hit))
}

fn main() {
	run()
	println(int_str(hit))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n1\n2\n202\n'
}

fn test_eval_or_block_return_propagates() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() int {
	_ := maybe() or {
		return 1
	}
	return 2
}

fn main() {
	println(int_str(run()))
}
') or {
		panic(err)
	}
	assert e.stdout() == '1\n'
}

fn test_eval_optional_bool_return_wraps_false_payload() {
	mut e := create()
	e.run_text('
fn maybe() ?bool {
	return false
}

fn run_or() bool {
	return maybe() or {
		return true
	}
}

fn run_guard() string {
	if v := maybe() {
		if v {
			return "true"
		}
		return "false"
	}
	return "none"
}

fn main() {
	println(run_or())
	println(run_guard())
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'false\nfalse\n'
}

fn test_eval_optional_if_guard_binds_payload_type() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return 41
}

fn absent() ?int {
	return none
}

fn main() {
	if x := maybe() {
		println(int_str(x + 1))
	} else {
		println("missing")
	}
	if x := absent() {
		println(int_str(x))
	} else {
		println("absent")
	}
}
	') or {
		panic(err)
	}
	assert e.stdout() == '42\nabsent\n'
}

fn test_eval_optional_sum_return_adapts_payload_before_wrapping() {
	mut e := create()
	e.run_text('
type Any = int | string

fn maybe() ?Any {
	return 1
}

fn main() {
	x := maybe() or {
		return
	}
	println(int_str(x._typ))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n'
}

fn test_eval_or_block_return_propagates_from_call_argument() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() int {
	println(int_str(maybe() or {
		return 1
	}))
	return 2
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n'
}

fn test_eval_or_block_return_propagates_from_nested_infix() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() int {
	x := (maybe() or {
		return 12
	}) + 1
	return x
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '12\n'
}

fn test_eval_or_block_return_propagates_from_selector_receiver() {
	mut e := create()
	e.run_text('
fn maybe() ?string {
	return none
}

fn run() string {
	n := (maybe() or {
		return "early"
	}).len
	return int_str(n)
}

fn main() {
	println(run())
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'early\n'
}

fn test_eval_or_block_return_propagates_from_method_receiver() {
	mut e := create()
	e.run_text('
fn maybe_arr() ?[]int {
	return none
}

fn run() int {
	x := (maybe_arr() or {
		return 12
	}).first()
	return x
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '12\n'
}

fn test_eval_or_block_return_propagates_from_outer_or_left_operand() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn wrap(x int) ?int {
	return x
}

fn run() int {
	_ := wrap(maybe() or {
		return 1
	}) or {
		return 2
	}
	return 3
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\n'
}

fn test_eval_or_block_return_propagates_from_match_subject() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() int {
	match (maybe() or {
		return 7
	}) {
		else {}
	}
	return 0
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_map_index_or_uses_lookup_presence() {
	mut e := create()
	e.run_text('
fn main() {
	mut m := map[string]int{}
	m["present"] = 0
	println(int_str(m["present"] or { 7 }))
	println(int_str(m["missing"] or { 7 }))

	mut s := map[string]string{}
	s["empty"] = ""
	println(s["empty"] or { "fallback" })
	println(s["missing"] or { "fallback" })
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\n7\n\nfallback\n'
}

fn test_eval_map_index_if_guard_uses_lookup_presence() {
	mut e := create()
	e.run_text('
fn main() {
	mut m := map[string]int{}
	m["present"] = 0
	if v := m["present"] {
		println(int_str(v))
	} else {
		println("missing-present")
	}
	if v := m["missing"] {
		println(int_str(v))
	} else {
		println("missing")
	}
}
	') or {
		panic(err)
	}
	assert e.stdout() == '0\nmissing\n'
}

fn test_eval_postfix_option_propagates_failure() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() ?int {
	maybe()?
	println("after")
	return 1
}

fn main() {
	run() or {
		println("none")
		return
	}
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'none\n'
}

fn test_eval_or_block_binds_err() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn main() {
	maybe() or {
		_ := err
		println("handled")
		return
	}
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'handled\n'
}

fn test_eval_or_block_return_propagates_from_for_in_header() {
	mut e := create()
	e.run_text('
fn maybe_arr() ?[]int {
	return none
}

fn maybe_int() ?int {
	return none
}

fn maybe_two() ?int {
	return 2
}

fn iterable() int {
	for x in (maybe_arr() or {
		return 7
	}) {
		_ = x
	}
	return 0
}

fn range_start() int {
	for i in (maybe_int() or {
		return 8
	}) .. 3 {
		_ = i
	}
	return 0
}

fn range_end() int {
	for i in 0 .. (maybe_int() or {
		return 9
	}) {
		_ = i
	}
	return 0
}

fn main() {
	println(int_str(iterable()))
	println(int_str(range_start()))
	println(int_str(range_end()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n8\n9\n'
}

fn test_eval_or_block_return_propagates_from_flow_contexts() {
	mut e := create()
	e.run_text('
fn maybe_bool() ?bool {
	return none
}

fn maybe_arr() ?[]int {
	return none
}

fn maybe_int() ?int {
	return none
}

fn maybe_two() ?int {
	return 2
}

fn if_condition() int {
	if maybe_bool() or {
		return 1
	} {
		return 99
	}
	return 0
}

fn assert_condition() bool {
	assert maybe_bool() or {
		return false
	}
	return true
}

fn loop_condition() int {
	for maybe_bool() or {
		return 5
	} {
		return 99
	}
	return 0
}

fn index_receiver() int {
	x := (maybe_arr() or {
		return 6
	})[0]
	return x
}

fn index_value() int {
	arr := [10]
	x := arr[maybe_int() or {
		return 7
	}]
	return x
}

fn if_expr_branch() int {
	x := if true {
		maybe_int() or {
			return 8
		}
	} else {
		0
	}
	return x
}

fn if_guard_scope() int {
	x := 1
	if x := maybe_two() {
		if x != 2 {
			return 99
		}
	}
	return x
}

fn main() {
	println(int_str(if_condition()))
	println(assert_condition())
	println(int_str(loop_condition()))
	println(int_str(index_receiver()))
	println(int_str(index_value()))
	println(int_str(if_expr_branch()))
	println(int_str(if_guard_scope()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '1\nfalse\n5\n6\n7\n8\n1\n'
}

fn test_eval_or_block_return_propagates_from_array_literal_element() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() []int {
	_ := [maybe() or {
		return [7]
	}]
	return [0]
}

fn main() {
	println(int_str(run()[0]))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_or_block_return_propagates_from_struct_init_field() {
	mut e := create()
	e.run_text('
struct S {
	n int
}

fn maybe() ?int {
	return none
}

fn run() S {
	return S{
		n: maybe() or {
			return S{
				n: 7
			}
		}
	}
}

fn main() {
	println(int_str(run().n))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_or_block_return_propagates_from_string_interpolation() {
	mut e := create()
	e.run_text('
fn maybe() ?int {
	return none
}

fn run() int {
	_ := "\${maybe() or {
		return 7
	}}"
	return 9
}

fn main() {
	println(int_str(run()))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '7\n'
}

fn test_eval_match_expression_propagates_branch_return() {
	mut e := create()
	e.run_text('
fn run(n int) int {
	x := match n {
		0 {
			return 12
		}
		else {
			7
		}
	}
	return x + 1
}

fn main() {
	println(int_str(run(0)))
	println(int_str(run(1)))
}
	') or {
		panic(err)
	}
	assert e.stdout() == '12\n8\n'
}

fn test_eval_module_const_uses_defining_module_context() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_const_module_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	mod_file := os.join_path(dir, 'm.v')
	main_file := os.join_path(dir, 'main.v')
	os.write_file(mod_file, '
module m

const x = helper()

fn helper() int {
	return 7
}
') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

fn helper() int {
	return 99
}

fn main() {
	println(int_str(m.x))
}
') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([mod_file, main_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '7\n'
}

fn test_eval_enum_initializers_use_declaring_module_context() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_enum_module_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	mod_file := os.join_path(dir, 'm.v')
	main_file := os.join_path(dir, 'main.v')
	os.write_file(mod_file, '
module m

const base = 2

pub enum E {
	a = base
	b
}

pub fn value() int {
	return int(E.a) * 10 + int(E.b)
}
') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

const base = 10

fn main() {
	println(int_str(m.value()))
}
') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([mod_file, main_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '23\n'
}

fn test_eval_enum_initializers_use_file_import_aliases() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_enum_alias_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	dep_file := os.join_path(dir, 'dep.v')
	main_file := os.join_path(dir, 'main.v')
	os.write_file(dep_file, '
module dep

pub const n = 7
	') or { panic(err) }
	os.write_file(main_file, '
module main

import dep as d

enum E {
	a = d.n
}

fn main() {
	println(int_str(int(E.a)))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, dep_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '7\n'
}

fn test_eval_global_initializers_run_after_registration_in_declaring_module() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_global_init_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

__global g = helper()

fn helper() int {
	return 7
}

pub fn value() int {
	return g
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

fn helper() int {
	return 99
}

__global main_g = helper()

fn main() {
	println(int_str(main_g))
	println(int_str(m.value()))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '99\n7\n'
}

fn test_eval_global_initializers_run_imports_before_main() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_global_init_order_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

__global g = 7

pub fn value() int {
	return g
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

__global x = m.value()

fn main() {
	println(int_str(x))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '7\n'
}

fn test_eval_module_inits_run_dependencies_first() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_init_order_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	moda_file := os.join_path(dir, 'moda.v')
	modb_file := os.join_path(dir, 'modb.v')
	os.write_file(modb_file, '
module modb

__global flag int

fn init() {
	flag = 41
}

pub fn value() int {
	return flag
}
	') or {
		panic(err)
	}
	os.write_file(moda_file, '
module moda

import modb

__global seen int

fn init() {
	seen = modb.value()
}

pub fn value() int {
	return seen
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import moda

fn main() {
	println(int_str(moda.value()))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, moda_file, modb_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '41\n'
}

fn test_eval_struct_metadata_uses_current_module() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_struct_scope_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(main_file, '
module main

import m

struct Point {
	x int
}

fn main() {
	p := Point{}
	println(int_str(p.x))
}
') or {
		panic(err)
	}
	os.write_file(mod_file, '
module m

struct Point {
	y int
}
') or { panic(err) }
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '0\n'
}

fn test_eval_struct_field_defaults_use_declaring_module() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_struct_field_module_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub struct Inner {
	x int
}

pub struct Outer {
	inner Inner
}

pub fn outer_x(o Outer) int {
	return o.inner.x
}
') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

struct Inner {
	y int
}

fn main() {
	outer := m.Outer{}
	println(int_str(m.outer_x(outer)))
}
') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '0\n'
}

fn test_eval_struct_field_default_initializers_are_preserved() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_struct_field_defaults_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub struct S {
pub:
	n int = helper()
}

fn helper() int {
	return 7
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

struct S {
	n int = helper()
}

fn helper() int {
	return 3
}

fn main() {
	println(int_str(S{}.n))
	println(int_str(m.S{}.n))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '3\n7\n'
}

fn test_eval_import_alias_struct_init_resolves_real_module() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_import_alias_struct_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub struct S {
pub:
	n int = 7
}
	') or { panic(err) }
	os.write_file(main_file, '
module main

import m as mm

fn main() {
	println(int_str(mm.S{}.n))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '7\n'
}

fn test_eval_zero_array_field_qualifies_imported_element_type() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_import_array_elem_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub struct Item {}

pub struct Box {
pub mut:
	xs []Item
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

fn main() {
	mut b := m.Box{}
	b.xs << [m.Item{}, m.Item{}]
	println(int_str(b.xs.len))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '2\n'
}

fn test_eval_is_checks_keep_module_qualified_types_distinct() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_type_scope_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub struct Foo {}
') or { panic(err) }
	os.write_file(main_file, '
module main

import m

struct Foo {}

type Any = Foo | m.Foo

fn classify(x Any) string {
	if x is Foo {
		return "local"
	}
	if x is m.Foo {
		return "module"
	}
	return "none"
}

fn main() {
	println(classify(m.Foo{}))
}
') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == 'module\n'
}

fn test_eval_module_sum_type_keeps_primitive_variants_unqualified() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_primitive_type_scope_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub type Any = u32 | string

pub fn classify(x Any) string {
	if x is u32 {
		return "u32"
	}
	if x is string {
		return "string"
	}
	return "none"
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

fn main() {
	println(m.classify(u32(7)))
	println(m.classify("ok"))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == 'u32\nstring\n'
}

fn test_eval_sum_tables_are_module_scoped_and_exact() {
	dir := os.join_path(os.temp_dir(), 'v3_eval_sum_scope_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_file := os.join_path(dir, 'main.v')
	mod_file := os.join_path(dir, 'm.v')
	os.write_file(mod_file, '
module m

pub struct Foo {}

pub type Any = string | bool

pub fn variant_index() int {
	x := Any("s")
	return x._typ
}
	') or {
		panic(err)
	}
	os.write_file(main_file, '
module main

import m

struct Foo {}

type Any = int | string
type Both = Foo | m.Foo

fn local_variant_index() int {
	x := Any("s")
	return x._typ
}

fn imported_variant_index() int {
	x := Both(m.Foo{})
	return x._typ
}

fn main() {
	println(int_str(local_variant_index()))
	println(int_str(m.variant_index()))
	println(int_str(imported_variant_index()))
}
	') or {
		panic(err)
	}
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([main_file, mod_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '1\n0\n1\n'
}

fn test_eval_is_checks_respect_container_element_types() {
	mut e := create()
	e.run_text('
type Any = []int | []string | map[string]int | map[string]string

fn classify(x Any) string {
	if x is []string {
		return "strings"
	}
	if x is []int {
		return "ints"
	}
	if x is map[string]string {
		return "string-map"
	}
	if x is map[string]int {
		return "int-map"
	}
	return "none"
}

fn main() {
	println(classify([]int{len: 1, init: 1}))
	println(classify([]string{len: 1, init: "s"}))
	mut mi := map[string]int{}
	mi["a"] = 1
	println(classify(mi))
	mut ms := map[string]string{}
	ms["a"] = "s"
	println(classify(ms))
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'ints\nstrings\nint-map\nstring-map\n'
}

fn test_eval_primitive_str_methods_are_direct() {
	mut e := create()
	e.run_text('
fn main() {
	total := 42
	println(total.str())
	println(true.str())
	f := 1.5
	println(f.str())
}
	') or {
		panic(err)
	}
	assert e.stdout() == '42\ntrue\n1.5\n'
}

fn test_eval_enum_stringification_uses_field_names() {
	mut e := create()
	e.run_text('
enum State {
	idle
	busy = 10
}

fn main() {
	println(State.busy)
	println("\${State.idle}")
}
	') or {
		panic(err)
	}
	assert e.stdout() == 'busy\nidle\n'
}

fn test_eval_struct_stringification_matches_v_output() {
	mut e := create()
	e.run_text('
struct Point {
	x int
}

struct User {
	name string
	age  int
}

struct Custom {
	n int
}

fn (c Custom) str() string {
	return "custom " + int_str(c.n)
}

fn main() {
	println(Point{x: 1})
	println(User{name: "Ada", age: 2})
	println(Custom{n: 3})
	println("\${Custom{n: 4}}")
}
	') or {
		panic(err)
	}
	assert e.stdout() == "Point{
    x: 1
}
User{
    name: 'Ada'
    age: 2
}
custom 3
custom 4
"
}

fn test_eval_os_join_path_delegates_to_os() {
	mut e := create()
	e.run_text('
import os

fn main() {
	println(os.join_path("", "foo"))
	println(os.join_path("foo", ""))
}
	') or {
		panic(err)
	}
	expected := os.join_path('', 'foo') + '\n' + os.join_path('foo', '') + '\n'
	assert e.stdout() == expected
}

fn test_v3_eval_backend_cli() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_eval_backend_test')
	build := os.execute('${vexe} -gc none -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	src := os.join_path(os.temp_dir(), 'v3_eval_backend_sample.v')
	os.write_file(src, '
fn main() {
	mut total := 0
	for i in 0 .. 7 {
		total += i
	}
	println(int_str(total))
}
') or {
		panic(err)
	}
	result := os.execute('${v3_bin} ${src} -b eval')
	assert result.exit_code == 0
	assert result.output.contains('21\n')
}
