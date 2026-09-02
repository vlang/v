// Regression test for https://github.com/vlang/v/issues/28000
// A `match` whose arms use `!`/`?` propagation, used as the value of an
// `if`-expression, must assign the unwrapped result to the if-expression's
// result temp (it used to emit an empty expression `_t = ;`).
import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_match_as_if_expr_value_with_propagation() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_match_as_if_expr_value_propagation_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_match_as_if_expr_value_propagation_input.v')
	os.write_file(src, 'module main

struct First {}
struct Second {}

type Node = First | Second

fn lower_first(_ First) !int {
	return 1
}

fn lower_second(_ Second) !int {
	return 2
}

fn select_value(node ?Node) !int {
	result := if value := node {
		match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}
	} else {
		0
	}
	return result
}

fn select_value_paren(node ?Node) !int {
	result := if value := node {
		(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

fn select_value_unsafe(node ?Node) !int {
	result := if value := node {
		(unsafe {
			match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		})
	} else {
		0
	}
	return result
}

fn select_value_cast(node ?Node) !i64 {
	result := if value := node {
		i64(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		i64(0)
	}
	return result
}

fn select_value_cast_unsafe(node ?Node) !i64 {
	result := if value := node {
		i64(unsafe {
			match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		})
	} else {
		i64(0)
	}
	return result
}

fn select_value_infix_right(node ?Node) !int {
	result := if value := node {
		1 + (match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

fn select_value_infix_left(node ?Node) !int {
	result := if value := node {
		(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}) + 10
	} else {
		0
	}
	return result
}

fn wrap(x int) int {
	return x * 10
}

fn select_value_callarg(node ?Node) !int {
	result := if value := node {
		wrap(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

fn select_value_nested_callarg(node ?Node) !int {
	result := if value := node {
		wrap(wrap(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}))
	} else {
		0
	}
	return result
}

fn select_value_callarg_infix(node ?Node) !int {
	result := if value := node {
		wrap(1 + (match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}))
	} else {
		0
	}
	return result
}

fn select_value_arraylit(node ?Node) ![]int {
	result := if value := node {
		[match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}]
	} else {
		[0]
	}
	return result
}

fn select_value_prefix(node ?Node) !int {
	result := if value := node {
		-(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

fn select_value_index(node ?Node) !int {
	values := [10, 20, 30]
	result := if value := node {
		values[match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}]
	} else {
		0
	}
	return result
}

fn select_value_slice_bound(node ?Node) ![]int {
	values := [10, 20, 30, 40]
	result := if value := node {
		values[(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})..]
	} else {
		[0]
	}
	return result
}

fn select_value_membership(node ?Node) !bool {
	result := if value := node {
		(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}) in [1, 2]
	} else {
		false
	}
	return result
}

fn select_value_mapkey(node ?Node) !map[int]int {
	result := if value := node {
		{(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}): 100}
	} else {
		{0: 100}
	}
	return result
}

fn select_value_interp(node ?Node) !string {
	result := if value := node {
		"x=\${match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}}"
	} else {
		"x=0"
	}
	return result
}

fn bool_first(_ First) !bool {
	return true
}

fn bool_second(_ Second) !bool {
	return false
}

fn select_value_likely(node ?Node) !bool {
	result := if value := node {
		_likely_(match value {
			First { bool_first(value)! }
			Second { bool_second(value)! }
		})
	} else {
		false
	}
	return result
}

struct Boxed {
	value int
}

fn boxed(v int) Boxed {
	return Boxed{v}
}

fn select_value_selector(node ?Node) !int {
	result := if value := node {
		(match value {
			First { boxed(lower_first(value)!) }
			Second { boxed(lower_second(value)!) }
		}).value
	} else {
		0
	}
	return result
}

struct Holder {
	value int
	other int
}

fn select_value_mapinit(node ?Node) !map[int]int {
	result := if value := node {
		{
			7: match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		}
	} else {
		{
			7: 0
		}
	}
	return result
}

fn select_value_structinit(node ?Node) !Holder {
	result := if value := node {
		Holder{
			value: match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
			other: 100
		}
	} else {
		Holder{}
	}
	return result
}

struct Circle {
	r int
}

struct Square {
	s int
}

type Shape = Circle | Square

fn make_circle(r int) !Shape {
	return Circle{r}
}

fn select_value_ascast(node ?int) !int {
	shape := if v := node {
		(match v {
			0 { make_circle(v)! }
			else { make_circle(v + 1)! }
		}) as Circle
	} else {
		Circle{99}
	}
	return shape.r
}

fn select_value_ascast_unsafe(node ?int) !int {
	shape := if v := node {
		(unsafe {
			match v {
				0 { make_circle(v)! }
				else { make_circle(v + 1)! }
			}
		}) as Circle
	} else {
		Circle{99}
	}
	return shape.r
}

fn direct_match(node Node) !int {
	return match node {
		First { lower_first(node)! }
		Second { lower_second(node)! }
	}
}

struct Tracer {
mut:
	order []int
}

fn (mut tr Tracer) lhs() int {
	tr.order << 1
	return 1
}

fn (mut tr Tracer) rf(_ First) !int {
	tr.order << 2
	return 10
}

fn (mut tr Tracer) rs(_ Second) !int {
	tr.order << 2
	return 20
}

// The LHS call must be evaluated before the RHS match materialization prelude.
// Encodes sum (11) and the recorded order ([1,2] = LHS then RHS) as 1112; a
// reversed order would yield 1121.
fn select_value_infix_order(node ?Node) !int {
	mut tr := Tracer{}
	sum := if value := node {
		tr.lhs() + (match value {
			First { tr.rf(value)! }
			Second { tr.rs(value)! }
		})
	} else {
		0
	}
	return sum * 100 + tr.order[0] * 10 + tr.order[1]
}

// Infix ordering with a *nested* branch: the match is buried inside a compound RHS
// (`lhs() + (1 + (match ...))`), whose inner infix still hoists the match prelude. The
// side-effecting LHS must run before that prelude. First -> 1 + (1 + 10) = 12, order [1,2]
// -> 1212 (a reversed order would be 1221).
fn select_value_nested_infix_order(node Node) !int {
	mut tr := Tracer{}
	sum := tr.lhs() + (1 + (match node {
		First { tr.rf(node)! }
		Second { tr.rs(node)! }
	}))
	return sum * 100 + tr.order[0] * 10 + tr.order[1]
}

fn (mut tr Tracer) shift_lhs() int {
	tr.order << 1
	return 1
}

// Left-shift ordering: the side-effecting LHS must run before the RHS match
// prelude. Encodes shift result (1 << 10 = 1024) and order ([1,2]) as 102412; a
// reversed order would be 102421.
fn select_value_shift_order(node Node) !int {
	mut tr := Tracer{}
	sum := tr.shift_lhs() << (match node {
		First { tr.rf(node)! }
		Second { tr.rs(node)! }
	})
	return sum * 100 + tr.order[0] * 10 + tr.order[1]
}

// Left-shift ordering with a *nested* RHS branch: the match is buried inside a compound
// shift RHS (`shift_lhs() << (1 + (match ...))`), which still hoists the match prelude. The
// side-effecting LHS must run before it. First -> 1 << (1 + 10) = 1 << 11 = 2048, order
// [1,2] -> 204812 (a reversed order would be 204821).
fn select_value_nested_shift_order(node Node) !int {
	mut tr := Tracer{}
	sum := tr.shift_lhs() << (1 + (match node {
		First { tr.rf(node)! }
		Second { tr.rs(node)! }
	}))
	return sum * 100 + tr.order[0] * 10 + tr.order[1]
}

fn (mut tr Tracer) base_values() []int {
	tr.order << 1
	return [10, 20, 30]
}

fn (mut tr Tracer) idx_first(_ First) !int {
	tr.order << 2
	return 0
}

fn (mut tr Tracer) idx_second(_ Second) !int {
	tr.order << 2
	return 1
}

// Index ordering: the side-effecting base must run before the match-index prelude.
// Encodes indexed value ([10,20,30][0] = 10) and order ([1,2]) as 1012; a reversed
// order would be 1021.
fn select_value_index_order(node Node) !int {
	mut tr := Tracer{}
	val := tr.base_values()[match node {
		First { tr.idx_first(node)! }
		Second { tr.idx_second(node)! }
	}]
	return val * 100 + tr.order[0] * 10 + tr.order[1]
}

// Index ordering with a *nested* branch: the match is buried inside a compound index
// (`base[1 + (match ...)]`), whose infix lowering still hoists the match prelude. The
// side-effecting base must run before that prelude. First -> base_values()[1 + 0] = 20,
// order [1,2] -> 2012 (a reversed order would be 2021).
fn select_value_nested_index_order(node Node) !int {
	mut tr := Tracer{}
	val := tr.base_values()[1 + (match node {
		First { tr.idx_first(node)! }
		Second { tr.idx_second(node)! }
	})]
	return val * 100 + tr.order[0] * 10 + tr.order[1]
}

fn (mut tr Tracer) gated_first(_ First) !int {
	tr.order << 2
	return -1
}

fn (mut tr Tracer) gated_second(_ Second) !int {
	tr.order << 2
	return -2
}

// Gated index (`#[]`) with a propagating value match: the match tail must be
// lowered as a value (the gated helper otherwise lowers it with plain
// `transform_expr`). Encodes the gated value ([10,20,30]#[-1] = 30) and order
// ([1,2]) as 3012.
fn select_value_gated_index_order(node Node) !int {
	mut tr := Tracer{}
	val := tr.base_values()#[match node {
		First { tr.gated_first(node)! }
		Second { tr.gated_second(node)! }
	}]
	return val * 100 + tr.order[0] * 10 + tr.order[1]
}

// A propagating value match as the low bound of a `for in` range loop: the bound
// must be lowered as a value. First -> low 1, sum of 1..4 = 6.
fn select_value_range_low(node Node) !int {
	mut sum := 0
	for i in (match node {
		First { lower_first(node)! }
		Second { lower_second(node)! }
	}) .. 4 {
		sum += i
	}
	return sum
}

// A propagating value match as the low bound of an `x in low..high` membership
// test. First -> low 1, so `3 in 1..4` is true.
fn select_value_range_membership(node Node) !bool {
	return 3 in (match node {
		First { lower_first(node)! }
		Second { lower_second(node)! }
	}) .. 4
}

fn (mut tr Tracer) range_low() int {
	tr.order << 1
	return 0
}

fn (mut tr Tracer) range_high_first(_ First) !int {
	tr.order << 2
	return 10
}

fn (mut tr Tracer) range_high_second(_ Second) !int {
	tr.order << 2
	return 20
}

// Membership range ordering: a side-effecting low bound must run before the
// hoisting match high bound. `5 in 0..10` is true; order [1,2] -> 112 (a reversed
// order would be 121).
fn select_value_range_order(node Node) !int {
	mut tr := Tracer{}
	inside := 5 in tr.range_low() .. (match node {
		First { tr.range_high_first(node)! }
		Second { tr.range_high_second(node)! }
	})
	flag := if inside { 1 } else { 0 }
	return flag * 100 + tr.order[0] * 10 + tr.order[1]
}

// Membership-range ordering with a *nested* high bound: the match is buried inside a
// compound high bound (`.. (1 + (match ...))`), which still hoists the match prelude. The
// side-effecting low bound must run before it. First -> `5 in 0 .. (1 + 10)` = true, order
// [1,2] -> 112 (a reversed order would be 121).
fn select_value_nested_range_order(node Node) !int {
	mut tr := Tracer{}
	inside := 5 in tr.range_low() .. (1 + (match node {
		First { tr.range_high_first(node)! }
		Second { tr.range_high_second(node)! }
	}))
	flag := if inside { 1 } else { 0 }
	return flag * 100 + tr.order[0] * 10 + tr.order[1]
}

fn make_values_first(_ First) ![]int {
	return [10, 20, 30]
}

fn make_values_second(_ Second) ![]int {
	return [40, 50]
}

// Membership over a value-match container (dynamic array): the propagating match
// tail must be lowered as a value. First -> [10,20,30], so `20 in ...` is true.
fn select_value_membership_container(node Node) !bool {
	return 20 in (match node {
		First { make_values_first(node)! }
		Second { make_values_second(node)! }
	})
}

// for-in over a value-match container: the propagating match tail must be lowered
// as a value. First -> [10,20,30] -> sum 60.
fn select_value_forin_container(node Node) !int {
	mut sum := 0
	for v in (match node {
		First { make_values_first(node)! }
		Second { make_values_second(node)! }
	}) {
		sum += v
	}
	return sum
}

fn make_map_first(_ First) !map[string]int {
	return {
		"a": 1
		"b": 2
	}
}

fn make_map_second(_ Second) !map[string]int {
	return {
		"a": 3
	}
}

// Map index whose base is a value match: the propagating arm tail must be lowered
// as a value. First -> {a:1, b:2}, so ["b"] is 2.
fn select_value_map_index(node Node) !int {
	return (match node {
		First { make_map_first(node)! }
		Second { make_map_second(node)! }
	})["b"]
}

fn (mut tr Tracer) needle_str() string {
	tr.order << 1
	return "lo"
}

fn (mut tr Tracer) text_first(_ First) !string {
	tr.order << 2
	return "hello"
}

fn (mut tr Tracer) text_second(_ Second) !string {
	tr.order << 2
	return "world"
}

// String-membership ordering: the side-effecting needle must run before the
// hoisting match container. "lo" in "hello" is true; order [1,2] -> 512 (a
// reversed order would be 521).
fn select_value_string_membership_order(node Node) !int {
	mut tr := Tracer{}
	inside := tr.needle_str() in (match node {
		First { tr.text_first(node)! }
		Second { tr.text_second(node)! }
	})
	return (if inside { 500 } else { 0 }) + tr.order[0] * 10 + tr.order[1]
}

fn wrap_str(s string) string {
	return s
}

// String-membership ordering with a *nested* container: the match is wrapped inside a call
// (`needle in wrap_str(match ...)`), which still hoists the match prelude. The side-effecting
// needle must run before it. First -> "lo" in "hello" = true, order [1,2] -> 512 (a reversed
// order would be 521).
fn select_value_nested_string_membership_order(node Node) !int {
	mut tr := Tracer{}
	inside := tr.needle_str() in wrap_str(match node {
		First { tr.text_first(node)! }
		Second { tr.text_second(node)! }
	})
	return (if inside { 500 } else { 0 }) + tr.order[0] * 10 + tr.order[1]
}

fn (mut tr Tracer) next_index() int {
	tr.order << 1
	return 0
}

fn (mut tr Tracer) append_val_first(_ First) !int {
	tr.order << 2
	return 7
}

fn (mut tr Tracer) append_val_second(_ Second) !int {
	tr.order << 2
	return 8
}

// Array-append ordering: a side-effecting LHS index must run before the RHS match
// prelude, while the mutated array is not spilled. First -> arrays[0] << 7, order
// [1,2] -> 712 (a reversed order would be 721).
fn select_value_append_order(node Node) !int {
	mut tr := Tracer{}
	mut arrays := [[]int{}, []int{}]
	arrays[tr.next_index()] << (match node {
		First { tr.append_val_first(node)! }
		Second { tr.append_val_second(node)! }
	})
	return arrays[0][0] * 100 + tr.order[0] * 10 + tr.order[1]
}

fn wrap_append(x int) int {
	return x + 1
}

// Array-append ordering with a *nested* RHS branch: the match is wrapped inside a call
// (`arrays[i] << wrap_append(match ...)`), which still hoists the match prelude. The
// side-effecting LHS index must run before that prelude while the array is not spilled.
// First -> arrays[0] << 8, order [1,2] -> 812 (a reversed order would be 821).
fn select_value_nested_append_order(node Node) !int {
	mut tr := Tracer{}
	mut arrays := [[]int{}, []int{}]
	arrays[tr.next_index()] << wrap_append(match node {
		First { tr.append_val_first(node)! }
		Second { tr.append_val_second(node)! }
	})
	return arrays[0][0] * 100 + tr.order[0] * 10 + tr.order[1]
}

const allowed_words = ["alpha", "beta"]

fn get_first(_ First) !string {
	return "alpha"
}

fn get_second(_ Second) !string {
	return "gamma"
}

// Value-match needle in a constant string array (membership shortcut): the
// propagating arm tail must be lowered as a value. First -> "alpha" in
// ["alpha", "beta"] -> true.
fn select_value_const_membership(node Node) !bool {
	return (match node {
		First { get_first(node)! }
		Second { get_second(node)! }
	}) in allowed_words
}

fn (mut tr Tracer) map_key() string {
	tr.order << 1
	return "b"
}

fn (mut tr Tracer) map_first(_ First) !map[string]int {
	tr.order << 2
	return {
		"a": 1
		"b": 2
	}
}

fn (mut tr Tracer) map_second(_ Second) !map[string]int {
	tr.order << 2
	return {
		"a": 3
	}
}

// Map-membership ordering: a side-effecting key must run before the hoisting match
// container. `"b" in {a:1, b:2}` is true; order [1,2] -> 612 (a reversed order
// would be 621).
fn select_value_map_membership_order(node Node) !int {
	mut tr := Tracer{}
	inside := tr.map_key() in (match node {
		First { tr.map_first(node)! }
		Second { tr.map_second(node)! }
	})
	return (if inside { 600 } else { 0 }) + tr.order[0] * 10 + tr.order[1]
}

// Push-many append whose RHS is a value match producing an array: the propagating
// arm tail must be lowered as a value. First -> [1] << [10,20,30] -> sum 61.
fn select_value_push_many(node Node) !int {
	mut out := [1]
	out << (match node {
		First { make_values_first(node)! }
		Second { make_values_second(node)! }
	})
	mut sum := 0
	for v in out {
		sum += v
	}
	return sum
}

// Builtin method call on a value match receiver: the receiver must be materialized
// as a value before builtin dispatch (`.clone()` -> make_array_clone_call). First ->
// [10,20,30].clone() -> sum 60 + len 3 = 63.
fn select_value_method_receiver(node Node) !int {
	cloned := (match node {
		First { make_values_first(node)! }
		Second { make_values_second(node)! }
	}).clone()
	mut sum := 0
	for v in cloned {
		sum += v
	}
	return sum + cloned.len
}

fn idx_needle_first(_ First) !int {
	return 20
}

fn idx_needle_second(_ Second) !int {
	return 30
}

// Builtin method argument that is a value match: the needle must be materialized as
// a value before builtin dispatch (`values.index(match ...)` -> lower_array_index_expr).
// First -> index of 20 in [10,20,30] = 1, encoded as 1*1000 + len 3 = 1003.
fn select_value_method_arg(node Node) !int {
	values := [10, 20, 30]
	idx := values.index(match node {
		First { idx_needle_first(node)! }
		Second { idx_needle_second(node)! }
	})
	return idx * 1000 + values.len
}

fn ch_first(_ First) !int {
	return 42
}

fn ch_second(_ Second) !int {
	return 43
}

// Channel send with an `or {}` handler whose sent value is a value match: the
// propagating arm tail must be lowered as a value inside the `.arrow` fast path.
// First -> send 42, receive 42.
fn select_value_channel_send(node Node) !int {
	ch := chan int{cap: 1}
	ch <- (match node {
		First { ch_first(node)! }
		Second { ch_second(node)! }
	}) or { return -1 }
	return <-ch
}

fn (mut tr Tracer) make_index_values() []int {
	tr.order << 1
	return [10, 20, 30]
}

fn (mut tr Tracer) needle_first(_ First) !int {
	tr.order << 2
	return 30
}

fn (mut tr Tracer) needle_second(_ Second) !int {
	tr.order << 2
	return 20
}

// Call operand ordering: a side-effecting receiver must run before the branch
// argument hoisted prelude. First -> index of 30 in [10,20,30] = 2, order [1,2]
// -> 2012 (a reversed order would be 2021).
fn select_value_call_operand_order(node Node) !int {
	mut tr := Tracer{}
	idx := tr.make_index_values().index(match node {
		First { tr.needle_first(node)! }
		Second { tr.needle_second(node)! }
	})
	return idx * 1000 + tr.order[0] * 10 + tr.order[1]
}

struct MutItem {
mut:
	v int
}

fn (mut m MutItem) add(x int) {
	m.v += x
}

fn (mut tr Tracer) pick_index() int {
	tr.order << 1
	return 0
}

fn (mut tr Tracer) add_first(_ First) !int {
	tr.order << 2
	return 5
}

fn (mut tr Tracer) add_second(_ Second) !int {
	tr.order << 2
	return 7
}

// Mutable receiver with a value-match argument: the receiver lvalue identity must
// be preserved (only its index is stabilized), so the mutation reaches items[0], and
// the index runs before the argument prelude. First -> items[0].v = 40 + 5 = 45,
// order [1,2] -> 4512 (mutation lost would be 4012; reversed order 4521).
fn select_value_mut_receiver(node Node) !int {
	mut tr := Tracer{}
	mut items := [MutItem{
		v: 40
	}, MutItem{
		v: 50
	}]
	items[tr.pick_index()].add(match node {
		First { tr.add_first(node)! }
		Second { tr.add_second(node)! }
	})
	return items[0].v * 100 + tr.order[0] * 10 + tr.order[1]
}

struct ValItem {
	v int
}

fn (vi ValItem) read(x int) int {
	return vi.v * 1000 + x
}

struct ValHolder {
mut:
	items []ValItem
}

fn (mut vh ValHolder) at() int {
	return 0
}

fn (mut vh ValHolder) overwrite_first(_ First) !int {
	vh.items[0] = ValItem{
		v: 9
	}
	return 2
}

fn (mut vh ValHolder) overwrite_second(_ Second) !int {
	vh.items[0] = ValItem{
		v: 8
	}
	return 3
}

// A non-mut *value*-receiver method called on an lvalue element, with a later argument whose
// match arm mutates that element: the receiver value must be read in source order (before the
// mutation), not reloaded after the branch prelude. First -> items[0] (ValItem{5}).read(2) =
// 5 * 1000 + 2 = 5002 (if the mutation leaked, the reloaded receiver would give 9002).
fn select_value_value_receiver(node Node) !int {
	mut vh := ValHolder{
		items: [ValItem{
			v: 5
		}, ValItem{
			v: 6
		}]
	}
	return vh.items[vh.at()].read(match node {
		First { vh.overwrite_first(node)! }
		Second { vh.overwrite_second(node)! }
	})
}

fn (mut tr Tracer) ch_index() int {
	tr.order << 1
	return 0
}

fn (mut tr Tracer) ch_rhs_first(_ First) !int {
	tr.order << 2
	return 99
}

fn (mut tr Tracer) ch_rhs_second(_ Second) !int {
	tr.order << 2
	return 88
}

// Channel target with a side-effecting index and a value-match sent value: the
// target index must run before the sent value prelude. First -> send 99, order [1,2]
// -> 9912 (reversed order 9921).
fn select_value_channel_target_order(node Node) !int {
	mut tr := Tracer{}
	mut channels := [chan int{cap: 1}, chan int{cap: 1}]
	channels[tr.ch_index()] <- (match node {
		First { tr.ch_rhs_first(node)! }
		Second { tr.ch_rhs_second(node)! }
	}) or { return -1 }
	got := <-channels[0]
	return got * 100 + tr.order[0] * 10 + tr.order[1]
}

fn wrap_send(x int) int {
	return x
}

// Channel-send ordering with a *nested* sent value: the match is wrapped inside a call
// (`channels[i] <- wrap_send(match ...) or {}`), which still hoists the match prelude. The
// side-effecting target index must run before it. First -> send 99, order [1,2] -> 9912 (a
// reversed order would be 9921).
fn select_value_nested_channel_target_order(node Node) !int {
	mut tr := Tracer{}
	mut channels := [chan int{cap: 1}, chan int{cap: 1}]
	channels[tr.ch_index()] <- wrap_send(match node {
		First { tr.ch_rhs_first(node)! }
		Second { tr.ch_rhs_second(node)! }
	}) or { return -1 }
	got := <-channels[0]
	return got * 100 + tr.order[0] * 10 + tr.order[1]
}

struct ChanHolder {
mut:
	order []int
	ch    chan int
}

fn (mut c ChanHolder) get_channel() chan int {
	c.order << 1
	return c.ch
}

fn (mut c ChanHolder) sent_first(_ First) !int {
	c.order << 2
	return 77
}

fn (mut c ChanHolder) sent_second(_ Second) !int {
	c.order << 2
	return 66
}

// A side-effecting *rvalue* channel target (a method call, not an lvalue) with a value-match
// sent value: the target call must run before the hoisted prelude of the sent value. Since it
// is not an lvalue shape it is spilled by value. First -> send 77, order [1,2] -> 7712 (a
// reversed order would be 7721).
fn select_value_rvalue_channel_target(node Node) !int {
	mut c := ChanHolder{
		ch: chan int{cap: 1}
	}
	c.get_channel() <- (match node {
		First { c.sent_first(node)! }
		Second { c.sent_second(node)! }
	}) or { return -1 }
	got := <-c.ch
	return got * 100 + c.order[0] * 10 + c.order[1]
}

struct ChanFactory {
mut:
	order []int
	chans []chan int
}

fn (mut f ChanFactory) make_channels() []chan int {
	f.order << 1
	return f.chans
}

fn (mut f ChanFactory) fsent_first(_ First) !int {
	f.order << 2
	return 55
}

fn (mut f ChanFactory) fsent_second(_ Second) !int {
	f.order << 2
	return 44
}

// A channel target that is an index into a side-effecting *rvalue* base (`make_channels()[0]`,
// not an lvalue): the lvalue stabilizer rebuilds the outer index but leaves the base call
// inline, so the whole target must be spilled by value to keep the base call ahead of the sent
// value prelude. First -> send 55, order [1,2] -> 5512 (a reversed order would be 5521).
fn select_value_composite_rvalue_channel_target(node Node) !int {
	mut f := ChanFactory{
		chans: [chan int{cap: 1}, chan int{cap: 1}]
	}
	f.make_channels()[0] <- (match node {
		First { f.fsent_first(node)! }
		Second { f.fsent_second(node)! }
	}) or { return -1 }
	got := <-f.chans[0]
	return got * 100 + f.order[0] * 10 + f.order[1]
}

struct ChanPick {
mut:
	ch1 chan int
	ch2 chan int
}

fn (mut c ChanPick) pick_first(_ First) !chan int {
	return c.ch1
}

fn (mut c ChanPick) pick_second(_ Second) !chan int {
	return c.ch2
}

// The channel target is itself a value match producing a channel: it must be materialized as a
// value, not lowered as an empty channel. First -> send 7 to ch1 -> received 7.
fn select_value_branch_channel_target(node Node) !int {
	mut c := ChanPick{
		ch1: chan int{cap: 1}
		ch2: chan int{cap: 1}
	}
	(match node {
		First { c.pick_first(node)! }
		Second { c.pick_second(node)! }
	}) <- 7 or { return -1 }
	return <-c.ch1
}

struct ChanReassign {
mut:
	ch1    chan int
	ch2    chan int
	target chan int
}

fn (mut c ChanReassign) retarget(_ First) !int {
	c.target = c.ch2
	return 7
}

// A stable channel target whose variable the sent-value branch reassigns: the value must be
// sent to the channel evaluated before the RHS, not the replacement. First -> 7 lands on the
// original target ch1 -> 7 (a leaked reassignment sends to ch2 -> 107).
fn select_value_channel_target_reassign(node Node) !int {
	mut c := ChanReassign{
		ch1: chan int{cap: 1}
		ch2: chan int{cap: 1}
	}
	c.target = c.ch1
	c.target <- (match node {
		First { c.retarget(node)! }
		Second { 1 }
	}) or { return -1 }
	return if c.ch1.len == 1 {
		<-c.ch1
	} else if c.ch2.len == 1 {
		100 + <-c.ch2
	} else {
		-1
	}
}

struct Counter {
mut:
	v int
}

fn (mut c Counter) bump() !int {
	c.v = 100
	return 5
}

fn (mut c Counter) arr_first(_ First) ![]int {
	c.v = 100
	return [1, 5, 9]
}

fn (mut c Counter) arr_second(_ Second) ![]int {
	c.v = 200
	return [2, 6]
}

// A syntactically stable needle (a struct-field read) whose value the prelude of the
// value-branch container mutates: the membership loop must read the needle in source order,
// before the mutation. First -> c.v (5) in [1, 5, 9] -> true (a leaked mutation to 100 gives
// false).
fn select_value_membership_needle_snapshot(node Node) !bool {
	mut c := Counter{
		v: 5
	}
	return c.v in (match node {
		First { c.arr_first(node)! }
		Second { c.arr_second(node)! }
	})
}

// A syntactically stable LHS lvalue (a struct-field read) whose value the RHS branch prelude
// mutates: the infix must read the LHS source-order value, not the updated value. First ->
// c.v (1) + 5 = 6, then c.v is 100 -> 6 * 1000 + 100 = 6100 (a leaked mutation gives 105100).
fn select_value_stable_lhs_snapshot(node Node) !int {
	mut c := Counter{
		v: 1
	}
	y := c.v + (match node {
		First { c.bump()! }
		Second { c.bump()! }
	})
	return y * 1000 + c.v
}

fn take2(a int, b int) int {
	return a * 1000 + b
}

// A stable field-read argument whose value the prelude of a later value-branch argument
// mutates: the argument must be read in source order. First -> take2(1, 5) = 1005 (a leaked
// mutation into the first argument gives 100005).
fn select_value_stable_arg_snapshot(node Node) !int {
	mut c := Counter{
		v: 1
	}
	return take2(c.v, match node {
		First { c.bump()! }
		Second { c.bump()! }
	})
}

fn (mut c Counter) rng_hi_first(_ First) !int {
	c.v = 100
	return 3
}

fn (mut c Counter) rng_hi_second(_ Second) !int {
	c.v = 200
	return 5
}

// A stable range low bound whose value the high-bound branch prelude mutates: the loop must
// start at the source-order low, not the mutated value. First -> `for i in 1 .. 3` sums 1+2 =
// 3 (a leaked mutation of the low bound to 100 gives an empty range and sum 0).
fn select_value_range_low_snapshot(node Node) !int {
	mut c := Counter{
		v: 1
	}
	mut sum := 0
	for i in c.v .. (match node {
		First { c.rng_hi_first(node)! }
		Second { c.rng_hi_second(node)! }
	}) {
		sum += i
	}
	return sum
}

fn replace_map(mut m map[string]int) !string {
	m = {
		"x": 999
	}
	return "x"
}

// A stable map base whose variable the key branch prelude reassigns: the lookup must use the
// map evaluated before the key, not the replacement. First -> original items["x"] = 5 (a
// leaked reassignment gives the replacement value 999).
fn select_value_map_base_snapshot(node Node) !int {
	mut items := {
		"x": 5
		"y": 7
	}
	return items[match node {
		First { replace_map(mut items)! }
		Second { "y" }
	}]
}

fn replace_arr(mut a []int) !int {
	a = [100, 200, 300]
	return 0
}

// A stable gated-index base whose variable the index branch prelude reassigns: the gated access
// must index the array evaluated before the index, not the replacement. First -> original
// values#[0] = 5 (a leaked reassignment indexes [100, 200, 300] -> 100).
fn select_value_gated_base_snapshot(node Node) !int {
	mut values := [5, 6, 7]
	return values#[match node {
		First { replace_arr(mut values)! }
		Second { 1 }
	}]
}

fn (mut c Counter) idx_bump_first(_ First) !int {
	c.v = 1
	return 5
}

fn (mut c Counter) idx_bump_second(_ Second) !int {
	c.v = 1
	return 6
}

// A mutable receiver whose index (a stable field read) the value-branch argument mutates: the
// index value must be captured in source order, so the method updates the original element.
// First -> items[c.v=0].add(5) -> items[0].v = 45 (a mutated index updates items[1] -> 4055).
fn select_value_mut_receiver_index_snapshot(node Node) !int {
	mut c := Counter{
		v: 0
	}
	mut items := [MutItem{
		v: 40
	}, MutItem{
		v: 50
	}]
	items[c.v].add(match node {
		First { c.idx_bump_first(node)! }
		Second { c.idx_bump_second(node)! }
	})
	return items[0].v * 100 + items[1].v
}

struct PtrObj {
mut:
	v int
}

fn (mut o PtrObj) add(x int) {
	o.v += x
}

struct PtrHolder {
mut:
	ptr &PtrObj
}

fn (mut h PtrHolder) retarget_first(_ First) !int {
	h.ptr = &PtrObj{
		v: 1000
	}
	return 5
}

fn (mut h PtrHolder) retarget_second(_ Second) !int {
	h.ptr = &PtrObj{
		v: 2000
	}
	return 6
}

// A pointer-field mutable receiver whose pointer a value-branch argument reassigns: the method
// must mutate the object selected in source order, not the replacement. First -> orig.v = 15,
// then h.ptr points to the replacement (v 1000) -> 15 * 1000 + 1000 = 16000 (a retargeted call
// gives 11005).
fn select_value_pointer_receiver_capture(node Node) !int {
	orig := &PtrObj{
		v: 10
	}
	mut h := PtrHolder{
		ptr: orig
	}
	h.ptr.add(match node {
		First { h.retarget_first(node)! }
		Second { h.retarget_second(node)! }
	})
	return orig.v * 1000 + h.ptr.v
}

fn replace_cells(mut a []PtrObj) !int {
	a = [PtrObj{
		v: 1000
	}, PtrObj{
		v: 2000
	}]
	return 5
}

// An indexed mutable receiver whose array base a value-branch argument replaces: the method
// must mutate the element in the source-order array. First -> orig[0].v = 15, items reassigned
// -> orig[0].v * 100 + items[0].v = 1500 + 1000 = 2500 (a retargeted call gives 2005).
fn select_value_array_base_capture(node Node) !int {
	mut items := [PtrObj{
		v: 10
	}, PtrObj{
		v: 20
	}]
	orig := items
	items[0].add(match node {
		First { replace_cells(mut items)! }
		Second { 0 }
	})
	return orig[0].v * 100 + items[0].v
}

fn (mut tr Tracer) sfirst() int {
	tr.order << 1
	return 3
}

fn (mut tr Tracer) ssecond(_ First) !int {
	tr.order << 2
	return 4
}

struct SPair {
	a int
	b int
}

// A later struct field whose value is a nested block/if with a propagating match tail hoists a
// prelude; earlier field values must be snapshotted so fields evaluate in source order. First
// -> a=3, b=4, order [1,2] -> 3 * 1000 + 4 * 100 + 12 = 3412 (a reversed order gives 3421).
fn select_value_struct_field_order(node Node) !int {
	mut tr := Tracer{}
	cond := true
	p := SPair{
		a: tr.sfirst()
		b: if cond {
			match node {
				First { tr.ssecond(node)! }
				Second { 0 }
			}
		} else {
			0
		}
	}
	return p.a * 1000 + p.b * 100 + tr.order[0] * 10 + tr.order[1]
}

struct OptHolder {
mut:
	values ?[]int
}

fn (mut h OptHolder) replace_first(_ First) ![]int {
	h.values = [100, 200]
	return [7, 8]
}

// A push-many optional-LHS append whose value-branch RHS reassigns the optional source: the
// append targets the value-array storage selected before the RHS (captured up front), matching
// mainline. First -> [100, 200] << [7, 8] = [100, 200, 7, 8], len 4, first 100 -> 4 * 100 + 100
// = 500.
fn select_value_optional_append_reassign(node Node) !int {
	mut h := OptHolder{
		values: [1, 2]
	}
	h.values or { return error("none") } << (match node {
		First { h.replace_first(node)! }
		Second { [9] }
	})
	got := h.values or { []int{} }
	return got.len * 100 + got[0]
}

fn (mut tr Tracer) sel_first() int {
	tr.order << 1
	return 3
}

fn (mut tr Tracer) sel_second(_ First) !int {
	tr.order << 2
	return 4
}

// A select whose later send-case value is a value branch: all case values are evaluated during
// select setup in source order, so the branch prelude must not be drained before the whole
// select. First -> ch1 <- sel_first (order 1) then ch2 <- (match -> sel_second) (order 2) -> 12
// (a reversed order would be 21).
fn select_value_select_case_order(node Node) !int {
	mut tr := Tracer{}
	ch1 := chan int{cap: 1}
	ch2 := chan int{cap: 1}
	select {
		ch1 <- tr.sel_first() {}
		ch2 <- (match node {
			First { tr.sel_second(node)! }
			Second { 0 }
		}) {}
	}
	return tr.order[0] * 10 + tr.order[1]
}

fn (mut c Counter) sel_change(_ First) !int {
	c.v = 100
	return 0
}

// A stable value operand in an earlier select send case that a later branch prelude mutates: it
// must be captured in source order, before the prelude. First -> ch1 gets c.v in source order
// (5), not the mutated 100 (an unbuffered ch2 with no receiver forces case 1).
fn select_value_select_stable_operand(node Node) !int {
	mut c := Counter{
		v: 5
	}
	ch1 := chan int{cap: 1}
	ch2 := chan int{}
	select {
		ch1 <- c.v {}
		ch2 <- (match node {
			First { c.sel_change(node)! }
			Second { 0 }
		}) {}
	}
	return <-ch1
}

struct ChanCtx {
mut:
	order []int
	ch2   chan int
}

fn (mut c ChanCtx) c_first() int {
	c.order << 1
	return 3
}

fn (mut c ChanCtx) c_make(_ First) !chan int {
	c.order << 2
	return c.ch2
}

// A later select case whose channel is a value branch: case operands must evaluate in source
// order during select setup. First -> c_first (order 1) then the channel match -> c_make (order
// 2) -> 12 (a reversed order would be 21).
fn select_value_select_branch_channel(node Node) !int {
	mut c := ChanCtx{
		ch2: chan int{cap: 1}
	}
	ch1 := chan int{cap: 1}
	select {
		ch1 <- c.c_first() {}
		(match node {
			First { c.c_make(node)! }
			Second { c.ch2 }
		}) <- 1 {}
	}
	return c.order[0] * 10 + c.order[1]
}

type IntFn = fn () int

struct FnBox {
	f IntFn
}

fn cb_a() int {
	return 41
}

fn cb_b() int {
	return 52
}

fn make_cb_first(_ First) !FnBox {
	return FnBox{
		f: cb_a
	}
}

fn make_cb_second(_ Second) !FnBox {
	return FnBox{
		f: cb_b
	}
}

// The call target itself is a value match (with propagating arms) producing a function value,
// immediately invoked: child 0 (the callee) must be materialized as a value, not lowered with
// plain transform_expr into an empty callee. First -> cb_a() = 41.
fn select_value_branch_callee(node Node) !int {
	return (match node {
		First { make_cb_first(node)!.f }
		Second { make_cb_second(node)!.f }
	})()
}

type ArgFn = fn (int) int

fn adder(x int) int {
	return x
}

fn make_adder_cb(mut tr Tracer) ArgFn {
	tr.order << 1
	return adder
}

fn (mut tr Tracer) cbarg_first(_ First) !int {
	tr.order << 2
	return 7
}

fn (mut tr Tracer) cbarg_second(_ Second) !int {
	tr.order << 2
	return 9
}

// A non-method runtime callee (a call returning a function value) with a value-match argument:
// the callee must evaluate before the argument prelude. First -> make_adder_cb (order 1) then
// arg (order 2), adder(7) = 7 -> 712 (a reversed order would be 721).
fn select_value_runtime_callee_order(node Node) !int {
	mut tr := Tracer{}
	r := make_adder_cb(mut tr)(match node {
		First { tr.cbarg_first(node)! }
		Second { tr.cbarg_second(node)! }
	})
	return r * 100 + tr.order[0] * 10 + tr.order[1]
}

fn cb_orig(x int) int {
	return x * 10
}

fn cb_new(x int) int {
	return x * 100
}

struct CbHolder {
mut:
	callback ArgFn
}

fn (mut h CbHolder) install_new(_ First) !int {
	h.callback = cb_new
	return 3
}

// The callee is a function-valued field on a reference-backed holder; the value-match argument
// prelude replaces the field. The call must invoke the callback evaluated before the arguments
// (snapshotting the whole callee), not the replacement. First -> cb_orig(3) = 30 (a replaced
// field would invoke cb_new(3) = 300).
fn select_value_fn_field_callee_order(node Node) !int {
	mut p := &CbHolder{
		callback: cb_orig
	}
	return p.callback(match node {
		First { p.install_new(node)! }
		Second { 3 }
	})
}

fn combine(a int, b int) int {
	return a * 10 + b
}

fn (mut tr Tracer) first_arg() int {
	tr.order << 1
	return 3
}

fn (mut tr Tracer) second_arg_first(_ First) !int {
	tr.order << 2
	return 4
}

fn (mut tr Tracer) second_arg_second(_ Second) !int {
	tr.order << 2
	return 5
}

// Plain function call ordering: a side-effecting first argument must run before the
// value-match second argument prelude. First -> combine(3, 4) = 34, order [1,2] -> 3412
// (a reversed order would be 3421).
fn select_value_plain_call_order(node Node) !int {
	mut tr := Tracer{}
	r := combine(tr.first_arg(), match node {
		First { tr.second_arg_first(node)! }
		Second { tr.second_arg_second(node)! }
	})
	return r * 100 + tr.order[0] * 10 + tr.order[1]
}

// Plain call ordering with a *nested* value branch: the match is buried inside a compound
// second argument (`1 + (match ...)`), which still materializes the branch prelude into
// pending_stmts. The side-effecting first argument must run before that prelude.
// First -> combine(3, 1 + 4) = combine(3, 5) = 35, order [1,2] -> 3512
// (a reversed order, the match prelude before first_arg, would be 3521).
fn select_value_nested_branch_arg_order(node Node) !int {
	mut tr := Tracer{}
	r := combine(tr.first_arg(), 1 + (match node {
		First { tr.second_arg_first(node)! }
		Second { tr.second_arg_second(node)! }
	}))
	return r * 100 + tr.order[0] * 10 + tr.order[1]
}

struct Holder2 {
mut:
	v int
}

struct Helper {}

fn (h Helper) apply(mut item Holder2, x int) {
	item.v += x
}

fn (mut tr Tracer) which_index() int {
	tr.order << 1
	return 0
}

fn (mut tr Tracer) delta_first(_ First) !int {
	tr.order << 2
	return 6
}

fn (mut tr Tracer) delta_second(_ Second) !int {
	tr.order << 2
	return 8
}

// Mutable argument lvalue before a value-match argument: the mut lvalue identity must
// be preserved (only its index stabilized), so the mutation reaches holders[0], and the
// index runs before the argument prelude. First -> holders[0].v = 70 + 6 = 76, order
// [1,2] -> 7612 (mutation lost would be 7012; reversed order 7621).
fn select_value_mut_arg(node Node) !int {
	mut tr := Tracer{}
	helper := Helper{}
	mut holders := [Holder2{
		v: 70
	}, Holder2{
		v: 80
	}]
	helper.apply(mut holders[tr.which_index()], match node {
		First { tr.delta_first(node)! }
		Second { tr.delta_second(node)! }
	})
	return holders[0].v * 100 + tr.order[0] * 10 + tr.order[1]
}

fn len_first(_ First) !int {
	return 4
}

fn len_second(_ Second) !int {
	return 6
}

// Array initializer field that is a value match: the len field must be lowered as a
// value. First -> []int{len: 4}, so arr.len = 4, encoded 4*1000 + 4 = 4004.
fn select_value_array_init(node Node) !int {
	arr := []int{len: match node {
		First { len_first(node)! }
		Second { len_second(node)! }
	}}
	return arr.len * 1000 + arr.len
}

struct Mutator {
mut:
	items []int
}

fn (mut m Mutator) idx() int {
	return 0
}

fn (mut m Mutator) bump_first(_ First) !int {
	m.items[0] = 999
	return 7
}

fn (mut m Mutator) bump_second(_ Second) !int {
	m.items[0] = 888
	return 8
}

fn take(a int, b int) int {
	return a * 100 + b
}

// Ordinary (non-mut) lvalue argument (non-stable index) before a value-match argument
// whose prelude mutates the container: the argument value must be read in source order,
// before the mutation. First -> take(items[idx()]=5, 7) = 507 (if the mutated 999 leaked
// in it would be 99907).
fn select_value_nonmut_arg_value(node Node) !int {
	mut m := Mutator{
		items: [5, 6]
	}
	return take(m.items[m.idx()], match node {
		First { m.bump_first(node)! }
		Second { m.bump_second(node)! }
	})
}

fn (mut tr Tracer) tlen() int {
	tr.order << 1
	return 2
}

fn (mut tr Tracer) tcap_first(_ First) !int {
	tr.order << 2
	return 4
}

fn (mut tr Tracer) tcap_second(_ Second) !int {
	tr.order << 2
	return 6
}

// Array initializer field ordering: a side-effecting len must run before a value-match
// cap prelude. First -> len 2, order [1,2] -> 212 (a reversed order would be 221).
fn select_value_array_init_cap_order(node Node) !int {
	mut tr := Tracer{}
	arr := []int{len: tr.tlen(), cap: match node {
		First { tr.tcap_first(node)! }
		Second { tr.tcap_second(node)! }
	}}
	return arr.len * 100 + tr.order[0] * 10 + tr.order[1]
}

fn (mut tr Tracer) tlen2() int {
	tr.order << 1
	return 3
}

// Array initializer field ordering with a *nested* cap branch: the match is buried inside a
// compound cap (`cap: 1 + (match ...)`), which still hoists the prelude ahead of the
// allocation call. A side-effecting len must still run before it. First -> len 3, order
// [1,2] -> 312 (a reversed order would be 321).
fn select_value_nested_cap_order(node Node) !int {
	mut tr := Tracer{}
	arr := []int{len: tr.tlen2(), cap: 1 + (match node {
		First { tr.tcap_first(node)! }
		Second { tr.tcap_second(node)! }
	})}
	return arr.len * 100 + tr.order[0] * 10 + tr.order[1]
}

// Address-of a value match (the checker permits `&` on a struct-typed match):
// the propagating branch tail is materialized to a value temp whose address is
// taken, then a field is read through it.
fn select_value_addr(node ?Node) !int {
	result := if value := node {
		p := &(match value {
			First { boxed(lower_first(value)!) }
			Second { boxed(lower_second(value)!) }
		})
		p.value
	} else {
		0
	}
	return result
}

fn main() {
	println(select_value(First{})!)
	println(select_value(Second{})!)
	println(select_value_paren(First{})!)
	println(select_value_unsafe(Second{})!)
	println(select_value_cast(First{})!)
	println(select_value_cast_unsafe(Second{})!)
	println(select_value_infix_right(First{})!)
	println(select_value_infix_left(Second{})!)
	println(select_value_callarg(Second{})!)
	println(select_value_nested_callarg(First{})!)
	println(select_value_callarg_infix(First{})!)
	println(select_value_arraylit(First{})!)
	println(select_value_prefix(First{})!)
	println(select_value_index(First{})!)
	println(select_value_slice_bound(First{})!)
	println(select_value_membership(First{})!)
	println(select_value_selector(First{})!)
	println(select_value_mapkey(First{})![1])
	println(select_value_interp(First{})!)
	println(select_value_likely(First{})!)
	println(select_value_structinit(First{})!.value)
	println(select_value_mapinit(Second{})![7])
	println(select_value_ascast(5)!)
	println(select_value_ascast_unsafe(5)!)
	println(direct_match(Second{})!)
	println(select_value_infix_order(First{})!)
	println(select_value_nested_infix_order(First{})!)
	println(select_value_shift_order(First{})!)
	println(select_value_nested_shift_order(First{})!)
	println(select_value_index_order(First{})!)
	println(select_value_nested_index_order(First{})!)
	println(select_value_gated_index_order(First{})!)
	println(select_value_range_low(First{})!)
	println(select_value_range_membership(First{})!)
	println(select_value_range_order(First{})!)
	println(select_value_nested_range_order(First{})!)
	println(select_value_membership_container(First{})!)
	println(select_value_forin_container(First{})!)
	println(select_value_map_index(First{})!)
	println(select_value_string_membership_order(First{})!)
	println(select_value_nested_string_membership_order(First{})!)
	println(select_value_append_order(First{})!)
	println(select_value_nested_append_order(First{})!)
	println(select_value_const_membership(First{})!)
	println(select_value_map_membership_order(First{})!)
	println(select_value_push_many(First{})!)
	println(select_value_method_receiver(First{})!)
	println(select_value_method_arg(First{})!)
	println(select_value_channel_send(First{})!)
	println(select_value_call_operand_order(First{})!)
	println(select_value_mut_receiver(First{})!)
	println(select_value_value_receiver(First{})!)
	println(select_value_channel_target_order(First{})!)
	println(select_value_nested_channel_target_order(First{})!)
	println(select_value_rvalue_channel_target(First{})!)
	println(select_value_composite_rvalue_channel_target(First{})!)
	println(select_value_branch_channel_target(First{})!)
	println(select_value_channel_target_reassign(First{})!)
	println(select_value_plain_call_order(First{})!)
	println(select_value_nested_branch_arg_order(First{})!)
	println(select_value_mut_arg(First{})!)
	println(select_value_array_init(First{})!)
	println(select_value_nonmut_arg_value(First{})!)
	println(select_value_array_init_cap_order(First{})!)
	println(select_value_nested_cap_order(First{})!)
	println(select_value_stable_lhs_snapshot(First{})!)
	println(select_value_stable_arg_snapshot(First{})!)
	println(select_value_range_low_snapshot(First{})!)
	println(select_value_map_base_snapshot(First{})!)
	println(select_value_gated_base_snapshot(First{})!)
	println(select_value_mut_receiver_index_snapshot(First{})!)
	println(select_value_pointer_receiver_capture(First{})!)
	println(select_value_array_base_capture(First{})!)
	println(select_value_struct_field_order(First{})!)
	println(select_value_optional_append_reassign(First{})!)
	println(select_value_select_case_order(First{})!)
	println(select_value_select_stable_operand(First{})!)
	println(select_value_select_branch_channel(First{})!)
	println(select_value_membership_needle_snapshot(First{})!)
	println(select_value_branch_callee(First{})!)
	println(select_value_runtime_callee_order(First{})!)
	println(select_value_fn_field_callee_order(First{})!)
	println(select_value_addr(First{})!)
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_match_as_if_expr_value_propagation_out')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1\n2\n1\n2\n1\n2\n2\n12\n20\n100\n20\n[1]\n-1\n20\n[20, 30, 40]\ntrue\n1\n100\nx=1\ntrue\n1\n2\n6\n6\n2\n1112\n1212\n102412\n204812\n1012\n2012\n3012\n6\ntrue\n112\n112\ntrue\n60\n2\n512\n512\n712\n812\ntrue\n612\n61\n63\n1003\n42\n2012\n4512\n5002\n9912\n9912\n7712\n5512\n7\n7\n3412\n3512\n7612\n4004\n507\n212\n312\n6100\n1005\n3\n5\n5\n4550\n16000\n2500\n3412\n500\n12\n5\n12\ntrue\n41\n712\n30\n1'
}
