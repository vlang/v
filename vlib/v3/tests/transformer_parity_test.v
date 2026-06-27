import os
import v3.flat
import v3.parser
import v3.pref
import v3.transform
import v3.types

// parse_transform_source reads parse transform source input for v3 tests.
fn parse_transform_source(source string) &flat.FlatAst {
	src := os.join_path(os.temp_dir(), 'v3_transformer_parity_test.v')
	return parse_transform_file(src, source)
}

// parse_transform_file reads parse transform file input for v3 tests.
fn parse_transform_file(src string, source string) &flat.FlatAst {
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	transform.transform(mut a, &tc)
	return a
}

// find_fn resolves find fn information for v3 tests.
fn find_fn(a &flat.FlatAst, name string) flat.Node {
	for node in a.nodes {
		if node.kind == .fn_decl && node.value == name {
			return node
		}
	}
	assert false
	return flat.Node{}
}

// first_decl_rhs returns first decl rhs data for v3 tests.
fn first_decl_rhs(a &flat.FlatAst, fn_name string) flat.Node {
	f := find_fn(a, fn_name)
	for i in 0 .. f.children_count {
		stmt := a.child_node(&f, i)
		if stmt.kind == .decl_assign && stmt.children_count == 2 {
			return *a.child_node(stmt, 1)
		}
	}
	assert false
	return flat.Node{}
}

// decl_rhs supports decl rhs handling for v3 tests.
fn decl_rhs(a &flat.FlatAst, fn_name string, name string) flat.Node {
	f := find_fn(a, fn_name)
	for i in 0 .. f.children_count {
		stmt := a.child_node(&f, i)
		if stmt.kind == .decl_assign && stmt.children_count == 2 {
			lhs := a.child_node(stmt, 0)
			if lhs.kind == .ident && lhs.value == name {
				return *a.child_node(stmt, 1)
			}
		}
	}
	assert false
	return flat.Node{}
}

// first_return_expr returns first return expr data for v3 tests.
fn first_return_expr(a &flat.FlatAst, fn_name string) flat.Node {
	f := find_fn(a, fn_name)
	for i in 0 .. f.children_count {
		stmt := a.child_node(&f, i)
		if stmt.kind == .return_stmt && stmt.children_count > 0 {
			return *a.child_node(stmt, 0)
		}
	}
	assert false
	return flat.Node{}
}

// count_kind supports count kind handling for v3 tests.
fn count_kind(a &flat.FlatAst, id flat.NodeId, kind flat.NodeKind) int {
	if int(id) < 0 {
		return 0
	}
	node := a.nodes[int(id)]
	mut total := if node.kind == kind { 1 } else { 0 }
	for i in 0 .. node.children_count {
		total += count_kind(a, a.child(&node, i), kind)
	}
	return total
}

// count_call_name supports count call name handling for v3 tests.
fn count_call_name(a &flat.FlatAst, id flat.NodeId, name string) int {
	if int(id) < 0 {
		return 0
	}
	node := a.nodes[int(id)]
	mut total := 0
	if node.kind == .call && node.children_count > 0 {
		fn_node := a.child_node(&node, 0)
		if fn_node.kind == .ident && fn_node.value == name {
			total++
		}
	}
	for i in 0 .. node.children_count {
		total += count_call_name(a, a.child(&node, i), name)
	}
	return total
}

// count_infix_op supports count infix op handling for v3 tests.
fn count_infix_op(a &flat.FlatAst, id flat.NodeId, op flat.Op) int {
	if int(id) < 0 {
		return 0
	}
	node := a.nodes[int(id)]
	mut total := if node.kind == .infix && node.op == op { 1 } else { 0 }
	for i in 0 .. node.children_count {
		total += count_infix_op(a, a.child(&node, i), op)
	}
	return total
}

// count_wide_decl_assigns supports count wide decl assigns handling for v3 tests.
fn count_wide_decl_assigns(a &flat.FlatAst, id flat.NodeId) int {
	if int(id) < 0 {
		return 0
	}
	node := a.nodes[int(id)]
	mut total := if node.kind == .decl_assign && node.children_count > 2 { 1 } else { 0 }
	for i in 0 .. node.children_count {
		total += count_wide_decl_assigns(a, a.child(&node, i))
	}
	return total
}

// count_wide_assigns supports count wide assigns handling for v3 tests.
fn count_wide_assigns(a &flat.FlatAst, id flat.NodeId) int {
	if int(id) < 0 {
		return 0
	}
	node := a.nodes[int(id)]
	mut total := if node.kind == .assign && node.children_count > 2 { 1 } else { 0 }
	for i in 0 .. node.children_count {
		total += count_wide_assigns(a, a.child(&node, i))
	}
	return total
}

// count_selector_value supports count selector value handling for v3 tests.
fn count_selector_value(a &flat.FlatAst, id flat.NodeId, value string) int {
	if int(id) < 0 {
		return 0
	}
	node := a.nodes[int(id)]
	mut total := if node.kind == .selector && node.value == value { 1 } else { 0 }
	for i in 0 .. node.children_count {
		total += count_selector_value(a, a.child(&node, i), value)
	}
	return total
}

// test_typeof_expression_lowers_to_string_literal validates this v3 regression case.
fn test_typeof_expression_lowers_to_string_literal() {
	a := parse_transform_source('
fn main() {
	name := typeof(123)
}
')
	main_fn := find_fn(a, 'main')
	for i in 0 .. main_fn.children_count {
		stmt := a.child_node(&main_fn, i)
		if stmt.kind == .decl_assign && stmt.children_count == 2 {
			rhs := a.child_node(stmt, 1)
			assert rhs.kind == .string_literal
			assert rhs.value == 'int literal'
			return
		}
	}
	assert false
}

// test_vmodroot_lowers_to_nearest_vmod_dir validates this v3 regression case.
fn test_vmodroot_lowers_to_nearest_vmod_dir() {
	root := os.join_path(os.temp_dir(), 'v3_vmodroot_transformer_parity')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "parity" }') or { panic(err) }
	src := os.join_path(root, 'main.v')
	a := parse_transform_file(src, '
fn main() {
	root := @VMODROOT
}
')
	rhs := first_decl_rhs(a, 'main')
	assert rhs.kind == .string_literal
	assert rhs.value == root
}

// test_return_match_lowers_to_explicit_branch_returns validates this v3 regression case.
fn test_return_match_lowers_to_explicit_branch_returns() {
	a := parse_transform_source('
fn choose(x int) int {
	return match x {
		0 {
			10
		}
		else {
			20
		}
	}
}
')
	choose_fn := find_fn(a, 'choose')
	mut match_count := 0
	mut return_count := 0
	for i in 0 .. choose_fn.children_count {
		child_id := a.child(&choose_fn, i)
		match_count += count_kind(a, child_id, .match_stmt)
		return_count += count_kind(a, child_id, .return_stmt)
	}
	assert match_count == 0
	assert return_count == 2
}

// test_return_if_keeps_or_lowering_inside_branch validates this v3 regression case.
fn test_return_if_keeps_or_lowering_inside_branch() {
	a := parse_transform_source('
fn maybe_int() ?int {
	return 8
}

fn choose(flag bool) int {
	return if flag {
		maybe_int() or {
			1
		}
	} else {
		2
	}
}
')
	choose_fn := find_fn(a, 'choose')
	mut body_ids := []flat.NodeId{}
	for i in 0 .. choose_fn.children_count {
		child_id := a.child(&choose_fn, i)
		child := a.nodes[int(child_id)]
		if child.kind != .param {
			body_ids << child_id
		}
	}
	assert body_ids.len == 1
	assert a.nodes[int(body_ids[0])].kind == .if_expr
}

// test_if_expr_value_lowers_to_temp_and_branch_assigns validates this v3 regression case.
fn test_if_expr_value_lowers_to_temp_and_branch_assigns() {
	a := parse_transform_source('
fn main() {
	x := if true {
		1
	} else {
		2
	}
}
')
	rhs := decl_rhs(a, 'main', 'x')
	assert rhs.kind == .ident
	assert rhs.value.starts_with('__if_val_')
	main_fn := find_fn(a, 'main')
	mut if_count := 0
	mut assign_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		if_count += count_kind(a, child_id, .if_expr)
		assign_count += count_kind(a, child_id, .assign)
	}
	assert if_count == 1
	assert assign_count == 2
}

// test_if_expr_call_arg_lowers_before_call validates this v3 regression case.
fn test_if_expr_call_arg_lowers_before_call() {
	a := parse_transform_source('
fn use(x int) int {
	return x
}

fn main() {
	x := use(if true { 1 } else { 2 })
}
')
	rhs := decl_rhs(a, 'main', 'x')
	assert rhs.kind == .call
	assert rhs.children_count == 2
	arg := a.child_node(&rhs, 1)
	assert arg.kind == .ident
	assert arg.value.starts_with('__if_val_')
}

// test_nested_if_expr_branch_lowers_to_outer_temp_assignment validates this v3 regression case.
fn test_nested_if_expr_branch_lowers_to_outer_temp_assignment() {
	a := parse_transform_source('
fn main() {
	x := if true {
		if false {
			1
		} else {
			2
		}
	} else {
		3
	}
}
')
	rhs := decl_rhs(a, 'main', 'x')
	assert rhs.kind == .ident
	assert rhs.value.starts_with('__if_val_')
	main_fn := find_fn(a, 'main')
	mut if_count := 0
	mut assign_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		if_count += count_kind(a, child_id, .if_expr)
		assign_count += count_kind(a, child_id, .assign)
	}
	assert if_count == 2
	assert assign_count == 4
}

// test_multi_return_decl_lowers_to_temp_field_decls validates this v3 regression case.
fn test_multi_return_decl_lowers_to_temp_field_decls() {
	a := parse_transform_source("
fn pair() (int, string) {
	return 1, 'ok'
}

fn main() {
	a, b := pair()
}
")
	main_fn := find_fn(a, 'main')
	mut wide_decl_count := 0
	for i in 0 .. main_fn.children_count {
		wide_decl_count += count_wide_decl_assigns(a, a.child(&main_fn, i))
	}
	assert wide_decl_count == 0
	a_rhs := decl_rhs(a, 'main', 'a')
	b_rhs := decl_rhs(a, 'main', 'b')
	assert a_rhs.kind == .selector
	assert a_rhs.value == 'arg0'
	assert b_rhs.kind == .selector
	assert b_rhs.value == 'arg1'
}

// test_multi_return_assign_lowers_to_temp_field_assigns validates this v3 regression case.
fn test_multi_return_assign_lowers_to_temp_field_assigns() {
	a := parse_transform_source("
fn pair() (int, string) {
	return 1, 'ok'
}

fn main() {
	mut a := 0
	mut b := ''
	a, b = pair()
}
")
	main_fn := find_fn(a, 'main')
	mut wide_assign_count := 0
	mut arg0_count := 0
	mut arg1_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		wide_assign_count += count_wide_assigns(a, child_id)
		arg0_count += count_selector_value(a, child_id, 'arg0')
		arg1_count += count_selector_value(a, child_id, 'arg1')
	}
	assert wide_assign_count == 0
	assert arg0_count == 1
	assert arg1_count == 1
}

// test_assoc_expr_lowers_to_temp_and_field_assigns validates this v3 regression case.
fn test_assoc_expr_lowers_to_temp_and_field_assigns() {
	a := parse_transform_source('
struct Point {
	x int
	y int
}

fn main() {
	p := Point{x: 1, y: 2}
	q := Point{...p, y: 3}
}
')
	rhs := decl_rhs(a, 'main', 'q')
	assert rhs.kind == .ident
	assert rhs.value.starts_with('__assoc_')
	main_fn := find_fn(a, 'main')
	mut assoc_count := 0
	mut assign_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		assoc_count += count_kind(a, child_id, .assoc)
		assign_count += count_kind(a, child_id, .assign)
	}
	assert assoc_count == 0
	assert assign_count == 1
}

// test_return_assoc_expr_lowers_before_return validates this v3 regression case.
fn test_return_assoc_expr_lowers_before_return() {
	a := parse_transform_source('
struct Point {
	x int
	y int
}

fn moved(p Point) Point {
	return Point{...p, x: 4}
}
')
	ret := first_return_expr(a, 'moved')
	assert ret.kind == .ident
	assert ret.value.starts_with('__assoc_')
	moved_fn := find_fn(a, 'moved')
	mut assoc_count := 0
	for i in 0 .. moved_fn.children_count {
		assoc_count += count_kind(a, a.child(&moved_fn, i), .assoc)
	}
	assert assoc_count == 0
}

// test_array_append_stmt_lowers_to_runtime_push validates this v3 regression case.
fn test_array_append_stmt_lowers_to_runtime_push() {
	a := parse_transform_source('
fn main() {
	mut xs := []int{}
	xs << 3
}
')
	main_fn := find_fn(a, 'main')
	mut push_count := 0
	mut left_shift_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		push_count += count_call_name(a, child_id, 'array_push')
		left_shift_count += count_infix_op(a, child_id, .left_shift)
	}
	assert push_count == 1
	assert left_shift_count == 0
}

// test_array_append_many_stmt_lowers_to_runtime_push_many validates this v3 regression case.
fn test_array_append_many_stmt_lowers_to_runtime_push_many() {
	a := parse_transform_source('
fn main() {
	mut xs := []int{}
	ys := []int{}
	xs << ys
}
')
	main_fn := find_fn(a, 'main')
	mut push_many_count := 0
	mut left_shift_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		push_many_count += count_call_name(a, child_id, 'array__push_many')
		left_shift_count += count_infix_op(a, child_id, .left_shift)
	}
	assert push_many_count == 1
	assert left_shift_count == 0
}

fn test_array_push_many_method_lowers_to_runtime_ptr_call() {
	a := parse_transform_source('
fn main() {
	mut xs := []u8{}
	mut b := u8(7)
	unsafe {
		xs.push_many(&b, 1)
	}
}
')
	main_fn := find_fn(a, 'main')
	mut push_many_ptr_count := 0
	mut push_many_selector_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		push_many_ptr_count += count_call_name(a, child_id, 'array_push_many_ptr')
		push_many_selector_count += count_selector_value(a, child_id, 'push_many')
	}
	assert push_many_ptr_count == 1
	assert push_many_selector_count == 0
}

fn test_array_push_many_type_marker_lowers_to_runtime_ptr_call() {
	a := parse_transform_source('
struct PushManyItem {
	value int
}

fn main() {
	mut xs := []PushManyItem{}
	item := PushManyItem{value: 7}
	xs.push_many(item, PushManyItem)
}
')
	main_fn := find_fn(a, 'main')
	mut push_many_ptr_count := 0
	mut push_many_selector_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		push_many_ptr_count += count_call_name(a, child_id, 'array_push_many_ptr')
		push_many_selector_count += count_selector_value(a, child_id, 'push_many')
	}
	assert push_many_ptr_count == 1
	assert push_many_selector_count == 0
}

fn test_sql_expr_lowers_to_success_result_default() {
	a := parse_transform_source('
struct User {
	id int
}

fn main() {
	db := 0
	users := sql db {
		select from User
	}
	count := sql db {
		select count from User
	}
	created := sql db {
		create table User
	}
}
')
	main_fn := find_fn(a, 'main')
	mut sql_expr_count := 0
	for i in 0 .. main_fn.children_count {
		sql_expr_count += count_kind(a, a.child(&main_fn, i), .sql_expr)
	}
	assert sql_expr_count == 0
	users := decl_rhs(a, 'main', 'users')
	assert users.kind == .struct_init
	assert users.value == '![]User'
	count := decl_rhs(a, 'main', 'count')
	assert count.kind == .struct_init
	assert count.value == '!int'
	created := decl_rhs(a, 'main', 'created')
	assert created.kind == .struct_init
	assert created.value == '!void'
}

// test_or_expr_lowers_to_temp_and_if validates this v3 regression case.
fn test_or_expr_lowers_to_temp_and_if() {
	a := parse_transform_source('
fn maybe_int() ?int {
	return 3
}

fn main() {
	x := maybe_int() or {
		7
	}
}
')
	main_fn := find_fn(a, 'main')
	mut or_count := 0
	mut if_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		or_count += count_kind(a, child_id, .or_expr)
		if_count += count_kind(a, child_id, .if_expr)
	}
	assert or_count == 0
	assert if_count == 1
}

// test_map_index_or_lowers_to_get_check validates this v3 regression case.
fn test_map_index_or_lowers_to_get_check() {
	a := parse_transform_source('
fn main() {
	mut m := map[string]int{}
	x := m["a"] or {
		7
	}
}
')
	main_fn := find_fn(a, 'main')
	mut or_count := 0
	mut get_check_count := 0
	for i in 0 .. main_fn.children_count {
		child_id := a.child(&main_fn, i)
		or_count += count_kind(a, child_id, .or_expr)
		get_check_count += count_call_name(a, child_id, 'map__get_check')
	}
	assert or_count == 0
	assert get_check_count == 1
}
