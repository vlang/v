import os
import v3.flat
import v3.parser
import v3.pref
import v3.types

const local_vexe = @VEXE
const local_tests_dir = os.dir(@FILE)
const local_v3_dir = os.dir(local_tests_dir)
const local_vlib_dir = os.dir(local_v3_dir)

fn proxy_to_local_v3_if_needed() bool {
	if os.getenv('V3_MATCH_GENERIC_PATTERN_INNER') == '1' {
		return false
	}
	cmd := 'V3_MATCH_GENERIC_PATTERN_INNER=1 ${os.quoted_path(local_vexe)} -gc none -path "${local_vlib_dir}|@vlib|@vmodules" ${os.quoted_path(@FILE)}'
	result := os.execute(cmd)
	assert result.exit_code == 0, result.output
	return true
}

fn parse_match_generic_pattern_source(src string) &flat.FlatAst {
	path := os.join_path(os.temp_dir(), 'v3_match_generic_pattern_${os.getpid()}.v')
	os.write_file(path, src) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_into(path)
	return p.a
}

fn find_fn_node(a &flat.FlatAst, name string) flat.Node {
	for node in a.nodes {
		if node.kind == .fn_decl && node.value == name {
			return node
		}
	}
	assert false, 'missing fn `${name}`'
	return flat.Node{}
}

fn find_match_branch_with_pattern(a &flat.FlatAst, pattern string) flat.Node {
	for node in a.nodes {
		if node.kind != .match_branch || node.children_count == 0 {
			continue
		}
		cond := a.child_node(&node, 0)
		if cond.kind == .ident && cond.value == pattern {
			return node
		}
		if cond.kind == .selector && cond.value == pattern {
			return node
		}
	}
	assert false, 'missing match branch pattern `${pattern}`'
	return flat.Node{}
}

fn final_file_node(a &flat.FlatAst) flat.Node {
	for i := a.nodes.len - 1; i >= 0; i-- {
		node := a.nodes[i]
		if node.kind == .file && node.children_count > 0 {
			return node
		}
	}
	assert false, 'missing final file node'
	return flat.Node{}
}

fn file_child_fn_names(a &flat.FlatAst) []string {
	file_node := final_file_node(a)
	mut names := []string{}
	for i in 0 .. file_node.children_count {
		child := a.child_node(&file_node, i)
		if child.kind == .fn_decl {
			names << child.value
		}
	}
	return names
}

fn find_match_branch_with_qualified_pattern(a &flat.FlatAst, module_name string, pattern string) flat.Node {
	for node in a.nodes {
		if node.kind != .match_branch || node.children_count == 0 {
			continue
		}
		cond := a.child_node(&node, 0)
		if cond.kind != .selector || cond.value != pattern || cond.children_count == 0 {
			continue
		}
		base := a.child_node(cond, 0)
		if base.kind == .ident && base.value == module_name {
			return node
		}
	}
	assert false, 'missing match branch pattern `${module_name}.${pattern}`'
	return flat.Node{}
}

fn branch_has_direct_array_literal(a &flat.FlatAst, branch flat.Node) bool {
	for i in 1 .. branch.children_count {
		child := a.child_node(&branch, i)
		if child.kind == .array_literal {
			return true
		}
		if child.kind == .expr_stmt && child.children_count > 0 {
			expr := a.child_node(child, 0)
			if expr.kind == .array_literal {
				return true
			}
		}
	}
	return false
}

fn test_match_branch_generic_type_patterns_are_consumed() {
	if proxy_to_local_v3_if_needed() {
		return
	}
	a := parse_match_generic_pattern_source('
struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn walk[T](tree Tree[T]) int {
	return match tree {
		Empty { 0 }
		Node[T] { tree.value }
	}
}

fn qualified[T](tree Tree[T]) int {
	return match tree {
		foo.Node[T] { 1 }
		else { 0 }
	}
}

fn nested_arg[T](tree Tree[T]) int {
	return match tree {
		Node[foo.Bar] { 1 }
		else { 0 }
	}
}

fn after() int {
	return 7
}
')

	names := file_child_fn_names(a)
	assert names == ['walk', 'qualified', 'nested_arg', 'after']
	find_fn_node(a, 'after')

	node_branch := find_match_branch_with_pattern(a, 'Node[T]')
	assert node_branch.children_count == 2
	assert !branch_has_direct_array_literal(a, node_branch)

	nested_arg_branch := find_match_branch_with_pattern(a, 'Node[foo.Bar]')
	nested_cond := a.child_node(&nested_arg_branch, 0)
	assert nested_cond.kind == .ident
	assert nested_cond.value == 'Node[foo.Bar]'

	qualified_branch := find_match_branch_with_qualified_pattern(a, 'foo', 'Node[T]')
	cond := a.child_node(&qualified_branch, 0)
	base := a.child_node(cond, 0)
	assert base.kind == .ident
	assert base.value == 'foo'
	assert cond.value == 'Node[T]'
}

fn check_source_errors(name string, src string) []types.TypeError {
	path := os.join_path(os.temp_dir(), 'v3_generic_sum_checker_${name}_${os.getpid()}.v')
	os.write_file(path, src) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[path] = true
	tc.check_semantics()
	return tc.errors.clone()
}

fn assert_checker_ok(name string, src string) {
	errors := check_source_errors(name, src)
	assert errors.len == 0, errors.str()
}

fn assert_checker_error_contains(name string, src string, expected string) {
	errors := check_source_errors(name, src)
	assert errors.len > 0
	mut text := ''
	for err in errors {
		text += err.msg + '\n'
	}
	assert text.contains(expected), text
}

const generic_sum_defs = '
struct Empty {}

struct Node[T] {
	value T
	left Tree[T]
}

struct Other[T] {
	value T
}

type Tree[T] = Empty | Node[T]
'

fn test_generic_sum_type_checker_accepts_shared_field_on_concrete_sum() {
	if proxy_to_local_v3_if_needed() {
		return
	}
	assert_checker_ok('generic_sum_shared_field', '
struct A[T] {
	x T
}

struct B[T] {
	x T
}

type S[T] = A[T] | B[T]

fn get(s S[int]) int {
	return s.x
}
')
}

fn test_generic_sum_type_checker_accepts_methods_returns_and_smartcasts() {
	if proxy_to_local_v3_if_needed() {
		return
	}
	assert_checker_ok('generic_sum_method_and_returns', generic_sum_defs +
		'
fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		Node[T] { 1 +
		tree.left.size() }
	}
}

fn make_int_tree() Tree[int] {
	return Node[int]{
		value: 1
		left: Empty{}
	}
}

fn make_tree[T](value T) Tree[T] {
	return Node[T]{
		value: value
		left: Empty{}
	}
}

fn field_after_is[T](tree Tree[T]) T {
	if tree is Node[T] {
		return tree.value
	}
	return T(0)
}

fn main() {
	tree := make_int_tree()
	_ := tree.size()
}
')
}

fn test_generic_sum_type_checker_rejects_wrong_generic_variant() {
	if proxy_to_local_v3_if_needed() {
		return
	}
	assert_checker_error_contains('generic_sum_wrong_variant_arg', generic_sum_defs +
		'
fn bad() Tree[int] {
	return Node[string]{
		value: "bad"
		left: Empty{}
	}
}

fn main() {}
',
		'cannot return `Node[string]` as `Tree[int]`')
}

fn test_generic_sum_type_checker_rejects_non_variant_pattern() {
	if proxy_to_local_v3_if_needed() {
		return
	}
	assert_checker_error_contains('generic_sum_non_variant_pattern', generic_sum_defs +
		'
fn bad(tree Tree[int]) int {
	return match tree {
		Other[int] { 1 }
		else { 0 }
	}
}

fn main() {}
',
		'`Other[int]` is not a variant of sum type `Tree[int]`')
}

fn test_generic_sum_type_checker_accepts_qualified_generic_variant_pattern() {
	if proxy_to_local_v3_if_needed() {
		return
	}
	root := os.join_path(os.temp_dir(), 'v3_generic_sum_checker_qualified_${os.getpid()}')
	os.rmdir_all(root) or {}
	foo_path := os.join_path(root, 'foo', 'foo.v')
	main_path := os.join_path(root, 'main.v')
	os.mkdir_all(os.dir(foo_path)) or { panic(err) }
	os.write_file(foo_path, 'module foo

pub struct Node[T] {
pub:
	value T
}
') or { panic(err) }
	os.write_file(main_path, 'module main

import foo

struct Empty {}

type Tree[T] = Empty | foo.Node[T]

fn value(tree Tree[int]) int {
	return match tree {
		foo.Node[int] { tree.value }
		Empty { 0 }
	}
}

fn main() {}
') or {
		panic(err)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files([main_path, foo_path])
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[main_path] = true
	tc.diagnostic_files[foo_path] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
}
