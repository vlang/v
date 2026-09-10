module transform

import v3.flat
import v3.token
import v3.types

fn test_local_decl_type_before_pos_uses_indexed_same_file_function() {
	mut a := flat.FlatAst.new()
	first_lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'int'
	})
	first_rhs := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	first_children := a.children.len
	a.children << first_lhs
	a.children << first_rhs
	a.add_node(flat.Node{
		kind: .decl_assign
		typ: 'int'
		pos: token.new_pos(1, 20)
		children_start: first_children
		children_count: 2
	})
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'first'
		pos: token.new_pos(1, 10)
	})
	other_lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'string'
	})
	other_rhs := a.add_node(flat.Node{
		kind: .string_literal
		value: 'other'
		typ: 'string'
	})
	other_children := a.children.len
	a.children << other_lhs
	a.children << other_rhs
	a.add_node(flat.Node{
		kind: .decl_assign
		typ: 'string'
		pos: token.new_pos(2, 80)
		children_start: other_children
		children_count: 2
	})
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'other'
		pos: token.new_pos(2, 10)
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_source_parent_index()

	assert t.fn_decl_offsets_by_file[1] == [10]
	assert t.fn_decl_offsets_by_file[2] == [10]
	assert t.local_decl_type_before_pos('value', flat.Node{
		pos: token.new_pos(1, 90)
	})? == 'int'
}

fn test_comptime_option_unwrap_uses_indexed_alias_and_if_body() {
	mut a := flat.FlatAst.new()
	alias_lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
	})
	alias_rhs := a.add_node(flat.Node{
		kind: .ident
		value: 'original'
	})
	alias_children := a.children.len
	a.children << alias_lhs
	a.children << alias_rhs
	a.add_node(flat.Node{
		kind: .decl_assign
		pos: token.new_pos(1, 40)
		children_start: alias_children
		children_count: 2
	})
	condition_left := a.add_node(flat.Node{
		kind: .ident
		value: 'original'
	})
	condition_right := a.add_node(flat.Node{
		kind: .none_expr
	})
	condition_children := a.children.len
	a.children << condition_left
	a.children << condition_right
	condition := a.add_node(flat.Node{
		kind: .infix
		op: .ne
		children_start: condition_children
		children_count: 2
	})
	body := a.add_node(flat.Node{
		kind: .block
		pos: token.new_span(1, 100, 200)
	})
	if_children := a.children.len
	a.children << condition
	a.children << body
	a.add_node(flat.Node{
		kind: .if_expr
		children_start: if_children
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_source_parent_index()

	assert t.if_expr_nodes_by_file[1].len == 1
	assert t.comptime_option_unwrapped_local_type('value', flat.Node{
		pos: token.new_pos(1, 150)
	}, FieldMeta{
		is_option: true
		comptime_typ: '?int'
	})? == 'int'
}

fn test_monomorph_job_limit_keeps_four_workers_for_large_programs() {
	$if !v3_no_parallel ? {
		assert monomorph_job_limit(12, 500_000, 0) == 4
	}
}
