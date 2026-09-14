module types

import v.flat
import v.token

fn generic_comptime_shadow_scan(condition string) []TypeError {
	mut a := flat.FlatAst.new()
	mut file_set := token.FileSet.new()
	file := file_set.add_file('generic.v', 12)
	a.source_files[1] = file

	lhs_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'counter'
		pos:   token.new_span(1, 0, 7)
	})
	rhs_id := a.add_node(flat.Node{
		kind:  .int_literal
		value: '1'
		pos:   token.new_span(1, 11, 12)
	})
	decl_children := a.begin_children()
	a.add_child(lhs_id)
	a.add_child(rhs_id)
	decl_id := a.add_node(flat.Node{
		kind:           .decl_assign
		children_start: decl_children
		children_count: 2
		pos:            token.new_span(1, 0, 12)
	})
	block_children := a.begin_children()
	a.add_child(decl_id)
	block_id := a.add_node(flat.Node{
		kind:           .block
		children_start: block_children
		children_count: 1
		pos:            token.new_span(1, 0, 12)
	})
	if_children := a.begin_children()
	a.add_child(block_id)
	if_id := a.add_node(flat.Node{
		kind:           .comptime_if
		value:          condition
		children_start: if_children
		children_count: 1
		pos:            token.new_span(1, 0, 12)
	})
	fn_children := a.begin_children()
	a.add_child(if_id)
	mut fn_node := flat.Node{
		kind:           .fn_decl
		value:          'get'
		children_start: fn_children
		children_count: 1
		pos:            token.new_span(1, 0, 12)
	}
	fn_node.set_generic_params(['T'])
	fn_id := a.add_node(fn_node)

	mut tc := TypeChecker.new(&a)
	tc.global_names['counter'] = true
	tc.check_generic_fn_body_global_shadowing(a.node(fn_id))
	return tc.errors
}

fn test_generic_shadow_scan_skips_known_inactive_comptime_branch() {
	assert generic_comptime_shadow_scan('false').len == 0
}

fn test_generic_shadow_scan_checks_known_active_comptime_branch() {
	errors := generic_comptime_shadow_scan('true')
	assert errors.len == 1
	assert errors[0].msg == 'variable `counter` shadows a global variable'
}

fn test_generic_shadow_scan_defers_specialization_dependent_comptime_branch() {
	assert generic_comptime_shadow_scan('T is int').len == 0
}

fn test_generic_shadow_scan_checks_portable_target_comptime_branches() {
	errors := generic_comptime_shadow_scan('windows')
	assert errors.len == 1
	assert errors[0].msg == 'variable `counter` shadows a global variable'
}

fn test_generic_shadow_scan_defers_mixed_target_and_specialization_condition() {
	assert generic_comptime_shadow_scan('windows && T is int').len == 0
}
