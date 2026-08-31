module c

import v3.flat
import v3.types

fn add_const_deps_test_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: i32(start)
		children_count: flat.child_count(children.len)
	})
}

fn const_deps_test_read(mut a flat.FlatAst, name string) flat.NodeId {
	ident := add_const_deps_test_node(mut a, .ident, name, [])
	return add_const_deps_test_node(mut a, .expr_stmt, '', [ident])
}

fn const_deps_test_decl(mut a flat.FlatAst, name string) flat.NodeId {
	lhs := add_const_deps_test_node(mut a, .ident, name, [])
	rhs := add_const_deps_test_node(mut a, .int_literal, '1', [])
	return add_const_deps_test_node(mut a, .decl_assign, '', [lhs, rhs])
}

fn const_deps_test_fn(mut a flat.FlatAst, name string, body []flat.NodeId) flat.NodeId {
	block := add_const_deps_test_node(mut a, .block, '', body)
	return add_const_deps_test_node(mut a, .fn_decl, name, [block])
}

fn const_deps_test_call(mut a flat.FlatAst, name string) flat.NodeId {
	callee := add_const_deps_test_node(mut a, .ident, name, [])
	return add_const_deps_test_node(mut a, .call, '', [callee])
}

fn const_deps_test_method_call(mut a flat.FlatAst, receiver string, method string) flat.NodeId {
	base := add_const_deps_test_node(mut a, .struct_init, receiver, [])
	a.nodes[int(base)].typ = receiver
	callee := add_const_deps_test_node(mut a, .selector, method, [base])
	return add_const_deps_test_node(mut a, .call, '', [callee])
}

fn test_const_helper_shadows_follow_lexical_scope_and_declaration_order() {
	mut a := flat.FlatAst.new()
	dep_value := add_const_deps_test_node(mut a, .int_literal, '41', [])

	later_read := const_deps_test_read(mut a, 'dep')
	later_decl := const_deps_test_decl(mut a, 'dep')
	const_deps_test_fn(mut a, 'later_make', [later_read, later_decl])
	later_call := const_deps_test_call(mut a, 'later_make')

	nested_decl := const_deps_test_decl(mut a, 'dep')
	nested_block := add_const_deps_test_node(mut a, .block, '', [nested_decl])
	nested_read := const_deps_test_read(mut a, 'dep')
	const_deps_test_fn(mut a, 'nested_make', [nested_block, nested_read])
	nested_call := const_deps_test_call(mut a, 'nested_make')

	prior_decl := const_deps_test_decl(mut a, 'dep')
	prior_read := const_deps_test_read(mut a, 'dep')
	const_deps_test_fn(mut a, 'prior_make', [prior_decl, prior_read])
	prior_call := const_deps_test_call(mut a, 'prior_make')

	lambda_param := add_const_deps_test_node(mut a, .param, 'dep', [])
	lambda := add_const_deps_test_node(mut a, .lambda_expr, '', [lambda_param])
	lambda_read := const_deps_test_read(mut a, 'dep')
	const_deps_test_fn(mut a, 'lambda_make', [lambda, lambda_read])
	lambda_call := const_deps_test_call(mut a, 'lambda_make')

	inner_read := const_deps_test_read(mut a, 'dep')
	const_deps_test_fn(mut a, 'inner_make', [inner_read])
	outer_decl := const_deps_test_decl(mut a, 'dep')
	inner_call := const_deps_test_call(mut a, 'inner_make')
	inner_call_stmt := add_const_deps_test_node(mut a, .expr_stmt, '', [inner_call])
	const_deps_test_fn(mut a, 'outer_make', [outer_decl, inner_call_stmt])
	outer_call := const_deps_test_call(mut a, 'outer_make')

	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.const_vals['dep'] = dep_value
	g.const_modules['dep'] = 'main'

	assert 'dep' in g.const_get_deps(later_call)
	assert 'dep' in g.const_get_deps(nested_call)
	assert 'dep' !in g.const_get_deps(prior_call)
	assert 'dep' in g.const_get_deps(lambda_call)
	assert 'dep' in g.const_get_deps(outer_call)
}

fn test_const_helper_direct_body_carries_local_bindings() {
	mut a := flat.FlatAst.new()
	add_const_deps_test_node(mut a, .module_decl, 'main', [])
	dep_value := add_const_deps_test_node(mut a, .int_literal, '41', [])
	local_dep := const_deps_test_decl(mut a, 'dep')
	return_ident := add_const_deps_test_node(mut a, .ident, 'dep', [])
	return_stmt := add_const_deps_test_node(mut a, .return_stmt, '', [return_ident])
	add_const_deps_test_node(mut a, .fn_decl, 'make', [local_dep, return_stmt])
	call := const_deps_test_call(mut a, 'make')

	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.const_vals['dep'] = dep_value
	g.const_modules['dep'] = 'main'

	assert 'dep' !in g.const_get_deps(call)
}

fn test_const_helper_does_not_recurse_into_shadowed_fn_callee() {
	mut a := flat.FlatAst.new()
	add_const_deps_test_node(mut a, .module_decl, 'main', [])
	wrong_value := add_const_deps_test_node(mut a, .int_literal, '41', [])
	wrong_read := const_deps_test_read(mut a, 'wrong_dep')
	const_deps_test_fn(mut a, 'f', [wrong_read])
	param := add_const_deps_test_node(mut a, .param, 'f', [])
	param_call := const_deps_test_call(mut a, 'f')
	return_stmt := add_const_deps_test_node(mut a, .return_stmt, '', [param_call])
	add_const_deps_test_node(mut a, .fn_decl, 'make', [param, return_stmt])
	callee := add_const_deps_test_node(mut a, .ident, 'make', [])
	argument := add_const_deps_test_node(mut a, .ident, 'one', [])
	call := add_const_deps_test_node(mut a, .call, '', [callee, argument])

	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.const_vals['wrong_dep'] = wrong_value
	g.const_modules['wrong_dep'] = 'main'

	assert 'wrong_dep' !in g.const_get_deps(call)
}

fn test_const_helper_deps_resolve_receiver_method_before_suffix_fallback() {
	mut a := flat.FlatAst.new()
	wrong_value := add_const_deps_test_node(mut a, .int_literal, '7', [])
	dep_value := add_const_deps_test_node(mut a, .int_literal, '41', [])

	wrong_read := const_deps_test_read(mut a, 'wrong_dep')
	const_deps_test_fn(mut a, 'B.value', [wrong_read])
	dep_read := const_deps_test_read(mut a, 'dep')
	const_deps_test_fn(mut a, 'A.value', [dep_read])
	call := const_deps_test_method_call(mut a, 'A', 'value')

	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.const_vals['wrong_dep'] = wrong_value
	g.const_modules['wrong_dep'] = 'main'
	g.const_vals['dep'] = dep_value
	g.const_modules['dep'] = 'main'

	deps := g.const_get_deps(call)
	assert 'dep' in deps
	assert 'wrong_dep' !in deps
}

fn test_enum_helper_prefers_exact_free_function_over_method_suffix() {
	mut a := flat.FlatAst.new()
	add_const_deps_test_node(mut a, .module_decl, 'main', [])
	receiver := add_const_deps_test_node(mut a, .param, 'm', [])
	a.nodes[int(receiver)].typ = 'Maker'
	method_value := add_const_deps_test_node(mut a, .int_literal, '99', [])
	method_return := add_const_deps_test_node(mut a, .return_stmt, '', [method_value])
	add_const_deps_test_node(mut a, .fn_decl, 'Maker.make', [receiver, method_return])
	free_value := add_const_deps_test_node(mut a, .int_literal, '4', [])
	free_return := add_const_deps_test_node(mut a, .return_stmt, '', [free_value])
	add_const_deps_test_node(mut a, .fn_decl, 'make', [free_return])
	call := const_deps_test_call(mut a, 'make')

	mut g := FlatGen.new()
	g.a = &a
	mut field_values := map[string]i64{}
	mut resolving := map[string]bool{}
	value := g.enum_comptime_call_value(call, 'main', 'E', mut field_values,
		map[string]flat.NodeId{}, mut resolving) or { -1 }
	assert value == 4
}
