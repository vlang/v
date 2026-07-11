module c

import v3.flat

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

fn test_const_helper_shadow_collection_skips_nested_function_scopes() {
	mut a := flat.FlatAst.new()
	outer_param := add_const_deps_test_node(mut a, .param, 'outer_param', [])
	outer_local_lhs := add_const_deps_test_node(mut a, .ident, 'outer_local', [])
	outer_local := add_const_deps_test_node(mut a, .decl_assign, '', [outer_local_lhs])

	lambda_param := add_const_deps_test_node(mut a, .param, 'lambda_param', [])
	lambda_local_lhs := add_const_deps_test_node(mut a, .ident, 'lambda_local', [])
	lambda_local := add_const_deps_test_node(mut a, .decl_assign, '', [
		lambda_local_lhs,
	])
	lambda := add_const_deps_test_node(mut a, .lambda_expr, '', [lambda_param, lambda_local])

	fn_param := add_const_deps_test_node(mut a, .param, 'fn_param', [])
	fn_literal := add_const_deps_test_node(mut a, .fn_literal, '', [fn_param])
	outer_fn := add_const_deps_test_node(mut a, .fn_decl, 'make', [outer_param, outer_local, lambda,
		fn_literal])

	mut g := FlatGen.new()
	g.a = &a
	mut names := map[string]bool{}
	g.collect_fn_scope_names(outer_fn, mut names)

	assert names['outer_param']
	assert names['outer_local']
	assert !names['lambda_param']
	assert !names['lambda_local']
	assert !names['fn_param']
}
