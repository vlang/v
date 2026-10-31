module types

import v.flat

fn test_scalar_child_call_helpers_preserve_saved_header_after_ast_growth() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'saved')
	a.children << callee
	call := a.add_node(flat.Node{ kind: .call, children_start: 0, children_count: 1 })
	snapshot := a.nodes[int(call)]
	// Querying the saved header must not reload a header changed by a nested check.
	a.nodes[int(call)].children_count = 0
	growth := a.nodes.cap + 100
	for _ in 0 .. growth {
		a.add_val(.ident, 'padding')
	}
	mut tc := TypeChecker.new(&a)
	tc.fn_ret_types['saved'] = builtin_int_type
	assert tc.call_display_name(snapshot) == 'saved'
	assert tc.call_argument_target_name(snapshot, CallInfo{}) == 'saved'
	assert tc.c_va_macro_call_name(snapshot) == ''
	direct := tc.direct_call_return_type(snapshot) or { panic('missing direct return type') }
	spawned := tc.spawn_child_call_return_type(snapshot) or { panic('missing spawn return type') }
	assert direct.source_type_id() == flat.source_type_int
	assert spawned.source_type_id() == flat.source_type_int
	assert tc.builtin_array_annotation_expected_type(snapshot, 1) is Void
	assert snapshot.children_count == 1
}

fn test_scalar_child_function_wrappers_keep_saved_header_and_empty_sentinel() {
	mut a := flat.FlatAst.new()
	ident := a.add_val(.ident, 'saved')
	a.children << ident
	paren := a.add_node(flat.Node{ kind: .paren, children_start: 0, children_count: 1 })
	a.children << paren
	cast := a.add_node(flat.Node{ kind: .cast_expr, children_start: 1, children_count: 1 })
	snapshot := a.nodes[int(cast)]
	a.nodes[int(cast)].children_count = 0
	growth := a.nodes.cap + 100
	for _ in 0 .. growth {
		a.add_val(.ident, 'padding')
	}
	mut tc := TypeChecker.new(&a)
	tc.fn_ret_types['saved'] = builtin_int_type
	tc.fn_param_types['saved'] = []Type{}
	assert tc.fn_value_key(snapshot) or { panic('missing wrapped function key') } == 'saved'
	for start in [-1, a.children.len, a.children.len + 100] {
		invalid := flat.Node{ kind: .call, children_start: start, children_count: 1 }
		assert tc.call_display_name(invalid) == ''
		assert tc.direct_call_return_type(invalid) == none
	}
	assert tc.call_display_name(flat.Node{ kind: .call }) == '<missing>'
}

fn test_scalar_child_c_macro_and_signature_queries_keep_saved_headers() {
	mut a := flat.FlatAst.new()
	base := a.add_val(.ident, 'C')
	a.children << base
	selector := a.add_node(flat.Node{ kind: .selector, value: 'va_arg', children_start: 0, children_count: 1 })
	a.children << selector
	call := flat.Node{ kind: .call, children_start: 1, children_count: 1 }
	param := a.add_node(flat.Node{ kind: .param, value: 'arg', typ: flat.source_type_int })
	a.children << param
	decl := a.add_node(flat.Node{ kind: .fn_literal, typ: flat.source_type_bool, children_start: 2, children_count: 1 })
	signature := a.nodes[int(decl)]
	a.nodes[int(decl)].children_count = 0
	growth := a.nodes.cap + 100
	for _ in 0 .. growth {
		a.add_val(.ident, 'padding')
	}
	tc := TypeChecker.new(&a)
	assert tc.c_va_macro_call_name(call) == 'C.va_arg'
	assert tc.call_display_name(call) == 'C.va_arg'
	assert tc.fn_node_source_type_text(signature) == 'fn (arg int) bool'
	typ := tc.fn_literal_type(signature)
	assert typ is FnType
	if typ is FnType {
		assert typ.params.len == 1
		assert typ.params[0].typ.source_type_id() == flat.source_type_int
		assert typ.return_type.source_type_id() == flat.source_type_bool
	}
}
