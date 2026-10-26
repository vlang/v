module transform

import v.flat
import v.types
import v.token

fn test_scalar_statement_scope_restores_shadowed_and_new_bindings() {
	for name in ['outer', 'new_local'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.comptime_scalar_locals['outer'] = ComptimeStringScalar{flat.source_type_int, '1'}
		decl := t.make_decl_assign_typed(name, t.make_int_literal(2), flat.source_type_int)
		a.nodes[int(decl)].pos = token.new_span(1, 1, 10)
		nested := t.make_block([decl])
		_ = t.transform_stmts([nested])
		assert t.comptime_scalar_locals.len == 1
		assert t.comptime_scalar_locals['outer'].value == '1'
	}
}

fn test_scalar_unknown_shadow_restores_outer_binding() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('input', flat.source_type_int)
	t.comptime_scalar_locals['outer'] = ComptimeStringScalar{flat.source_type_int, '1'}
	decl := t.make_decl_assign_typed('outer', t.make_ident('input'), flat.source_type_int)
	a.nodes[int(decl)].pos = token.new_span(1, 1, 10)
	_ = t.transform_stmts([decl])
	assert t.comptime_scalar_locals.len == 1
	assert t.comptime_scalar_locals['outer'].value == '1'
}

fn test_scalar_function_state_isolates_and_restores_bindings() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.comptime_scalar_locals['outer'] = ComptimeStringScalar{flat.source_type_string, 'kept'}
	state := t.detach_function_local_state()
	assert t.comptime_scalar_locals.len == 0
	decl := t.make_decl_assign_typed('outer', t.make_int_literal(2), flat.source_type_int)
	a.nodes[int(decl)].pos = token.new_span(1, 1, 10)
	_ = t.transform_comptime_scalar_decl(decl, a.nodes[int(decl)])
	assert t.comptime_scalar_locals['outer'].value == '2'
	t.restore_function_local_state(state)
	assert t.comptime_scalar_locals['outer'].value == 'kept'
}

fn test_prefix_type_inference_preserves_boolean_and_numeric_results() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('flag', flat.source_type_bool)
	t.set_var_type('number', flat.source_type_i64)
	assert t.resolve_expr_type(t.make_prefix(.not, t.make_ident('flag'))) == flat.source_type_bool
	assert t.resolve_expr_type(t.make_prefix(.minus, t.make_ident('number'))) == flat.source_type_i64
	assert t.resolve_expr_type(t.make_prefix(.bit_not, t.make_ident('number'))) == flat.source_type_i64
}
