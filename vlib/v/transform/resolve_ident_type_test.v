module transform

import v.flat
import v.types

fn test_identifier_type_resolution_preserves_lookup_precedence() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.const_types['value'] = types.Type(types.string_)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.globals.set_id('value', flat.source_type_u64)
	id := t.make_ident('value')
	// The declaration owns the name even when a global and node annotation exist.
	t.set_node_typ(int(id), flat.source_type_f64)
	assert t.resolve_expr_type(id) == flat.source_type_string
	t.set_var_type('value', flat.source_type_int)
	assert t.resolve_expr_type(id) == flat.source_type_int
	assert t.raw_expr_type_without_smartcast(id) == flat.source_type_int
	t.push_smartcast('value', flat.source_type_bool, flat.source_type_id(option_unwrap_marker))
	assert t.resolve_expr_type(id) == flat.source_type_bool
	assert t.raw_expr_type_without_smartcast(id) == flat.source_type_int
	t.pop_smartcast()
	assert t.resolve_expr_type(id) == flat.source_type_int
	t.unset_var_type('value')
	assert t.resolve_expr_type(id) == flat.source_type_string
}

fn test_identifier_type_resolution_uses_the_active_module_and_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.const_types['first.constant'] = types.Type(types.string_)
	tc.const_types['second.constant'] = types.Type(types.bool_)
	tc.type_aliases.set_id(flat.source_type_id('Counter'), flat.source_type_i64)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.globals.set_id('first.global', flat.source_type_id('Counter'))
	t.globals.set_id('second.global', flat.source_type_u64)
	constant := t.make_ident('constant')
	global := t.make_ident('global')
	for module_name, expected in {
		'first':  flat.source_type_string
		'second': flat.source_type_bool
	} {
		t.cur_module = module_name
		assert t.resolve_expr_type(constant) == expected
	}
	t.cur_module = 'first'
	assert t.resolve_expr_type(global) == flat.source_type_i64
	t.cur_module = 'second'
	assert t.resolve_expr_type(global) == flat.source_type_u64
	assert t.resolve_expr_type(t.make_ident('first.global')) == flat.source_type_i64
	t.cur_module = 'third'
	assert t.resolve_expr_type(global) == 0
}

fn test_identifier_resolution_reads_the_current_ast_name_after_growth() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	name := 'local_${123}'
	t.set_var_type(name, flat.source_type_int)
	t.set_var_type('replacement', flat.source_type_string)
	id := t.make_ident(name.clone())
	assert t.resolve_expr_type(id) == flat.source_type_int
	for i in 0 .. 4096 {
		_ = t.make_ident('padding_${i}')
	}
	assert t.resolve_expr_type(id) == flat.source_type_int
	t.set_node_value(int(id), 'replacement')
	assert t.resolve_expr_type(id) == flat.source_type_string
	assert t.raw_expr_type_without_smartcast(id) == flat.source_type_string
}

fn test_identifier_dispatch_preserves_unknown_and_nonidentifier_results() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	missing := t.make_ident('missing')
	t.set_node_typ(int(missing), flat.source_type_bool)
	assert t.resolve_expr_type(flat.empty_node) == 0
	assert t.resolve_expr_type(missing) == 0
	assert t.raw_expr_type_without_smartcast(missing) == flat.source_type_bool
	t.set_var_type('number', flat.source_type_i64)
	paren := t.make_paren(t.make_ident('number'))
	assert t.resolve_expr_type(paren) == flat.source_type_i64
	assert t.original_expr_type(paren) == flat.source_type_i64
	assert t.resolve_expr_type(t.make_prefix(.minus, paren)) == flat.source_type_i64
}
