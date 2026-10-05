module transform

import v.flat
import v.types

fn test_conditional_borrows_outer_storage_and_preserves_pointer_results() {
	for typ in ['int', 'Item', 'Value', '&Value'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.sum_types['Value'] = ['int', 'string']
		tc.sum_types['Value'] = ['int', 'string']
		t.fn_ret_types['first'] = typ
		t.fn_ret_types['second'] = typ
		first := t.make_call_typed('first', [], typ)
		second := t.make_call_typed('second', [], typ)
		then_branch := t.make_block([t.make_expr_stmt(first)])
		else_branch := t.make_block([t.make_expr_stmt(second)])
		condition := t.make_bool_literal(true)
		id := t.make_if(condition, then_branch, else_branch)
		target := if typ.starts_with('&') { typ } else { '&${typ}' }
		lowered := t.try_expand_if_expr_value_for_type(id, a.nodes[int(id)], target)?
		assert t.pending_stmts.len == 2
		declaration := a.node(t.pending_stmts[0])
		storage := a.child_node(declaration, 0)
		assert t.node_type(a.child(declaration, 0)) == typ
		result := a.node(lowered)
		if typ.starts_with('&') {
			assert result.kind == .ident
			assert result.value == storage.value
			continue
		}
		assert result.kind == .prefix
		assert result.op == .amp
		assert a.child_node(result, 0).value == storage.value
	}
}

fn test_value_type_removes_only_implicit_storage_indirection() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a: &a
	}
	storage_types := ['&int', '&&int', '&map[string]int']
	expected_types := ['int', '&int', 'map[string]int']
	for i, storage_type in storage_types {
		name := 'value${i}'
		t.set_var_type(name, storage_type)
		t.mut_param_values[name] = true
		id := t.make_ident(name)
		assert t.expr_value_type(id) == expected_types[i]
		block := t.make_block([t.make_expr_stmt(id)])
		assert t.stmt_value_type(block) == expected_types[i]
	}
	t.set_var_type('reference', '&int')
	reference := t.make_ident('reference')
	assert t.expr_value_type(reference) == '&int'
}

fn test_generic_inference_prefers_active_variant_to_storage_annotation() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a: &a
	}
	t.set_var_type('value', '&Value')
	id := t.make_ident('value')
	t.mut_value_ident_nodes[int(id)] = true
	t.smartcast_stack << SmartcastContext{
		expr_name:     'value'
		variant_name:  'map[string]int'
		sum_type_name: 'Value'
	}
	assert t.generic_call_arg_type_for_inference(id) == 'map[string]int'
}

fn test_generic_sum_storage_keeps_its_specialization() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['model.Value'] = ['int', 'model.Item[T]']
	tc.sum_generic_params['model.Value'] = ['T']
	sum_name := 'model.Value[payload.Token]'
	variants := ['int', 'model.Item[payload.Token]']
	t.set_var_type('value', sum_name)
	id := t.make_ident('value')
	value := t.sum_storage_value(id, sum_name, variants) or {
		panic('generic sum storage was rejected')
	}
	assert a.nodes[int(value)].kind == .ident
	assert t.node_type(value) == sum_name
}

fn test_generic_variant_short_names_preserve_type_structure() {
	variants := [
		'model.Value[payload.Token]',
		'model.Item[payload.Token]',
		'[]model.Item[payload.Token]',
		'map[string]&model.Item[payload.Token]',
		'model.Pair[left.Token, right.Value[other.Item]]',
	]
	expected := [
		'Value[Token]',
		'Item[Token]',
		'[]Item[Token]',
		'map[string]&Item[Token]',
		'Pair[Token, Value[Item]]',
	]
	for i, variant in variants {
		assert variant_short_name_text(variant) == expected[i]
	}
}
