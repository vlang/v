module transform

import v.flat
import v.types

fn test_var_type_scope_restores_like_a_copy() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('outer', flat.source_type_int)
	t.set_var_type('shadowed', flat.source_type_string)
	expected := t.var_types.clone()
	t.var_type_undo_active = true
	scope := t.mark_var_type_scope()
	t.set_var_type('inner', flat.source_type_u8)
	t.set_var_type('shadowed', flat.source_type_f64)
	nested := t.mark_var_type_scope()
	t.set_var_type('inner', flat.source_type_id('[]int'))
	t.set_var_type('deep', flat.source_type_bool)
	t.set_var_type('outer', flat.source_type_i64)
	t.set_var_type('outer', flat.source_type_u64)
	t.restore_var_type_scope(nested)
	assert t.var_type('inner') == flat.source_type_u8
	assert t.var_type('outer') == flat.source_type_int
	assert t.var_type('shadowed') == flat.source_type_f64
	assert t.var_type('deep') == u32(0)
	t.restore_var_type_scope(scope)
	t.var_type_undo_active = false
	assert t.var_types == expected
	assert t.var_type_indices.len == expected.len
	assert t.var_type_index('inner') == -1
	assert t.var_type('shadowed') == flat.source_type_string
	assert t.var_type_undo_log.len == 0
	// Outside such a walk a rebinding leaves no record behind.
	t.set_var_type('outer', flat.source_type_i8)
	assert t.var_type_undo_log.len == 0
}

fn test_late_scan_gate_rejects_only_names_no_published_name_ends_with() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Speaker'] = true
	tc.fn_ret_types['Speaker.speak'] = types.Type(types.void_)
	tc.fn_ret_types['os.getenv'] = types.Type(types.string_)
	tc.fn_param_types['Dog.bark'] = []types.Type{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.used_struct_operator_fns['Vec.+'] = true
	candidates := {
		'main.late_helper':  true
		'main__late_helper': true
		'Cat.purr':          true
	}
	// Without a gate every name is probed.
	assert !t.late_scan_gate_rejects('unrelated', &candidates)
	gate := t.new_late_scan_gate(&candidates)
	t.late_scan_gate = gate
	for kept in ['late_helper', 'main.late_helper', 'main__late_helper', 'purr', 'x.purr', '+',
		'speak'] {
		assert !t.late_scan_gate_rejects(kept, &candidates), kept
	}
	for dropped in ['getenv', 'bark', 'field', 'late', 'helper', 'main'] {
		assert t.late_scan_gate_rejects(dropped, &candidates), dropped
	}
	// Another filter, a new signature or a new interface each retire the gate.
	other := {
		'main.late_helper': true
	}
	assert !t.late_scan_gate_rejects('field', &other)
	tc.fn_param_types['Speaker.sing'] = []types.Type{}
	assert !t.late_scan_gate_rejects('field', &candidates)
	t.late_scan_gate = t.new_late_scan_gate(&candidates)
	assert t.late_scan_gate_rejects('field', &candidates)
	assert !t.late_scan_gate_rejects('sing', &candidates)
	tc.interface_names['Singer'] = true
	assert !t.late_scan_gate_rejects('field', &candidates)
}

fn test_generic_call_name_filter_keeps_every_spelling_of_a_declaration() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.skip_generics = false
	static_key := flat.encode_static_type_method_name('Box', 'make')
	t.generic_fn_decls_cache = {
		'main.pick':  GenericFnDecl{
			module: 'main'
			key:    'main.pick'
		}
		'Box.put':    GenericFnDecl{
			key: 'Box.put'
		}
		'json.@type': GenericFnDecl{
			module: 'json'
			key:    'json.@type'
		}
		static_key:   GenericFnDecl{
			key: static_key
		}
	}
	t.build_generic_receiver_method_index()
	decls := t.generic_fn_decls_cache
	for name in ['pick', 'main.pick', 'other.pick', 'put', 'Box[int].put', 'type', 'json.type'] {
		assert !t.generic_name_cannot_spell_decl(name), name
	}
	for name in ['plain', 'main.plain', 'Box.get', 'picker', 'main.pick.more'] {
		assert t.generic_name_cannot_spell_decl(name), name
	}
	// Escaped, flattened and empty names take the full lookup.
	for name in ['@plain', 'main.@plain', 'Box_int__get', static_key, ''] {
		assert !t.generic_name_cannot_spell_decl(name), name
	}
	ident := flat.Node{
		kind:  .ident
		value: 'plain'
	}
	selector := flat.Node{
		kind:  .selector
		value: 'get'
	}
	assert t.generic_call_cannot_name_decl('main.plain', true, ident, decls)
	assert t.generic_call_cannot_name_decl('', false, ident, decls)
	assert t.generic_call_cannot_name_decl('Box.get', true, selector, decls)
	assert !t.generic_call_cannot_name_decl('main.pick', true, ident, decls)
	assert !t.generic_call_cannot_name_decl('main.plain', true, flat.Node{
		kind:  .ident
		value: 'pick'
	}, decls)
	assert !t.generic_call_cannot_name_decl('', false, flat.Node{
		kind:  .ident
		value: 'plain_T_int'
	}, decls)
	assert !t.generic_call_cannot_name_decl('Box.make', true, flat.Node{
		kind:  .selector
		value: 'make'
	}, decls)
	assert !t.generic_call_cannot_name_decl('', true, ident, decls)
	// Another declaration table is never filtered through this one's names.
	mut larger := decls.clone()
	larger['main.plain'] = GenericFnDecl{
		key: 'main.plain'
	}
	assert !t.generic_call_cannot_name_decl('main.plain', true, ident, larger)
}

fn test_alias_receiver_candidates_follow_the_signature_tables() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Meters', flat.source_type_int)
	tc.type_aliases.set_id('unit.Feet', flat.source_type_int)
	receiver := [types.Type(types.int_)]
	tc.fn_param_types['Meters.double'] = receiver
	tc.fn_param_types['os.double'] = receiver
	tc.fn_param_types['Meters.pending'] = []types.Type{}
	tc.fn_param_types['unit.Feet.double'] = receiver
	tc.fn_param_types['double'] = receiver
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.skip_generics = false
	// A transformer that never collected its aliases scans the signatures.
	assert t.alias_receiver_method_candidates('double') == ['Meters.double', 'unit.Feet.double']
	t.collect_alias_methods()
	assert t.alias_receiver_fns_signatures == tc.fn_param_types.len
	assert t.alias_receiver_method_candidates('double') == ['Meters.double', 'unit.Feet.double']
	assert t.alias_receiver_method_candidates('pending') == []string{}
	assert t.alias_receiver_method_candidates('Feet.double') == ['unit.Feet.double']
	assert t.alias_receiver_method_candidates('missing') == []string{}
	// Parameters registered for a listed signature are seen without a new list.
	tc.fn_param_types['Meters.pending'] = receiver
	assert t.alias_receiver_method_candidates('pending') == ['Meters.pending']
	// A new signature or alias is found by the scan until the list is taken again.
	tc.type_aliases.set_id('Yards', flat.source_type_int)
	tc.fn_param_types['Yards.double'] = receiver
	expected := ['Meters.double', 'unit.Feet.double', 'Yards.double']
	assert t.alias_receiver_method_candidates('double') == expected
	t.refresh_alias_receiver_fns()
	assert t.alias_receiver_fns_signatures == tc.fn_param_types.len
	assert t.alias_receiver_method_candidates('double') == expected
}

fn test_append_element_interface_answers_follow_the_type_tables() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Shape'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.append_elem_interface_cache = &ElemInterfaceCache{}
	for _ in 0 .. 2 {
		assert t.array_append_elem_is_interface(flat.source_type_id('Shape'))
		assert !t.array_append_elem_is_interface(flat.source_type_int)
		assert !t.array_append_elem_is_interface(flat.source_type_id('Later'))
	}
	tc.interface_names['Later'] = true
	assert t.array_append_elem_is_interface(flat.source_type_id('Later'))
	tc.interface_names['geometry.Solid'] = true
	assert !t.array_append_elem_is_interface(flat.source_type_id('Solid'))
	t.cur_module = 'geometry'
	assert t.array_append_elem_is_interface(flat.source_type_id('Solid')) == t.array_append_elem_is_interface_uncached('Solid')
}

fn test_alias_chain_stops_at_plain_types_and_cycles() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Names', flat.source_type_id('Words'))
	tc.type_aliases.set_id('Words', flat.source_type_id('[]string'))
	tc.type_aliases.set_id('Ping', flat.source_type_id('Pong'))
	tc.type_aliases.set_id('Pong', flat.source_type_id('Ping'))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	assert t.normalize_type_alias_chain('int') == 'int'
	assert t.normalize_type_alias_chain('[]string') == '[]string'
	assert t.normalize_type_alias_chain('Names') == '[]string'
	assert t.normalize_type_alias_chain('Ping') in ['Ping', 'Pong']
}

fn shortcut_test_node(mut a flat.FlatAst, node flat.Node, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		...node
		children_start: start
		children_count: children.len
	})
}

// shortcut_capturing_literal is a function literal that captures a variable.
fn shortcut_capturing_literal(mut a flat.FlatAst, mut t Transformer) flat.NodeId {
	return shortcut_test_node(mut a, flat.Node{ kind: .fn_literal }, [
		t.make_ident('captured'),
	])
}

fn test_trimmed_transform_text_is_trim_space() {
	for text in ['', ' ', '\t\n', 'int', ' int', 'int ', '\tmap[string]int\n', 'a b', ' a b ',
		'\vx\f', 'é', ' é\r'] {
		assert trimmed_transform_text(text) == text.trim_space(), text.bytes().str()
	}
}

fn test_interface_box_scan_reads_the_kinds_it_handles() {
	for kind in [flat.NodeKind.file, .module_decl, .fn_decl, .fn_literal, .lambda_expr, .assign,
		.decl_assign, .selector_assign, .index_assign, .select_branch, .infix, .cast_expr, .call,
		.struct_init] {
		assert interface_box_scan_reads_kind(kind), kind.str()
	}
	for kind in [flat.NodeKind.ident, .string_literal, .selector, .block, .return_stmt, .if_expr,
		.param, .field_init, .array_literal, .prefix] {
		assert !interface_box_scan_reads_kind(kind), kind.str()
	}
}

fn test_closure_bounds_hold_for_every_value_the_cleanup_walk_accepts() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	plain := shortcut_test_node(mut a, flat.Node{ kind: .fn_literal }, [])
	text := t.make_string_literal('text')
	name := t.make_ident('name')
	field := t.make_selector(t.make_ident('base'), 'field', flat.source_type_int)
	cond := a.add_val(.bool_literal, 'true')
	in_paren := t.make_paren(shortcut_capturing_literal(mut a, mut t))
	in_block := shortcut_test_node(mut a, flat.Node{ kind: .block }, [
		t.make_string_literal('first'),
		shortcut_capturing_literal(mut a, mut t),
	])
	block_of_text := shortcut_test_node(mut a, flat.Node{ kind: .block }, [
		shortcut_capturing_literal(mut a, mut t),
		t.make_string_literal('last'),
	])
	both_branches := shortcut_test_node(mut a, flat.Node{ kind: .if_expr }, [cond,
		shortcut_capturing_literal(mut a, mut t), shortcut_capturing_literal(mut a, mut t)])
	one_branch := shortcut_test_node(mut a, flat.Node{ kind: .if_expr }, [cond,
		shortcut_capturing_literal(mut a, mut t), t.make_string_literal('else')])
	negated := shortcut_test_node(mut a, flat.Node{ kind: .postfix, op: .not }, [
		shortcut_capturing_literal(mut a, mut t),
	])
	elements := shortcut_test_node(mut a, flat.Node{ kind: .array_literal }, [
		t.make_string_literal('element'),
		shortcut_capturing_literal(mut a, mut t),
	])
	texts := shortcut_test_node(mut a, flat.Node{ kind: .array_literal }, [
		t.make_string_literal('one'),
		t.make_string_literal('two'),
	])
	nested := shortcut_test_node(mut a, flat.Node{ kind: .array_literal }, [elements])
	assert t.may_allocate_fresh_runtime_closure(in_paren)
	assert t.may_allocate_fresh_runtime_closure(in_block)
	assert t.may_allocate_fresh_runtime_closure(both_branches)
	assert t.may_allocate_fresh_runtime_closure(one_branch)
	assert t.may_allocate_fresh_runtime_closure(plain)
	assert t.may_allocate_fresh_runtime_closure(field)
	assert !t.may_allocate_fresh_runtime_closure(text)
	assert !t.may_allocate_fresh_runtime_closure(name)
	assert !t.may_allocate_fresh_runtime_closure(block_of_text)
	assert !t.may_allocate_fresh_runtime_closure(elements)
	assert t.appended_value_may_allocate_runtime_closure(elements)
	assert t.appended_value_may_allocate_runtime_closure(negated)
	assert !t.appended_value_may_allocate_runtime_closure(texts)
	assert !t.appended_value_may_allocate_runtime_closure(text)
	assert t.is_closure_field_initializer(elements)
	assert t.is_closure_field_initializer(shortcut_test_node(mut a, flat.Node{
		kind: .postfix
		op:   .not
	}, [nested]))
	assert !t.is_closure_field_initializer(in_paren)
	assert !t.is_closure_field_initializer(text)
	// The walk accepts no value that its bounds turn away.
	mut accepted := 0
	for idx in 0 .. a.nodes.len {
		id := flat.NodeId(idx)
		if t.expr_allocates_fresh_runtime_closure(id) {
			accepted++
			assert t.may_allocate_fresh_runtime_closure(id), a.nodes[idx].kind.str()
		}
		mut appended := []LocalClosureFieldCandidate{}
		t.collect_local_closure_append_value_candidates(id, id, id, 'values', 'values[*]',
			flat.NodeId(-1), mut appended)
		if appended.len > 0 {
			accepted++
			assert t.appended_value_may_allocate_runtime_closure(id), a.nodes[idx].kind.str()
		}
		mut fields := []LocalClosureFieldCandidate{}
		t.collect_local_closure_initializer_field_candidates(flat.NodeId(-1), id, id, 'values',
			'values', flat.NodeId(-1), mut fields)
		if fields.len > 0 {
			accepted++
			assert t.is_closure_field_initializer(id), a.nodes[idx].kind.str()
		}
	}
	assert accepted > 10
}
