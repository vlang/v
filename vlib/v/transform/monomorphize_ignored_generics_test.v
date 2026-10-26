module transform

import v.flat
import v.types

fn ignored_generics_test_decl(mut a flat.FlatAst, name string, generic bool) (flat.NodeId, flat.NodeId) {
	value := a.add_val(.int_literal, '1')
	body_start := a.children.len
	a.children << value
	body := a.add_node(flat.Node{
		kind:           .block
		children_start: body_start
		children_count: 1
	})
	param := a.add_node(flat.Node{
		kind:  .param
		value: 'value'
		typ:   if generic { flat.source_type_id('T') } else { flat.source_type_int }
	})
	children_start := a.children.len
	a.children << param
	a.children << body
	mut node := flat.Node{
		kind:           .fn_decl
		value:          name
		typ:            flat.source_type_void
		children_start: children_start
		children_count: 2
	}
	if generic {
		node.set_generic_params(['T'])
	}
	return a.add_node(node), body
}

fn test_generic_templates_stay_ignored_when_existing_functions_become_used() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	generic_fn, generic_body := ignored_generics_test_decl(mut a, 'template', true)
	concrete_fn, concrete_body := ignored_generics_test_decl(mut a, 'concrete', false)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	t.used_fns_log_active = true
	decls := t.collect_generic_fn_decls()
	assert decls.len == 1

	initial := t.monomorphize_ignored_nodes(decls)
	assert t.ignored_state.ready
	for id in [generic_fn, generic_body, concrete_fn, concrete_body] {
		assert initial[int(id)]
	}
	t.mark_fn_used('template')
	t.mark_fn_used('concrete')
	updated := t.monomorphize_ignored_nodes(decls)
	assert updated[int(generic_fn)]
	assert updated[int(generic_body)]
	assert !updated[int(concrete_fn)]
	assert !updated[int(concrete_body)]
	assert initial[int(concrete_body)]
}

fn test_appended_generic_templates_stay_ignored_when_functions_become_used() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	t.used_fns_log_active = true
	initial_decls := t.collect_generic_fn_decls()
	t.monomorphize_ignored_nodes(initial_decls)
	assert t.ignored_state.ready

	generic_fn, generic_body := ignored_generics_test_decl(mut a, 'appended_template', true)
	concrete_fn, concrete_body := ignored_generics_test_decl(mut a, 'appended_concrete', false)
	decls := t.collect_generic_fn_decls()
	assert decls.len == 1
	appended := t.monomorphize_ignored_nodes(decls)
	for id in [generic_fn, generic_body, concrete_fn, concrete_body] {
		assert appended[int(id)]
	}
	t.mark_fn_used('appended_template')
	t.mark_fn_used('appended_concrete')
	updated := t.monomorphize_ignored_nodes(decls)
	assert updated[int(generic_fn)]
	assert updated[int(generic_body)]
	assert !updated[int(concrete_fn)]
	assert !updated[int(concrete_body)]
}
