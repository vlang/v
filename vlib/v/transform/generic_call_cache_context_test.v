module transform

import v.flat
import v.types

fn test_missed_call_retries_after_its_result_type_is_refreshed() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'main.v')
	a.add_val(.module_decl, 'main')
	mut fn_node := flat.Node{ kind: .fn_decl, value: 'factory', typ: flat.source_type_id('T') }
	fn_node.set_generic_params(['T'])
	fn_id := a.add_node(fn_node)
	callee := a.add_val(.ident, 'factory')
	start := a.children.len
	a.children << callee
	call := flat.Node{
		kind:           .call
		children_start: start
		children_count: 1
	}
	id := a.add_node(call)
	decl := GenericFnDecl{ id: fn_id, node: fn_node, module: 'main', file: 'main.v', key: 'factory' }
	decls := {
		'factory': decl
	}
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.cur_file = 'main.v'
	t.ensure_node_module_map()
	t.mark_node_context(id, 'main', 'main.v')
	assert t.node_file_or(int(id), '') == 'main.v'
	assert t.generic_call_specialization(id, call, 'main', decls) == none
	assert t.generic_call_spec_misses[int(id)]
	t.set_node_typ(int(id), flat.source_type_int)
	refreshed := t.a.nodes[int(id)]
	inferred := t.infer_generic_call_args(decl, id, refreshed, 'main') or {
		assert false, 'the refreshed result must make inference succeed'
		return
	}
	assert inferred == [flat.source_type_int]
	mut emitted := GenericFnSpecBoolMap{}
	emitted.set(generic_fn_spec_key('factory', [flat.source_type_int]), true)
	mut recorded := map[int]bool{}
	sites := t.collect_generic_call_sites_after_type_refresh(map[u32]u32{}, decls, []bool{},
		[int(id)], mut emitted, mut recorded)
	assert t.node_file_or(int(id), '') == 'main.v'
	assert sites.len == 1, 'a result-type refresh must retry the missed call'
}

fn test_cloned_inference_cache_preserves_expected_context() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'main.v')
	a.add_val(.module_decl, 'main')
	mut factory := flat.Node{ kind: .fn_decl, value: 'factory', typ: flat.source_type_id('T') }
	factory.set_generic_params(['T'])
	fn_id := a.add_node(factory)
	callee := a.add_val(.ident, 'factory')
	start := a.children.len
	a.children << callee
	call := flat.Node{ kind: .call, children_start: start, children_count: 1 }
	first := a.add_node(call)
	second := a.add_node(call)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.cur_file = 'main.v'
	t.ensure_node_context_map_capacity()
	t.mark_node_context(first, 'main', 'main.v')
	t.mark_node_context(second, 'main', 'main.v')
	decl := GenericFnDecl{ id: fn_id, node: factory, module: 'main', file: 'main.v', key: 'factory' }
	decls := {
		'factory': decl
	}
	t.expected_expr_node = int(first)
	t.expected_expr_type = flat.source_type_id('int')
	_, first_args := t.cached_generic_call_specialization(first, call, 'main', decls) or { panic('no int inference') }
	assert first_args == [flat.source_type_id('int')]
	t.expected_expr_node = int(second)
	t.expected_expr_type = flat.source_type_string
	_, uncached_args := t.generic_call_specialization(second, call, 'main', decls) or { panic('no string inference') }
	assert uncached_args == [flat.source_type_string]
	_, cached_args := t.cached_generic_call_specialization(second, call, 'main', decls) or { panic('no cached string inference') }
	assert cached_args == uncached_args
}

fn test_missed_call_retries_when_expected_return_context_becomes_known() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'main.v')
	a.add_val(.module_decl, 'main')
	mut factory := flat.Node{ kind: .fn_decl, value: 'factory', typ: flat.source_type_id('T') }
	factory.set_generic_params(['T'])
	fn_id := a.add_node(factory)
	callee := a.add_val(.ident, 'factory')
	start := a.children.len
	a.children << callee
	call := flat.Node{ kind: .call, children_start: start, children_count: 1 }
	id := a.add_node(call)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.cur_file = 'main.v'
	t.ensure_node_context_map_capacity()
	t.mark_node_context(id, 'main', 'main.v')
	decl := GenericFnDecl{ id: fn_id, node: factory, module: 'main', file: 'main.v', key: 'factory' }
	decls := {
		'factory': decl
	}
	assert t.cached_generic_call_specialization(id, call, 'main', decls) == none
	assert t.generic_call_spec_misses[int(id)]
	t.expected_expr_node = int(id)
	t.expected_expr_type = flat.source_type_int
	_, fresh_args := t.generic_call_specialization(id, call, 'main', decls) or {
		panic('expected return context must make inference succeed')
	}
	assert fresh_args == [flat.source_type_int]
	_, retried_args := t.cached_generic_call_specialization(id, call, 'main', decls) or {
		panic('the recorded miss must be retried when context changes')
	}
	assert retried_args == fresh_args
}

fn test_cloned_inference_preserves_struct_literal_fields() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'main.v')
	a.add_val(.module_decl, 'main')
	param := a.add_node(flat.Node{ kind: .param, value: 'p', typ: flat.source_type_id('Box[T]') })
	fn_start := a.children.len
	a.children << param
	mut factory := flat.Node{ kind: .fn_decl, value: 'take', children_start: fn_start, children_count: 1 }
	factory.set_generic_params(['T'])
	fn_id := a.add_node(factory)
	callee := a.add_val(.ident, 'take')
	mut calls := []flat.NodeId{}
	for typ in [flat.source_type_int, flat.source_type_string] {
		value := a.add_node(flat.Node{ kind: .ident, value: 'item', typ: typ })
		field_start := a.children.len
		a.children << value
		field := a.add_node(flat.Node{ kind: .field_init, value: 'item', children_start: field_start, children_count: 1 })
		arg_start := a.children.len
		a.children << field
		arg := a.add_node(flat.Node{ kind: .struct_init, value: 'Box', children_start: arg_start, children_count: 1 })
		call_start := a.children.len
		a.children << callee
		a.children << arg
		calls << a.add_node(flat.Node{ kind: .call, children_start: call_start, children_count: 2 })
	}
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('Box')] = [types.StructField{ name: 'item', typ: types.Type(types.Unknown{ placeholder: flat.source_type_id('T'), generic: true }) }]
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.cur_file = 'main.v'
	t.structs[flat.source_type_id('Box')] = StructInfo{ fields: [FieldInfo{ name: 'item', typ: flat.source_type_id('T') }] }
	t.ensure_node_context_map_capacity()
	for id in calls {
		t.mark_node_context(id, 'main', 'main.v')
	}
	decl := GenericFnDecl{ id: fn_id, node: factory, module: 'main', file: 'main.v', key: 'take' }
	decls := {
		'take': decl
	}
	_, first_args := t.cached_generic_call_specialization(calls[0], a.nodes[int(calls[0])], 'main', decls) or { panic('no int inference') }
	assert first_args == [flat.source_type_int]
	_, uncached_args := t.generic_call_specialization(calls[1], a.nodes[int(calls[1])], 'main', decls) or { panic('no string inference') }
	assert uncached_args == [flat.source_type_string]
	_, cached_args := t.cached_generic_call_specialization(calls[1], a.nodes[int(calls[1])], 'main', decls) or { panic('no cached string inference') }
	assert cached_args == uncached_args
}

fn test_cloned_inference_preserves_live_scope_binding() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'main.v')
	a.add_val(.module_decl, 'main')
	param := a.add_node(flat.Node{ kind: .param, value: 'p', typ: flat.source_type_id('T') })
	fn_start := a.children.len
	a.children << param
	mut factory := flat.Node{ kind: .fn_decl, value: 'take', children_start: fn_start, children_count: 1 }
	factory.set_generic_params(['T'])
	fn_id := a.add_node(factory)
	callee := a.add_val(.ident, 'take')
	arg := a.add_node(flat.Node{ kind: .ident, value: 'value' })
	call_start := a.children.len
	a.children << callee
	a.children << arg
	call := flat.Node{ kind: .call, children_start: call_start, children_count: 2 }
	first := a.add_node(call)
	second := a.add_node(call)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.cur_file = 'main.v'
	t.cloning_generic_fn_depth = 1
	t.ensure_node_context_map_capacity()
	t.mark_node_context(first, 'main', 'main.v')
	t.mark_node_context(second, 'main', 'main.v')
	decl := GenericFnDecl{ id: fn_id, node: factory, module: 'main', file: 'main.v', key: 'take' }
	decls := {
		'take': decl
	}
	t.set_var_type('value', flat.source_type_int)
	_, first_args := t.cached_generic_call_specialization(first, call, 'main', decls) or { panic('no scoped int inference') }
	assert first_args == [flat.source_type_int]
	t.set_var_type('value', flat.source_type_string)
	_, uncached_args := t.generic_call_specialization(second, call, 'main', decls) or { panic('no scoped string inference') }
	assert uncached_args == [flat.source_type_string]
	_, cached_args := t.cached_generic_call_specialization(second, call, 'main', decls) or { panic('no cached scoped string inference') }
	assert cached_args == uncached_args
}
