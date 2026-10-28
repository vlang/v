module transform

import v.flat
import v.types

fn test_ordinary_alias_factory_is_refreshed_by_monomorphize_driver() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'main.v')
	a.add_val(.module_decl, 'main')
	mut template := flat.Node{
		kind:  .fn_decl
		value: 'unused'
		typ:   flat.source_type_void
	}
	template.set_generic_params(['T'])
	a.add_node(template)
	callee := a.add_val(.ident, 'make_splitter')
	call_start := a.children.len
	a.children << callee
	call := a.add_node(flat.Node{
		kind:           .call
		typ:            flat.source_type_id('fn(I) (O, R)[string, string, string]')
		children_start: call_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('FnMultiReturn'), flat.source_type_id('fn (I) (O, R)'))
	tc.type_alias_generic_params[flat.source_type_id('FnMultiReturn')] = ['I', 'O', 'R']
	tc.fn_ret_types['make_splitter'] = types.Type(types.Alias{
		name_id:   flat.source_type_id('FnMultiReturn[string, string, string]')
		base_type: &types.Type(types.void_)
	})
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.monomorphize_pass()
	concrete_type := flat.source_type_id('fn(string) (string, string)')
	assert a.nodes[int(call)].typ == concrete_type

	// A resumed pass must also refresh newly appended parents of an existing call.
	lhs := a.add_val(.ident, 'splitter')
	decl_start := a.children.len
	a.children << lhs
	a.children << call
	decl := a.add_node(flat.Node{
		kind:           .decl_assign
		children_start: decl_start
		children_count: 2
	})
	t.monomorphize_pass()
	assert a.nodes[int(decl)].typ == concrete_type
	assert a.nodes[int(lhs)].typ == concrete_type
	assert t.var_type('splitter') == concrete_type
}

fn test_generic_refresh_revisits_existing_result_parents_after_late_rewrite() {
	for prefix in ['!', '?'] {
		mut a := flat.FlatAst.new()
		lhs := a.add_node(flat.Node{
			kind:  .ident
			value: 'value'
			typ:   flat.source_type_id('int')
		})
		callee := a.add_val(.ident, 'specialized')
		call_start := a.children.len
		a.children << callee
		call := a.add_node(flat.Node{
			kind:           .call
			typ:            flat.source_type_id('${prefix}int')
			children_start: call_start
			children_count: 1
		})
		or_start := a.children.len
		a.children << call
		or_expr := a.add_node(flat.Node{
			kind:           .or_expr
			typ:            flat.source_type_id('int')
			children_start: or_start
			children_count: 1
		})
		decl_start := a.children.len
		a.children << lhs
		a.children << or_expr
		decl := a.add_node(flat.Node{
			kind:           .decl_assign
			typ:            flat.source_type_id('int')
			children_start: decl_start
			children_count: 2
		})
		mut tc := types.TypeChecker.new(&a)
		tc.specialized_generic_fns['specialized'] = true
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		mut parents := map[int][]int{}
		mut alias_calls := []int{}
		t.collect_generic_refresh_call_parents(0, a.nodes.len, mut parents, mut alias_calls)
		assert parents[int(call)] == [int(or_expr), int(decl)]
		nodes := generic_refresh_nodes_for_calls([int(call)], parents)
		assert nodes == [int(call), int(or_expr), int(decl)]
		changed, changed_decls := t.refresh_decl_assign_types_after_generic_rewrite(nodes)
		assert !changed
		assert !changed_decls
		node_count := a.nodes.len
		// A later specialization can rewrite a call without appending its parents.
		t.set_node_typ(int(call), flat.source_type_id('${prefix}string'))
		changed_after, changed_decls_after := t.refresh_decl_assign_types_after_generic_rewrite(nodes)
		assert changed_after
		assert changed_decls_after
		assert a.nodes.len == node_count
		assert a.nodes[int(or_expr)].typ == flat.source_type_id('string')
		assert a.nodes[int(decl)].typ == flat.source_type_id('string')
		assert a.nodes[int(lhs)].typ == flat.source_type_id('string')
		assert t.var_type('value') == flat.source_type_id('string')
	}
}

fn test_refresh_collection_keeps_every_duplicate_generic_call_site() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'main.v')
	a.add_val(.module_decl, 'main')
	callee := a.add_val(.ident, 'generic')
	arg := a.add_node(flat.Node{
		kind:  .ident
		value: 'value'
		typ:   flat.source_type_string
	})
	mut call_node := flat.Node{
		kind:           .call
		typ:            flat.source_type_string
		children_start: a.children.len
		children_count: 2
	}
	call_node.set_call_type_args([flat.source_type_string])
	a.children << callee
	a.children << arg
	first_call := a.add_node(call_node)
	second_call := a.add_node(call_node)

	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	spec := GenericCallSpec{
		decl_key: 'main.generic'
		args:     flat.TypeTextList{ ids: [flat.source_type_string] }
	}
	t.generic_call_spec_cache[int(first_call)] = spec
	t.generic_call_spec_cache[int(second_call)] = spec
	mut decls := map[string]GenericFnDecl{}
	decls['main.generic'] = GenericFnDecl{
		node:   flat.Node{
			kind:  .fn_decl
			value: 'generic'
		}
		module: 'main'
		key:    'main.generic'
	}
	mut emitted := GenericFnSpecBoolMap{}
	emitted.set(generic_fn_spec_key('main.generic', [flat.source_type_string]), true)
	mut recorded_call_sites := map[int]bool{}
	sites := t.collect_generic_call_sites_after_type_refresh(map[u32]u32{}, decls, []bool{},
		[int(first_call), int(second_call)], mut emitted, mut recorded_call_sites)
	assert sites.len == 2
	assert int(sites[0].id) == int(first_call)
	assert int(sites[1].id) == int(second_call)
}
